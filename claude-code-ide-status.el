;;; claude-code-ide-status.el --- Session status overview for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Keywords: ai, claude, tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides `claude-code-ide-status', a `tabulated-list-mode'
;; overview of all Claude Code sessions across projects and worktrees.  It
;; highlights sessions that need you — an Emacs diff awaiting your review,
;; Claude blocked on your input, or a finished turn — and lists resumable
;; projects from Claude's on-disk session history.
;;
;; The `permission' state is detected automatically.  The `waiting' and
;; `input' states are driven by Claude Code hooks, which distinguish the
;; two kinds of waiting: a Stop hook marks a finished turn (`waiting'),
;; while a Notification hook marks Claude blocked on your input (`input'):
;;
;;   ;; Stop hook — finished a turn:
;;   emacsclient --eval \
;;     "(claude-code-ide-status-mark-waiting \"$CLAUDE_PROJECT_DIR\")"
;;   ;; Notification hook (matcher permission_prompt) — needs your input:
;;   emacsclient --eval \
;;     "(claude-code-ide-status-mark-waiting \"$CLAUDE_PROJECT_DIR\" 'input)"
;;
;; The flag clears itself as soon as you select that session's terminal
;; buffer, so no matching "active" hook is required.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'cursor-sensor)
(require 'hl-line)
(require 'tabulated-list)
(require 'vc-git)
(require 'claude-code-ide)

;; Forward declarations for the public MCP session-query API (defined in
;; claude-code-ide-mcp.el, loaded transitively by claude-code-ide).
(declare-function claude-code-ide-mcp-session-connected-p "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-pending-permissions "claude-code-ide-mcp" (session))

(defvar claude-code-ide-status-buffer-name "*Claude Sessions*"
  "Name of the buffer showing the Claude session status list.")

(defcustom claude-code-ide-status-projects-directory
  (expand-file-name "projects" "~/.claude")
  "Directory where Claude Code stores per-project session history.
Each subdirectory holds the transcript files for one project and is used
to populate the resumable-project rows of `claude-code-ide-status'."
  :type 'directory
  :group 'claude-code-ide)

(defcustom claude-code-ide-status-refresh-interval 1.5
  "Seconds between automatic refreshes of a displayed status buffer.
The timer only fires while the buffer is on screen.  Set to nil to
disable live refresh and rely on manual \\[revert-buffer]."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'claude-code-ide)

(defcustom claude-code-ide-status-resume-cache-ttl 30
  "Seconds to reuse the cached resumable-project list before rebuilding.
Scanning Claude's on-disk history reads a transcript and queries git for
every project, which is too costly to repeat on the live-refresh timer,
so the result is cached for this long.  A manual refresh
\(`claude-code-ide-status-refresh', bound to \\`g') rebuilds it now."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-status-busy-timeout 12
  "Seconds of terminal quiet after which a session stops counting as busy.
A live session whose terminal produced output within this many seconds is
shown as `working'; once its terminal has been quiet longer than this it
falls back to `idle'.  This spans the normal pauses within a turn (thinking
or waiting on a tool) so `working' does not flicker; a finished turn shows
`waiting' instead once a Stop hook marks it (see
`claude-code-ide-status-mark-waiting').  Detection is a poll on the
live-refresh timer, so values below `claude-code-ide-status-refresh-interval'
have no effect."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-status-max-column-width 80
  "Maximum width, in columns, for an auto-sized status column.
Columns are sized to fit their content on each refresh; this caps how wide
a column with very long project paths or branch names may grow."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-status-worktree-directories '(".worktrees" "worktrees")
  "Directory names that hold git worktrees beside or inside a repository.
A session running in a worktree has a path whose leading components repeat
the repository on every row, pushing the part that identifies the session
off the right edge.  When a session directory sits directly inside one of
these, the Project column shows REPOSITORY/WORKTREE instead of the whole
path.  Set to nil to always show the plain directory name."
  :type '(repeat string)
  :group 'claude-code-ide)

(defcustom claude-code-ide-status-attention-interval 3
  "Seconds between updates of the global attention indicator.
See `claude-code-ide-status-attention-mode'."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-status-notify nil
  "When non-nil, notify as a session enters an attention state.
`claude-code-ide-status-notify-function' is called the moment a session
becomes blocked on a permission prompt or marked waiting, provided
`claude-code-ide-status-attention-mode' is on to poll for the change."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-status-notify-function
  #'claude-code-ide-status-notify-echo
  "Function called with (DIR STATE) when a session newly needs attention.
STATE is `permission', `input', or `waiting'.  The default announces in
the echo area and rings the bell, which works everywhere; set it to a
wrapper around `notifications-notify', `alert', or a shell notifier for
real desktop notifications."
  :type 'function
  :group 'claude-code-ide)

;;; State model

;; All three tables below are keyed by session ID rather than by project
;; directory: a project may run any number of instances concurrently, and each
;; one attends, works and waits on its own.

(defvar claude-code-ide-status--attention (make-hash-table :test 'equal)
  "Hash table mapping a session ID to non-nil when that instance is
waiting on the user.  Set via `claude-code-ide-status-mark-waiting' and
cleared via `claude-code-ide-status-mark-active' or by visiting the session.")

(defvar claude-code-ide-status--activity (make-hash-table :test 'equal)
  "Maps a live session's ID to (MODIFIED-TICK . LAST-ACTIVE-TIME).
Distinguishes an actively-working instance from a quiet idle one: the timer
polls each terminal's `buffer-chars-modified-tick', and a change since the
last poll means the instance produced output, stamping LAST-ACTIVE-TIME.
See `claude-code-ide-status--poll-activity' and
`claude-code-ide-status--busy-p'.")

(defun claude-code-ide-status--poll-activity ()
  "Note terminal output for each live session, for `working' detection.
Compares every live instance's terminal buffer modification tick to the
previous poll; a change stamps its last-active time.  An instance seen for
the first time is only recorded as a baseline, so a merely-open terminal is
not mistaken for one that just produced output."
  (let ((now (float-time)))
    (dolist (session (claude-code-ide-sessions))
      (when-let* ((buffer (claude-code-ide-mcp-session-buffer session))
                  (_ (buffer-live-p buffer))
                  (id (claude-code-ide-mcp-session-session-id session)))
        (let ((tick (buffer-chars-modified-tick buffer))
              (entry (gethash id claude-code-ide-status--activity)))
          (cond
           ((null entry)
            (puthash id (cons tick 0.0) claude-code-ide-status--activity))
           ((/= tick (car entry))
            (puthash id (cons tick now) claude-code-ide-status--activity))))))))

(defun claude-code-ide-status--busy-p (session)
  "Return non-nil when SESSION's terminal produced output very recently.
\"Recently\" is within `claude-code-ide-status-busy-timeout' seconds."
  (when-let* ((id (claude-code-ide-mcp-session-session-id session))
              (entry (gethash id claude-code-ide-status--activity)))
    (< (- (float-time) (cdr entry)) claude-code-ide-status-busy-timeout)))

(defun claude-code-ide-status--state-for (session)
  "Return a symbol describing the state of the Claude instance SESSION.
One of `permission', `input', `working', `waiting', `idle', or
`disconnected'.  Precedence, top to bottom: a pending permission wins over
all; an `input' flag (Claude blocked on your input) wins over active
output; output wins over a `waiting' flag (a finished turn, shown once the
final output has flushed); a merely-connected instance is `idle'."
  (let* ((live (claude-code-ide-session-live-p session))
         (flag (gethash (claude-code-ide-mcp-session-session-id session)
                        claude-code-ide-status--attention))
         (pending (claude-code-ide-mcp-session-pending-permissions session)))
    (cond
     ((and pending (> pending 0)) 'permission)
     ((eq flag 'input) 'input)
     ((and live (claude-code-ide-status--busy-p session))
      'working)
     (flag 'waiting)
     ((claude-code-ide-mcp-session-connected-p session)
      'idle)
     (t 'disconnected))))

(defun claude-code-ide-status--redraw ()
  "Revert the status list, then keep the highlight and show an empty note.
`tabulated-list' restores point to the same row by its id after redrawing,
so the selection follows a session across refreshes and reorderings; the
`hl-line' highlight tracks it without relying on a fragile cursor offset.
Assumes the current buffer is the status buffer."
  (revert-buffer)
  ;; The column header is printed as the first buffer line; mark it (and its
  ;; newline) `cursor-intangible' so point cannot rest on it, then nudge point
  ;; onto the first real row.  Re-applied here because each print rewrites it.
  (let ((inhibit-read-only t)
        (row1 (save-excursion (goto-char (point-min))
                              (forward-line 1) (point))))
    (put-text-property (point-min) row1 'cursor-intangible t)
    ;; Text properties are rear-sticky by default, so without this the start
    ;; of row 1 inherits the header's `cursor-intangible' (via
    ;; `get-pos-property').  `cursor-sensor' would then treat the first row as
    ;; intangible and try to move point off it, calling
    ;; `cursor-sensor-tangible-pos', which dereferences the
    ;; `cursor-intangible--last-point' window parameter — nil on a fresh
    ;; window's first redisplay — and signals (wrong-type-argument
    ;; number-or-marker-p nil).  Marking the boundary rear-nonsticky keeps the
    ;; property from bleeding onto row 1 so the row stays tangible.
    (when (> row1 (point-min))
      (put-text-property (1- row1) row1 'rear-nonsticky '(cursor-intangible))))
  (when (null (tabulated-list-get-id))
    (claude-code-ide-status--goto-first-row))
  ;; `tabulated-list' prints nothing when there are no rows; add a note.
  (when (null tabulated-list-entries)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert (propertize
                 "\n  No Claude sessions running, and no resumable projects found.\n"
                 'face 'shadow)))))
  ;; The timer reverts outside the command loop, so move the row highlight
  ;; to the restored point now rather than at the next key.
  (when (bound-and-true-p hl-line-mode)
    (hl-line-highlight)))

(defun claude-code-ide-status--maybe-refresh ()
  "Rebuild and redraw the status list, but only when it is displayed.
Cheap by design: if the status buffer is not on screen, do nothing."
  (when-let* ((buf (get-buffer claude-code-ide-status-buffer-name)))
    (when (get-buffer-window buf t)
      (with-current-buffer buf
        (claude-code-ide-status--redraw)))))

;;; Public API — the integration seam for finish-hooks / notification scripts

(defun claude-code-ide-status--normalize-dir (dir)
  "Return DIR as an absolute directory name, matching the process-table keys.
DIR defaults to the current project directory.  Normalising means an
external caller (such as a Stop hook passing \"$CLAUDE_PROJECT_DIR\"
without a trailing slash) still matches the live session's entry."
  (file-name-as-directory
   (expand-file-name (or dir (claude-code-ide-current-working-directory)))))

(defun claude-code-ide-status--sessions-in (dir)
  "Return the sessions whose project directory is DIR.
DIR is normalized first, so an external caller passing a path without a
trailing slash still matches."
  (let ((target (claude-code-ide-status--normalize-dir dir)))
    (seq-filter (lambda (session)
                  (equal target
                         (claude-code-ide-status--normalize-dir
                          (claude-code-ide-mcp-session-project-dir session))))
                (claude-code-ide-sessions))))

;;;###autoload
(defun claude-code-ide-status-mark-waiting (&optional dir reason)
  "Mark the Claude instances in DIR as needing the user, for REASON.
REASON is `waiting' (the default — a finished turn) or `input' (Claude is
blocked waiting for your input or approval).  DIR defaults to the current
project directory.  Drive `waiting' from a Claude Code Stop hook and
`input' from a Notification hook, e.g.:
  emacsclient --eval \\='(claude-code-ide-status-mark-waiting \"/path\")\\='
  emacsclient --eval \\='(claude-code-ide-status-mark-waiting \"/path\" \\='input)\\='

A project may run several instances, and the hooks report a directory
rather than an instance, so every instance of DIR is flagged.  Each one
clears again as soon as the user visits it."
  (interactive)
  (dolist (session (claude-code-ide-status--sessions-in dir))
    (puthash (claude-code-ide-mcp-session-session-id session)
             (or reason 'waiting) claude-code-ide-status--attention))
  (claude-code-ide-status--maybe-refresh))

;;;###autoload
(defun claude-code-ide-status-mark-active (&optional dir)
  "Clear the waiting flag for every Claude instance in DIR.
DIR defaults to the current project directory."
  (interactive)
  (dolist (session (claude-code-ide-status--sessions-in dir))
    (remhash (claude-code-ide-mcp-session-session-id session)
             claude-code-ide-status--attention))
  (claude-code-ide-status--maybe-refresh))

;;; Auto-clear when the user engages a session

(defun claude-code-ide-status--session-for-buffer (buffer)
  "Return the session whose terminal is BUFFER, or nil."
  (when (buffer-live-p buffer)
    (seq-find (lambda (session)
                (eq (claude-code-ide-mcp-session-buffer session) buffer))
              (claude-code-ide-sessions))))

(defun claude-code-ide-status--session-by-id (id)
  "Return the live session whose session ID is ID, or nil.
Live rows carry the session ID rather than the project directory, since a
project may run several instances."
  (seq-find (lambda (session)
              (equal id (claude-code-ide-mcp-session-session-id session)))
            (claude-code-ide-sessions)))

(defun claude-code-ide-status--session-buffer-at-point (id)
  "Return the live terminal buffer for row key ID, or signal a `user-error'."
  (let* ((session (or (claude-code-ide-status--session-by-id id)
                      (user-error "That instance is no longer running")))
         (buffer (claude-code-ide-mcp-session-buffer session)))
    (unless (buffer-live-p buffer)
      (user-error "The buffer for %s no longer exists"
                  (claude-code-ide-session-name session)))
    buffer))

(defun claude-code-ide-status--clear-on-select (&optional frame-or-window)
  "Clear the waiting flag once the user selects a Claude terminal buffer.
Intended for `window-selection-change-functions', whose argument is the
FRAME-OR-WINDOW whose selection changed; seeing the session counts as
attending to it, so any explicit waiting flag is dropped."
  (let ((buffer (cond
                 ((window-live-p frame-or-window) (window-buffer frame-or-window))
                 ((framep frame-or-window)
                  (window-buffer (frame-selected-window frame-or-window)))
                 (t (current-buffer)))))
    (when-let* ((session (claude-code-ide-status--session-for-buffer buffer))
                (id (claude-code-ide-mcp-session-session-id session)))
      (when (gethash id claude-code-ide-status--attention)
        (remhash id claude-code-ide-status--attention)
        (claude-code-ide-status--maybe-refresh)))))

(add-hook 'window-selection-change-functions
          #'claude-code-ide-status--clear-on-select)

;;; Faces

(defface claude-code-ide-status-permission-face '((t :inherit error))
  "Face for the `permission' state (a diff or tool awaiting your review)."
  :group 'claude-code-ide)

(defface claude-code-ide-status-input-face '((t :inherit error :weight bold))
  "Face for the `input' state (blocked waiting for your input)."
  :group 'claude-code-ide)

(defface claude-code-ide-status-waiting-face '((t :inherit warning :weight bold))
  "Face for the `waiting' state (finished a turn, needs the user)."
  :group 'claude-code-ide)

(defface claude-code-ide-status-working-face '((t :inherit font-lock-function-name-face :weight bold))
  "Face for the `working' state (terminal actively producing output)."
  :group 'claude-code-ide)

(defface claude-code-ide-status-idle-face '((t :inherit success))
  "Face for the `idle' state (connected, quiet)."
  :group 'claude-code-ide)

(defface claude-code-ide-status-disconnected-face '((t :inherit shadow))
  "Face for the `disconnected' state."
  :group 'claude-code-ide)

(defface claude-code-ide-status-resume-face '((t :inherit shadow))
  "Face for resumable-project rows."
  :group 'claude-code-ide)

;;; Row building

(defconst claude-code-ide-status--state-display
  '((permission   "●" claude-code-ide-status-permission-face   "permission")
    (input        "◆" claude-code-ide-status-input-face        "needs you")
    (working      "▶" claude-code-ide-status-working-face      "working")
    (waiting      "●" claude-code-ide-status-waiting-face      "waiting")
    (idle         "○" claude-code-ide-status-idle-face         "idle")
    (disconnected "·" claude-code-ide-status-disconnected-face "disconnected")
    (resume       "·" claude-code-ide-status-resume-face       "resume"))
  "Per-state display data as (STATE GLYPH FACE WORD).
Shared by the State column labels and the mode-line breakdown badge.")

(defconst claude-code-ide-status--columns
  ["State" "Instance" "Project" "Branch" "Last output" "Tokens" "Uptime"]
  "Header labels for the status columns, in order.")

(defun claude-code-ide-status--state-label (state)
  "Return a propertized display string for STATE.
Carries a `sort-rank' text property so the State column sorts by urgency
rather than by the glyph of the label string."
  (let* ((spec (alist-get state claude-code-ide-status--state-display))
         (label (if spec
                    (propertize (format "%s %s" (nth 0 spec) (nth 2 spec))
                                'face (nth 1 spec))
                  (format "%s" state))))
    (propertize label 'sort-rank (claude-code-ide-status--state-rank state))))

(defun claude-code-ide-status--branch (dir)
  "Return the current git branch name for DIR, or nil if unavailable."
  (let ((default-directory (file-name-as-directory dir)))
    (ignore-errors (car (vc-git-branches)))))

(defun claude-code-ide-status--format-duration (seconds)
  "Format SECONDS as a compact human-readable duration."
  (let ((s (floor (max 0 seconds))))
    (cond ((< s 60) (format "%ds" s))
          ((< s 3600) (format "%dm" (/ s 60)))
          ((< s 86400) (format "%dh%02dm" (/ s 3600) (% (/ s 60) 60)))
          (t (format "%dd%02dh" (/ s 86400) (% (/ s 3600) 24))))))

(defun claude-code-ide-status--project-label (dir)
  "Return a compact label identifying the project or worktree at DIR.
When DIR sits directly inside one of
`claude-code-ide-status-worktree-directories', the result is
REPOSITORY/WORKTREE; otherwise it is the directory name.  The full path
stays reachable as a `help-echo', so nothing is lost by shortening it.

Worktree paths repeat their repository prefix on every row, which pushes
the distinguishing part off the right edge exactly when several worktrees
of one repository are running at once."
  (let* ((clean (directory-file-name (expand-file-name dir)))
         (leaf (file-name-nondirectory clean))
         (container (file-name-directory clean))
         (container-name (and container
                              (file-name-nondirectory
                               (directory-file-name container))))
         (label
          (if (and container-name
                   (member container-name
                           claude-code-ide-status-worktree-directories))
              (let* ((repo (file-name-directory (directory-file-name container)))
                     (repo-name (and repo (file-name-nondirectory
                                           (directory-file-name repo)))))
                ;; Claude Code's own `--worktree' puts worktrees in
                ;; <repo>/.claude/worktrees/, so the directory above the
                ;; container is Claude's own state directory rather than the
                ;; repository.  Step over it to name the repository.
                (when (equal repo-name ".claude")
                  (setq repo (and repo (file-name-directory
                                        (directory-file-name repo)))
                        repo-name (and repo (file-name-nondirectory
                                             (directory-file-name repo)))))
                (if (and repo-name (not (string-empty-p repo-name)))
                    (format "%s/%s" repo-name leaf)
                  leaf))
            leaf)))
    (propertize (if (string-empty-p label) (abbreviate-file-name dir) label)
                'help-echo (abbreviate-file-name dir))))

(defun claude-code-ide-status--uptime-string (session)
  "Return how long SESSION's Claude process has run, or \"\" if unknown."
  (or (when-let* ((pid (claude-code-ide-mcp-session-cli-pid session))
                  (attrs (ignore-errors (process-attributes pid)))
                  (etime (alist-get 'etime attrs))
                  (seconds (float-time etime)))
        (propertize (claude-code-ide-status--format-duration seconds)
                    'sort-key seconds))
      ""))

(defun claude-code-ide-status--last-output-string (session)
  "Return how long ago SESSION's terminal last produced output.
\"now\" while it is still producing, otherwise the elapsed time, or a dash
when the instance has not been seen to produce output at all.

Deliberately a duration and nothing else.  The State column already names
what an instance is doing, so repeating `working' or `idle' here would say
the same thing twice; the elapsed time is the part State cannot express."
  (cond
   ;; Producing output right now is zero seconds ago, so it sorts first.
   ((claude-code-ide-status--busy-p session) (propertize "now" 'sort-key 0))
   ((when-let* ((id (claude-code-ide-mcp-session-session-id session))
                (entry (gethash id claude-code-ide-status--activity)))
      (and (> (cdr entry) 0)
           (let ((elapsed (- (float-time) (cdr entry))))
             (propertize (claude-code-ide-status--format-duration elapsed)
                         'sort-key elapsed)))))
   (t "—")))

(defun claude-code-ide-status--ago-string (time)
  "Return a compact \"N ago\" string for TIME.
Shares the Last output column with live rows, so it carries the same
`sort-key' property and the two kinds of row interleave correctly."
  (let ((elapsed (- (float-time) (float-time time))))
    (propertize (concat (claude-code-ide-status--format-duration elapsed) " ago")
                'sort-key elapsed)))

(defun claude-code-ide-status--state-rank (state)
  "Return a sort key for STATE, most urgent first."
  (pcase state
    ('permission 0)
    ('input 1)
    ('working 2)
    ('waiting 3)
    ('idle 4)
    ('disconnected 5)
    ('resume 6)
    (_ 7)))

(defun claude-code-ide-status--sort-key (cell)
  "Return CELL's numeric sort key.
Duration and count columns are rendered for people -- \"1.3M\", \"3d02h\" --
and those strings do not order the way the numbers do: sorted as text,
\"1.3M\" lands before \"195.8k\" and \"9s\" after \"45m\".  Each such cell
carries the underlying number as a `sort-key' property, and cells with no
number sort last so unknowns do not crowd the head of the list."
  (or (and (stringp cell) (get-text-property 0 'sort-key cell))
      most-positive-fixnum))

(defun claude-code-ide-status--sort-by-key (column)
  "Return a predicate sorting entries by the `sort-key' of COLUMN."
  (lambda (a b)
    (< (claude-code-ide-status--sort-key (aref (cadr a) column))
       (claude-code-ide-status--sort-key (aref (cadr b) column)))))

(defun claude-code-ide-status--sort-by-state (a b)
  "Sort entries A and B by the urgency rank on their State cell.
Each entry is (ID VECTOR); the State cell carries a `sort-rank' property
so the column sorts by urgency rather than by the label's leading glyph."
  (< (or (get-text-property 0 'sort-rank (aref (cadr a) 0)) 99)
     (or (get-text-property 0 'sort-rank (aref (cadr b) 0)) 99)))

(defun claude-code-ide-status--live-entries ()
  "Return `tabulated-list-mode' entries for all live Claude sessions.
Rows are ordered by urgency: a pending permission first, then working,
waiting, idle, and disconnected."
  (claude-code-ide-cleanup-dead-sessions)
  (let (rows)
    (dolist (session (claude-code-ide-sessions))
      ;; One row per instance.  The row id carries the session ID, since a
      ;; project may run several instances and the directory no longer
      ;; identifies one.
      (let* ((dir    (claude-code-ide-mcp-session-project-dir session))
             (state  (claude-code-ide-status--state-for session))
             (branch (or (claude-code-ide-status--branch dir) "—")))
        (push (cons state
                    (list (cons (claude-code-ide-mcp-session-session-id session) 'live)
                          (vector (claude-code-ide-status--state-label state)
                                  (claude-code-ide-session-name session)
                                  (claude-code-ide-status--project-label dir)
                                  branch
                                  (claude-code-ide-status--last-output-string session)
                                  (claude-code-ide-status--output-string dir)
                                  (claude-code-ide-status--uptime-string session))))
              rows)))
    (mapcar #'cdr
            (sort rows (lambda (a b)
                         (< (claude-code-ide-status--state-rank (car a))
                            (claude-code-ide-status--state-rank (car b))))))))

;;; Output-token accounting
;; Claude writes one JSONL transcript per session under
;; `claude-code-ide-status-projects-directory', and every assistant message
;; carries a `usage' object.  Only `output_tokens' is totalled: cache reads
;; outnumber it by orders of magnitude while costing a fraction, so a summed
;; figure would be dominated by the cheapest component and misrepresent the
;; work done.  Turning tokens into money is left alone deliberately -- it
;; needs a per-model price table that goes stale without saying so.

(defvar claude-code-ide-status--output-tokens (make-hash-table :test 'equal)
  "Maps a transcript path to (BYTE-CURSOR . OUTPUT-TOKEN-TOTAL).
Transcripts reach several megabytes and only ever grow at the end, so each
poll reads from the recorded cursor rather than re-reading the file.")

(defun claude-code-ide-status--newest-transcript (project-subdir)
  "Return the most recently modified transcript in PROJECT-SUBDIR, or nil."
  (when-let* ((files (directory-files project-subdir t "\\.jsonl\\'" t)))
    (car (sort files
               (lambda (a b)
                 (time-less-p (nth 5 (file-attributes b))
                              (nth 5 (file-attributes a))))))))

(defun claude-code-ide-status--scan-output-tokens (file)
  "Return the running total of `output_tokens' in FILE.
Reads only the bytes appended since the last call.  A trailing partial
line, which happens while Claude is mid-write, is left unconsumed so the
cursor never lands inside a record."
  (let* ((entry (gethash file claude-code-ide-status--output-tokens))
         (cursor (or (car entry) 0))
         (total (or (cdr entry) 0))
         (size (or (nth 7 (file-attributes file)) 0)))
    ;; A shorter file is a different session reusing the name; start over.
    (when (< size cursor)
      (setq cursor 0 total 0))
    (when (> size cursor)
      (with-temp-buffer
        (insert-file-contents file nil cursor size)
        (goto-char (point-max))
        ;; Consume whole lines only.
        (let ((last-newline (if (re-search-backward "\n" nil t) (1+ (point)) nil)))
          (when last-newline
            (setq cursor (+ cursor (1- last-newline)))
            (narrow-to-region (point-min) last-newline)
            (goto-char (point-min))
            (while (re-search-forward "\"output_tokens\":[[:space:]]*\\([0-9]+\\)" nil t)
              (setq total (+ total (string-to-number (match-string 1)))))))))
    (puthash file (cons cursor total) claude-code-ide-status--output-tokens)
    total))

(defun claude-code-ide-status--format-tokens (n)
  "Format token count N compactly, as in \"12.4k\" or \"1.2M\".
The count rides along as a `sort-key' property, since the rendered string
does not sort the way the number does."
  (let ((text (cond ((null n) "—")
                    ((zerop n) "—")
                    ((< n 1000) (number-to-string n))
                    ((< n 1000000) (format "%.1fk" (/ n 1000.0)))
                    (t (format "%.1fM" (/ n 1000000.0))))))
    ;; No tokens is a known zero, not an unknown, so it sorts as zero.
    (propertize text 'sort-key (or n 0))))

(defun claude-code-ide-status--project-cwd (project-subdir)
  "Return the working directory recorded in PROJECT-SUBDIR, or nil.
PROJECT-SUBDIR is one of Claude's per-project history directories.  The
real path is read from the newest transcript file rather than decoded
from the directory name, whose slash-to-dash encoding is lossy."
  (when-let* ((files (directory-files project-subdir t "\\.jsonl\\'" t))
              (newest (car (sort files
                                 (lambda (a b)
                                   (time-less-p (nth 5 (file-attributes b))
                                                (nth 5 (file-attributes a))))))))
    (with-temp-buffer
      ;; The cwd is recorded on the first real message, past the summary
      ;; line; reading the head of the file is enough and avoids slurping
      ;; multi-megabyte transcripts.
      (insert-file-contents newest nil 0 100000)
      (goto-char (point-min))
      (when (re-search-forward "\"cwd\":[[:space:]]*\"\\([^\"]+\\)\"" nil t)
        (file-name-as-directory (match-string 1))))))

(defvar claude-code-ide-status--resume-cache nil
  "Cached list of resumable-project rows, most recently active first.
Each element is a `tabulated-list-mode' entry for every project on disk,
before live sessions are excluded.  Rebuilt by
`claude-code-ide-status--build-resume-rows'.")

(defvar claude-code-ide-status--resume-cache-time 0
  "`float-time' at which `claude-code-ide-status--resume-cache' was built.")

(defvar claude-code-ide-status--transcript-map (make-hash-table :test 'equal)
  "Maps a project directory to the newest transcript recording work there.")

(defvar claude-code-ide-status--transcript-map-time 0
  "When `claude-code-ide-status--transcript-map' was last rebuilt.")

(defun claude-code-ide-status--transcript-for (dir)
  "Return the newest transcript recording work in DIR, or nil.
Claude's history directories encode their path lossily, so the mapping is
recovered by reading each transcript's own `cwd'.  That walks the same
tree as the resumable-project scan and is cached on the same cadence."
  (when (file-directory-p claude-code-ide-status-projects-directory)
    (when (> (- (float-time) claude-code-ide-status--transcript-map-time)
             claude-code-ide-status-resume-cache-ttl)
      (clrhash claude-code-ide-status--transcript-map)
      (dolist (sub (directory-files claude-code-ide-status-projects-directory t
                                    directory-files-no-dot-files-regexp))
        (when (file-directory-p sub)
          (when-let* ((cwd (claude-code-ide-status--project-cwd sub))
                      (file (claude-code-ide-status--newest-transcript sub)))
            (puthash (claude-code-ide-status--normalize-dir cwd) file
                     claude-code-ide-status--transcript-map))))
      (setq claude-code-ide-status--transcript-map-time (float-time)))
    (gethash (claude-code-ide-status--normalize-dir dir)
             claude-code-ide-status--transcript-map)))

(defun claude-code-ide-status--output-string (dir)
  "Return the output tokens produced by the session in DIR, formatted."
  (claude-code-ide-status--format-tokens
   (when-let* ((file (claude-code-ide-status--transcript-for dir)))
     (claude-code-ide-status--scan-output-tokens file))))

(defvar claude-code-ide-status--resume-cache nil
  "Cached list of resumable-project rows, most recently active first.
Each element is a `tabulated-list-mode' entry for every project on disk,
before live sessions are excluded.  Rebuilt by
`claude-code-ide-status--build-resume-rows'.")

(defvar claude-code-ide-status--resume-cache-time 0
  "`float-time' at which `claude-code-ide-status--resume-cache' was built.")

(defun claude-code-ide-status--build-resume-rows ()
  "Scan the projects directory and build a row for every resumable project.
This reads a transcript and queries git per project, so callers cache the
result rather than repeating it; see `claude-code-ide-status--resume-entries'."
  (when (file-directory-p claude-code-ide-status-projects-directory)
    (let (rows)
      (dolist (sub (directory-files claude-code-ide-status-projects-directory t
                                    directory-files-no-dot-files-regexp))
        (when (file-directory-p sub)
          (when-let* ((dir (claude-code-ide-status--project-cwd sub)))
            (push (cons (nth 5 (file-attributes sub)) dir) rows))))
      ;; Newest first.
      (setq rows (sort rows (lambda (a b) (time-less-p (car b) (car a)))))
      (mapcar
       (lambda (row)
         (let ((mtime (car row))
               (dir   (cdr row)))
           (list (cons dir 'resume)
                 (vector (claude-code-ide-status--state-label 'resume)
                         ""                    ; Instance — none running
                         (claude-code-ide-status--project-label dir)
                         (or (claude-code-ide-status--branch dir) "—")
                         (claude-code-ide-status--ago-string mtime)  ; Last output
                         (claude-code-ide-status--output-string dir)
                         ""))))                ; Uptime — not running
       rows))))

(defun claude-code-ide-status--resume-entries (exclude)
  "Return cached `tabulated-list-mode' entries for resumable projects.
Directories in EXCLUDE (a hash table keyed by directory) are dropped so
live sessions are not listed twice.  The underlying disk scan is rebuilt
only when the cache is older than
`claude-code-ide-status-resume-cache-ttl', keeping the live-refresh timer
cheap; `claude-code-ide-status-refresh' forces an immediate rebuild."
  (when (or (> (- (float-time) claude-code-ide-status--resume-cache-time)
               claude-code-ide-status-resume-cache-ttl)
            ;; Rebuild if cached rows have a stale column count — e.g. after a
            ;; code reload changed the columns — so printing never arefs past
            ;; a short vector.
            (when-let* ((row (car claude-code-ide-status--resume-cache)))
              (/= (length (cadr row)) (length claude-code-ide-status--columns))))
    (setq claude-code-ide-status--resume-cache (claude-code-ide-status--build-resume-rows)
          claude-code-ide-status--resume-cache-time (float-time)))
  (seq-remove (lambda (entry) (gethash (car (car entry)) exclude))
              claude-code-ide-status--resume-cache))

(defun claude-code-ide-status--entries ()
  "Return all `tabulated-list-mode' entries: live sessions then resumable ones."
  (let ((live (claude-code-ide-status--live-entries))
        (seen (make-hash-table :test 'equal)))
    (dolist (entry live)
      (puthash (car (car entry)) t seen))
    (append live (claude-code-ide-status--resume-entries seen))))

(defvar-local claude-code-ide-status--filter nil
  "Current filter string, or nil for no filter.
Set by `claude-code-ide-status-filter'; rows whose text does not match
every whitespace-separated token are hidden.")

(defun claude-code-ide-status--apply-filter (entries)
  "Return the ENTRIES whose row text matches `claude-code-ide-status--filter'.
Matching is case-insensitive and every whitespace-separated token in the
filter must appear somewhere in the row."
  (if (or (null claude-code-ide-status--filter)
          (string-empty-p claude-code-ide-status--filter))
      entries
    (let ((tokens (split-string claude-code-ide-status--filter nil t))
          (case-fold-search t))
      (seq-filter
       (lambda (entry)
         (let ((row (mapconcat (lambda (cell) (if (stringp cell) cell ""))
                               (cadr entry) " ")))
           (seq-every-p (lambda (tok) (string-match-p (regexp-quote tok) row))
                        tokens)))
       entries))))

(defun claude-code-ide-status--column-width (entries col header)
  "Return a width for column COL fitted to ENTRIES, titled HEADER.
The width spans the widest cell (or the header) plus a little padding,
capped by `claude-code-ide-status-max-column-width'."
  (let ((w (string-width header)))
    (dolist (entry entries)
      (setq w (max w (string-width (aref (cadr entry) col)))))
    (min (+ w 2) claude-code-ide-status-max-column-width)))

(defun claude-code-ide-status--refresh-format ()
  "Recompute entries and size each column to fit its content.
Installed on `tabulated-list-revert-hook' so the layout tracks the data
on every refresh.  Sets `tabulated-list-entries' to the computed list to
avoid rebuilding it again during printing."
  (let ((entries (claude-code-ide-status--apply-filter
                  (claude-code-ide-status--entries))))
    (setq tabulated-list-entries entries
          tabulated-list-format
          (vconcat
           (seq-map-indexed
            (lambda (header col)
              (list header
                    (claude-code-ide-status--column-width entries col header)
                    ;; The State column sorts by urgency rank, the rest by
                    ;; their string value.
                    (cond
                     ((equal header "State")
                      #'claude-code-ide-status--sort-by-state)
                     ;; Rendered numbers, so sort on the number not the text.
                     ((member header '("Last output" "Tokens" "Uptime"))
                      (claude-code-ide-status--sort-by-key col))
                     (t t))))
            claude-code-ide-status--columns)))
    (tabulated-list-init-header)))

(defun claude-code-ide-status--header ()
  "Return a one-line summary of session counts for the header line.
Cheap enough to recompute on each redisplay: it counts live states and
reuses the resumable-project cache without rescanning the disk."
  (let ((live 0) (permission 0) (input 0) (working 0) (waiting 0) (resumable 0)
        (running-dirs (mapcar (lambda (session)
                                (claude-code-ide-status--normalize-dir
                                 (claude-code-ide-mcp-session-project-dir session)))
                              (claude-code-ide-sessions))))
    (dolist (session (claude-code-ide-sessions))
      (setq live (1+ live))
      (pcase (claude-code-ide-status--state-for session)
        ('permission (setq permission (1+ permission)))
        ('input (setq input (1+ input)))
        ('working (setq working (1+ working)))
        ('waiting (setq waiting (1+ waiting)))))
    ;; A resumable project is one with no instance running in it.
    (dolist (entry claude-code-ide-status--resume-cache)
      (unless (member (claude-code-ide-status--normalize-dir (car (car entry)))
                      running-dirs)
        (setq resumable (1+ resumable))))
    (let (parts)
      (when (> permission 0) (push (format "%d permission" permission) parts))
      (when (> input 0) (push (format "%d needs-you" input) parts))
      (when (> working 0) (push (format "%d working" working) parts))
      (when (> waiting 0) (push (format "%d waiting" waiting) parts))
      (format " %d live%s · %d resumable%s"
              live
              (if parts (concat " (" (string-join (nreverse parts) ", ") ")") "")
              resumable
              (if (and claude-code-ide-status--filter
                       (not (string-empty-p claude-code-ide-status--filter)))
                  (propertize (format " · filter: %s" claude-code-ide-status--filter)
                              'face 'claude-code-ide-status-waiting-face)
                "")))))

;;; Major mode and commands

(defun claude-code-ide-status--goto-first-row ()
  "Move point to the first session row, skipping the column-header line."
  (goto-char (point-min))
  (while (and (not (eobp)) (null (tabulated-list-get-id)))
    (forward-line 1)))

(defun claude-code-ide-status--goto-last-row ()
  "Move point to the last session row, if any."
  (goto-char (point-max))
  (while (and (not (bobp)) (null (tabulated-list-get-id)))
    (forward-line -1)))

(defun claude-code-ide-status-next-line (&optional n)
  "Move down N session rows, wrapping past the last row back to the first.
Only lines carrying a row id are targets, so the column-header line is
skipped."
  (interactive "p")
  (dotimes (_ (max 1 (or n 1)))
    (forward-line 1)
    ;; Off the end (the trailing line has no id): wrap to the first row.
    (when (null (tabulated-list-get-id))
      (claude-code-ide-status--goto-first-row))))

(defun claude-code-ide-status-previous-line (&optional n)
  "Move up N session rows, wrapping past the first row to the last."
  (interactive "p")
  (dotimes (_ (max 1 (or n 1)))
    (let ((stuck (not (zerop (forward-line -1)))))
      ;; Stuck at the top, or stepped onto the header (no id): wrap to the last.
      (when (or stuck (null (tabulated-list-get-id)))
        (claude-code-ide-status--goto-last-row)))))

(defvar claude-code-ide-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-code-ide-status-visit)
    (define-key map (kbd "o")   #'claude-code-ide-status-visit-split)
    ;; GUI-only convenience alias for `o'; terminals cannot tell S-RET from
    ;; RET, so `o' remains the portable binding.
    (define-key map (kbd "S-<return>") #'claude-code-ide-status-visit-split)
    (define-key map (kbd "D")   #'claude-code-ide-status-stop)
    ;; Override the inherited `tabulated-list-sort', which keys off the
    ;; cursor's column — meaningless here where the highlight is per-line.
    (define-key map (kbd "S")   #'claude-code-ide-status-sort)
    (define-key map (kbd "/")   #'claude-code-ide-status-filter)
    (define-key map (kbd "g")   #'claude-code-ide-status-refresh)
    ;; Wrap-around row movement: past the last row rolls to the first and
    ;; vice versa, and the header (a window header line) is never a target.
    (define-key map (kbd "n")      #'claude-code-ide-status-next-line)
    (define-key map (kbd "p")      #'claude-code-ide-status-previous-line)
    (define-key map (kbd "C-n")    #'claude-code-ide-status-next-line)
    (define-key map (kbd "C-p")    #'claude-code-ide-status-previous-line)
    (define-key map (kbd "<down>") #'claude-code-ide-status-next-line)
    (define-key map (kbd "<up>")   #'claude-code-ide-status-previous-line)
    map)
  "Keymap for `claude-code-ide-status-mode'.")

(define-derived-mode claude-code-ide-status-mode tabulated-list-mode "Claude Sessions"
  "Major mode for the Claude Code session overview.
Shows every live session, highlighting those that need you (a pending
diff, blocked on your input, or a finished turn), followed by resumable
projects from Claude's on-disk history."
  (setq tabulated-list-format
        [("State" 14 t) ("Instance" 20 t) ("Project" 34 t) ("Branch" 18 t)
         ("Last output" 12 t) ("Tokens" 8 t) ("Uptime" 8 t)]
        tabulated-list-entries #'claude-code-ide-status--entries
        tabulated-list-padding 1
        ;; Draw the column header as the first buffer line, but make it
        ;; non-navigable: `claude-code-ide-status--redraw' marks it
        ;; `cursor-intangible' after each print so point skips onto the first
        ;; real row.  The count summary lives in the window header line, which
        ;; every mode-line package leaves alone (unlike the mode line itself).
        tabulated-list-use-header-line nil
        header-line-format '(:eval (claude-code-ide-status--header)))
  (tabulated-list-init-header)
  ;; Enable so the `cursor-intangible' header line actually repels point.
  (cursor-intangible-mode 1)
  ;; Highlight the current row instead of relying on a bare cursor; the
  ;; highlight follows the session by id across the timed refreshes, and
  ;; hiding the cursor leaves the row highlight as the sole selection mark.
  (hl-line-mode 1)
  (setq-local cursor-type nil)
  ;; Under Evil the cursor is driven by the state-cursor variables, which
  ;; would otherwise override `cursor-type'; hide it there too.  Guarded so
  ;; there is no hard dependency on Evil.
  (dolist (v '(evil-normal-state-cursor evil-motion-state-cursor
                                        evil-visual-state-cursor evil-insert-state-cursor
                                        evil-emacs-state-cursor evil-operator-state-cursor))
    (when (boundp v) (set (make-local-variable v) nil)))
  (when (fboundp 'evil-refresh-cursor)
    (ignore-errors (evil-refresh-cursor)))
  ;; Size columns to fit their content on every refresh.
  (add-hook 'tabulated-list-revert-hook
            #'claude-code-ide-status--refresh-format nil t)
  ;; Run the live-refresh timer only while the buffer is actually visible:
  ;; the tick stops it when the buffer is hidden, and this restarts it when
  ;; the buffer is shown again.
  (add-hook 'window-buffer-change-functions
            #'claude-code-ide-status--on-window-change nil t)
  ;; Tear the live-refresh timer down with the buffer it feeds.
  (add-hook 'kill-buffer-hook #'claude-code-ide-status--stop-timer nil t))

(defun claude-code-ide-status--on-window-change (&rest _)
  "Start the refresh timer when the overview buffer becomes visible.
Buffer-local on `window-buffer-change-functions'; `--start-timer' is
idempotent, so this is a no-op when the timer is already running."
  (when (get-buffer-window claude-code-ide-status-buffer-name t)
    (claude-code-ide-status--start-timer)))

;; Optional Evil integration: in Evil's normal/motion state the mode map is
;; shadowed, so RET and `r' never reach the commands and `g' is Evil's
;; prefix.  Mirror the bindings for Evil users (with `gr' to revert, the
;; Evil convention, leaving `g' as its prefix).  No hard dependency: this
;; only runs once Evil is loaded, and fresh status buffers pick the
;; bindings up when Evil normalises keymaps on mode entry.
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

(with-eval-after-load 'evil
  (evil-define-key* '(normal motion) claude-code-ide-status-mode-map
                    (kbd "RET") #'claude-code-ide-status-visit
                    "o"  #'claude-code-ide-status-visit-split
                    (kbd "S-<return>") #'claude-code-ide-status-visit-split
                    "D"  #'claude-code-ide-status-stop
                    ;; Cycle the sort column (`g' restores the default order).
                    "S"  #'claude-code-ide-status-sort
                    "/"  #'claude-code-ide-status-filter
                    "gr" #'claude-code-ide-status-refresh
                    ;; Wrap-around row movement (see the mode-map bindings).
                    "j"  #'claude-code-ide-status-next-line
                    "k"  #'claude-code-ide-status-previous-line
                    (kbd "<down>") #'claude-code-ide-status-next-line
                    (kbd "<up>")   #'claude-code-ide-status-previous-line
                    "q"  #'quit-window))

(defun claude-code-ide-status-visit ()
  "Act on the session at point.
For a live session, switch to its terminal buffer.  For a resumable
project, resume Claude in that directory."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (key (car id))
         (kind (cdr id)))
    (unless id
      (user-error "No session on this line"))
    (pcase kind
      ('live
       (let ((buffer (claude-code-ide-status--session-buffer-at-point key)))
         ;; Attending to this instance clears only its own flag.
         (remhash key claude-code-ide-status--attention)
         (claude-code-ide-status--maybe-refresh)
         (claude-code-ide-pop-to-session-buffer buffer)))
      ('resume
       (let ((default-directory key))
         (claude-code-ide-resume))))))

(defun claude-code-ide-status--main-window ()
  "Return the largest live non-side window, or nil if there is none.
This is the frame's main editing area — the target for a vertical split —
found by ignoring side windows (such as the overview's own popup and the
docked terminals) rather than splitting whichever window is selected."
  (car (sort (seq-filter (lambda (w) (null (window-parameter w 'window-side)))
                         (window-list nil 'no-mini))
             (lambda (a b)
               (> (window-total-width a) (window-total-width b))))))

(defun claude-code-ide-status--display-in-split (buffer &optional _alist)
  "Display BUFFER in a vertical split of the main editing window.
Returns the new window, or nil when there is no main window or it is too
small to split — so it can serve as a `display-buffer' action function
with a fallback.  Splitting the main window keeps the overview and docked
terminals intact."
  (when-let* ((main (claude-code-ide-status--main-window)))
    ;; `split-window' signals when the window is too small; return nil then
    ;; so callers fall back to the ordinary side-window display.
    (ignore-errors
      (let ((new (split-window main nil 'right)))
        (set-window-buffer new buffer)
        new))))

(defun claude-code-ide-status-visit-split ()
  "Like `claude-code-ide-status-visit', but open in a vertical split.
The live session's terminal, or the resumed project, is shown in a window
split from the frame's main editing area rather than in the side window,
leaving the overview window and other docked windows untouched."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (key (car id))
         (kind (cdr id)))
    (unless id
      (user-error "No session on this line"))
    (pcase kind
      ('live
       (let ((buffer (claude-code-ide-status--session-buffer-at-point key)))
         (remhash key claude-code-ide-status--attention)
         (claude-code-ide-status--maybe-refresh)
         (if-let* ((win (claude-code-ide-status--display-in-split buffer)))
             (select-window win)
           ;; No main window to split (frame is all side windows): fall
           ;; back to the ordinary side-window display.
           (claude-code-ide-pop-to-session-buffer buffer))))
      ('resume
       ;; `display-buffer-overriding-action' is consulted before the side-window
       ;; display inside `claude-code-ide-resume', so the split wins without
       ;; touching that code path; a nil return falls through to it.
       (let ((default-directory key)
             (display-buffer-overriding-action
              (list #'claude-code-ide-status--display-in-split)))
         (claude-code-ide-resume))))))

(defun claude-code-ide-status-stop ()
  "Stop the live Claude session on the current line.
Asks for a deliberate confirmation, then kills its terminal buffer; the
process sentinel performs the usual cleanup.  Resume rows have nothing to
stop.  The confirmation requires a full `yes'/`no' answer even when
`use-short-answers' is enabled, since stopping ends a running session."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (key (car id))
         (kind (cdr id)))
    (unless id
      (user-error "No session on this line"))
    (unless (eq kind 'live)
      (user-error "%s is not a live session" (abbreviate-file-name key)))
    (let* ((session (or (claude-code-ide-status--session-by-id key)
                        (user-error "That instance is no longer running")))
           ;; Name the instance, not just the project: a project may run
           ;; several, and stopping the wrong one is not recoverable.
           (name (claude-code-ide-session-name session)))
      (when (let ((use-short-answers nil))
              (yes-or-no-p (format "Stop Claude instance %s? " name)))
        (let ((buffer (claude-code-ide-mcp-session-buffer session)))
          (unless (buffer-live-p buffer)
            (user-error "The buffer for %s no longer exists" name))
          (kill-buffer buffer)
          (claude-code-ide-status--maybe-refresh))))))

(defun claude-code-ide-status-refresh ()
  "Rebuild and redraw the status list now, refreshing cached resume data.
Unlike the automatic refresh, this discards the resumable-project cache so
newly-started or -removed projects on disk are picked up immediately, and
clears any column sort and filter so the list returns to its default view."
  (interactive)
  (setq claude-code-ide-status--resume-cache-time 0
        tabulated-list-sort-key nil
        claude-code-ide-status--filter nil)
  (claude-code-ide-status--redraw))

(defun claude-code-ide-status-filter ()
  "Filter the list by typing; narrows live as you type.
RET keeps the filter, an empty string clears it, and \\[keyboard-quit]
cancels and restores the previous filter.  Rows match when every
whitespace-separated token appears somewhere in the row."
  (interactive)
  (let ((buffer (current-buffer))
        (original claude-code-ide-status--filter))
    (condition-case nil
        (let ((result
               (minibuffer-with-setup-hook
                   (lambda ()
                     (add-hook 'after-change-functions
                               (lambda (&rest _)
                                 (let ((cur (minibuffer-contents-no-properties)))
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (setq claude-code-ide-status--filter cur)
                                       (claude-code-ide-status--redraw)))))
                               nil t))
                 (read-string "Filter (empty clears): " original))))
          (setq claude-code-ide-status--filter
                (if (string-empty-p result) nil result)))
      (quit (setq claude-code-ide-status--filter original)))
    (claude-code-ide-status--redraw)))

(defun claude-code-ide-status-sort ()
  "Cycle the sort column and direction, then back to the default order.
Because the row highlight spans the whole line and the cursor is hidden,
the sort is not tied to the cursor's column.  Each press advances the
cycle: for each column, ascending then reversed, and after the last
column's reversed sort it returns to the default urgency order."
  (interactive)
  (let* ((columns (mapcar #'car (append tabulated-list-format nil)))
         (name (car tabulated-list-sort-key))
         (reversed (cdr tabulated-list-sort-key))
         (index (and name (cl-position name columns :test #'equal)))
         (next
          (cond
           ((null index) (cons (nth 0 columns) nil))                 ; none -> first, ascending
           ((not reversed) (cons name t))                            ; ascending -> reversed
           ((< (1+ index) (length columns)) (cons (nth (1+ index) columns) nil)) ; reversed -> next column
           (t nil))))                                                ; last reversed -> default
    (setq tabulated-list-sort-key next)
    (message "%s" (cond ((null next) "Default order (by urgency)")
                        ((cdr next) (format "Sorted by %s (reversed)" (car next)))
                        (t (format "Sorted by %s" (car next)))))
    (claude-code-ide-status--redraw)))

;;; Live-refresh timer

(defvar claude-code-ide-status--timer nil
  "Repeating timer that refreshes a displayed status buffer, or nil.")

(defun claude-code-ide-status--tick ()
  "Refresh the status buffer if it is on screen; else cancel the timer.
When the buffer is gone or merely hidden the timer stops; it is restarted
when the buffer is shown again (see `claude-code-ide-status--on-window-change')."
  (let ((buffer (get-buffer claude-code-ide-status-buffer-name)))
    (if (and (buffer-live-p buffer) (get-buffer-window buffer t))
        (progn
          (claude-code-ide-status--poll-activity)
          (claude-code-ide-status--maybe-refresh))
      (claude-code-ide-status--stop-timer))))

(defun claude-code-ide-status--start-timer ()
  "Start the live-refresh timer unless it is disabled or already running."
  (when (and claude-code-ide-status-refresh-interval
             (null claude-code-ide-status--timer))
    (setq claude-code-ide-status--timer
          (run-with-timer claude-code-ide-status-refresh-interval
                          claude-code-ide-status-refresh-interval
                          #'claude-code-ide-status--tick))))

(defun claude-code-ide-status--stop-timer ()
  "Cancel the live-refresh timer if it is running."
  (when claude-code-ide-status--timer
    (cancel-timer claude-code-ide-status--timer)
    (setq claude-code-ide-status--timer nil)))

;;;###autoload
(defun claude-code-ide-status ()
  "Display an overview of all Claude Code sessions.
Live sessions are listed first, with those waiting on the user
highlighted, followed by resumable projects from Claude's history.
While displayed, the list refreshes itself every
`claude-code-ide-status-refresh-interval' seconds."
  (interactive)
  (let ((buffer (get-buffer-create claude-code-ide-status-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'claude-code-ide-status-mode)
        (claude-code-ide-status-mode))
      (claude-code-ide-status--redraw))
    (claude-code-ide-status--start-timer)
    (pop-to-buffer buffer)))

;;; Global attention indicator and notifications

(defun claude-code-ide-status-notify-echo (dir state)
  "Announce in the echo area (and ring the bell) that DIR needs attention.
This is the portable default `claude-code-ide-status-notify-function'.
STATE is `permission', `input', or `waiting'.  Set the variable to a
wrapper around `notifications-notify', the `alert' package, or a shell
notifier such as terminal-notifier for real desktop notifications."
  (message "Claude %s — %s"
           (pcase state
             ('permission "has a diff for you to review")
             ('input "needs your input")
             ('waiting "is waiting for you")
             (_ state))
           (abbreviate-file-name dir))
  (ding))

(defvar claude-code-ide-status--attention-lighter ""
  "Mode-line string for `claude-code-ide-status-attention-mode'.")

(defvar claude-code-ide-status--attention-seen (make-hash-table :test 'equal)
  "Maps a directory to its last attention state, for edge-triggered notify.")

(defvar claude-code-ide-status--attention-timer nil
  "Repeating timer backing `claude-code-ide-status-attention-mode', or nil.")

(defun claude-code-ide-status--attention-lighter-string (counts total)
  "Return the mode-line breakdown badge for COUNTS, or \"\" if TOTAL is 0.
COUNTS is an alist of (STATE . N) over the live sessions.  Each non-zero
state is shown as its coloured glyph and count, most urgent first, so the
badge conveys the whole fleet at a glance and lights up red or yellow when
a session needs you.  Clickable to open the overview."
  (if (zerop total)
      ""
    (let ((map (make-sparse-keymap))
          (segments nil))
      (define-key map [mode-line mouse-1] #'claude-code-ide-status)
      (dolist (spec claude-code-ide-status--state-display)
        (let ((n (alist-get (car spec) counts 0)))
          (when (> n 0)
            (push (propertize (format "%s%d" (nth 1 spec) n) 'face (nth 2 spec))
                  segments))))
      (if (null segments)
          ""
        (concat " " (propertize (string-join (nreverse segments) " ")
                                'help-echo "Claude sessions (mouse-1: overview)"
                                'mouse-face 'mode-line-highlight
                                'local-map map))))))

(defun claude-code-ide-status--poll-attention ()
  "Refresh the breakdown badge and notify on newly-attentive sessions."
  (let ((counts nil) (total 0))
    (dolist (session (claude-code-ide-sessions))
      (setq total (1+ total))
      (let* ((id (claude-code-ide-mcp-session-session-id session))
             (dir (claude-code-ide-mcp-session-project-dir session))
             (state (claude-code-ide-status--state-for session))
             ;; Only these states are worth a notification.
             (attention (and (memq state '(permission input waiting)) state)))
        (setf (alist-get state counts 0) (1+ (alist-get state counts 0)))
        ;; Edge trigger, keyed per instance so two instances of one project
        ;; each get their own notification.  The notify function still
        ;; receives the project directory, keeping its published contract.
        (when (and attention
                   claude-code-ide-status-notify
                   (not (eq attention (gethash id claude-code-ide-status--attention-seen))))
          (funcall claude-code-ide-status-notify-function dir attention))
        (if attention
            (puthash id attention claude-code-ide-status--attention-seen)
          (remhash id claude-code-ide-status--attention-seen))))
    (setq claude-code-ide-status--attention-lighter
          (claude-code-ide-status--attention-lighter-string counts total))
    (force-mode-line-update t)))

(defun claude-code-ide-status--start-attention-timer ()
  "Start the attention-indicator poll timer unless it is running."
  (unless claude-code-ide-status--attention-timer
    (setq claude-code-ide-status--attention-timer
          (run-with-timer 0 claude-code-ide-status-attention-interval
                          #'claude-code-ide-status--poll-attention))))

(defun claude-code-ide-status--stop-attention-timer ()
  "Cancel the attention-indicator poll timer if it is running."
  (when claude-code-ide-status--attention-timer
    (cancel-timer claude-code-ide-status--attention-timer)
    (setq claude-code-ide-status--attention-timer nil)))

(defconst claude-code-ide-status--mode-line-construct
  '(:eval claude-code-ide-status--attention-lighter)
  "Mode-line entry rendering the attention lighter.
An `:eval' form rather than a bare variable, because `format-mode-line'
drops the text properties (the clickable keymap) of a variable's string
value but keeps those of an evaluated result.")

;;;###autoload
(define-minor-mode claude-code-ide-status-attention-mode
  "Global mode-line indicator of Claude sessions that need you.
Shows counts of sessions blocked on a permission prompt or waiting on you,
refreshed every `claude-code-ide-status-attention-interval' seconds; click
the badge to open the overview.  With `claude-code-ide-status-notify'
non-nil, `claude-code-ide-status-notify-function' also fires the moment a
session enters one of those states."
  :global t
  :group 'claude-code-ide
  (if claude-code-ide-status-attention-mode
      (progn
        (add-to-list 'global-mode-string
                     claude-code-ide-status--mode-line-construct t)
        (claude-code-ide-status--poll-attention)
        (claude-code-ide-status--start-attention-timer))
    (claude-code-ide-status--stop-attention-timer)
    (setq global-mode-string
          (delete claude-code-ide-status--mode-line-construct global-mode-string)
          claude-code-ide-status--attention-lighter "")
    (force-mode-line-update t)))

(provide 'claude-code-ide-status)
;;; claude-code-ide-status.el ends here
