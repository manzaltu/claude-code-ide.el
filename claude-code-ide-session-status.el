;;; claude-code-ide-session-status.el --- Live status of Claude Code sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Raymond Peck
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

;; Shows what each running Claude Code session does: working, waiting
;; for the user (a permission dialog, a question menu, a plan approval,
;; an MCP elicitation), idle (its turn ended), or failed.
;;
;; `claude-code-ide-session-registry-mode' passes CLI hooks to each
;; instance it starts.  Each hook calls a small shell script that writes
;; the hook input to a file in a directory of this Emacs process, with a
;; sequence number per launch.  Emacs reads the files in order, deletes
;; them, and updates the status of the session.
;;
;; `claude-code-ide-session-status-mode' shows the sessions in a buffer
;; that updates itself, the ones that wait for the user first.  RET in
;; that buffer, or `claude-code-ide-visit-waiting-session' from
;; anywhere, goes to the session.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'filenotify)
(require 'tabulated-list)
(require 'claude-code-ide)
(require 'claude-code-ide-session-registry)

;;; Customization

(defcustom claude-code-ide-session-status-tracking t
  "When non-nil, the session registry passes status hooks to new instances.
The hooks report each step of the CLI to Emacs; each costs one short
shell process.  The value is read at each start, so a change applies
to the instances started afterwards."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-status-start-timeout 15
  "Seconds after which a session that sent no event is shown as unobserved.
Such a session may wait on a dialog that runs before hooks, such as the
dialog that asks whether to trust the folder."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-status-quiet-time 5
  "Seconds without events after which a working session's transcript is read.
An interrupt with Esc fires no hook; the transcript shows it."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-status-display-action
  '((display-buffer-reuse-window display-buffer-in-side-window)
    (side . top)
    (slot . 0)
    (window-height . fit-window-to-buffer)
    (preserve-size . (nil . t)))
  "Display action for the status buffer of `claude-code-ide-session-status-mode'."
  :type 'sexp
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-status-max-height 12
  "Maximum height in lines of the status window."
  :type 'integer
  :group 'claude-code-ide)

(defface claude-code-ide-session-status-waiting
  '((t :inherit error :weight bold))
  "Face for a session that waits for an answer of the user."
  :group 'claude-code-ide)

(defface claude-code-ide-session-status-idle
  '((t :inherit warning))
  "Face for a session whose turn ended."
  :group 'claude-code-ide)

(defface claude-code-ide-session-status-working
  '((t :inherit success))
  "Face for a session that works."
  :group 'claude-code-ide)

(defface claude-code-ide-session-status-quiet
  '((t :inherit shadow))
  "Face for a session that is starting or not tracked."
  :group 'claude-code-ide)

;;; Data

(cl-defstruct (claude-code-ide-session-status--blocker
               (:constructor claude-code-ide-session-status--blocker-create)
               (:copier nil))
  "Something that stops a session until the user answers."
  (kind 'permission :type symbol
        :documentation "`permission', `elicitation' or `notification'.")
  (tool nil :documentation "(AGENT-ID . TOOL-USE-ID) of the tool, or nil.")
  (detail "" :type string)
  (since 0.0 :type float))

(cl-defstruct (claude-code-ide-session-status
               (:constructor claude-code-ide-session-status-create)
               (:copier nil))
  "What one session does, as its hooks reported it."
  (session nil :documentation "The `claude-code-ide-mcp-session'.")
  (key nil :type (or null string) :documentation "Registry key.")
  (nonce "" :type string :documentation "Id of this launch in event file names.")
  (phase 'starting :type symbol
         :documentation "`starting', `working', `idle' or `error'.")
  (detail "" :type string)
  (since 0.0 :type float :documentation "Time the phase started.")
  (registered 0.0 :type float)
  (last-event 0.0 :type float)
  (inflight (make-hash-table :test 'equal)
            :documentation "(AGENT-ID . TOOL-USE-ID) -> (SEQ NAME INPUT).")
  (tool-seq 0 :type integer)
  (blockers nil :type list :documentation "Open blockers, oldest first.")
  (guessed nil :type boolean
           :documentation "Non-nil when the idle phase is a guess of the interrupt check.
A sign of work, such as a tool event, ends it.")
  (transcript-path nil :type (or null string))
  (transcript-offset nil :type (or null integer)))

(defconst claude-code-ide-session-status--events
  '("SessionStart" "UserPromptSubmit" "PreToolUse" "PermissionRequest"
    "PostToolUse" "PostToolUseFailure" "Notification" "Stop" "StopFailure"
    "Elicitation" "ElicitationResult")
  "CLI hook events that the status is computed from.")

(defconst claude-code-ide-session-status--waiting-notifications
  '("permission_prompt" "worker_permission_prompt" "agent_needs_input"
    "elicitation_dialog" "elicitation_url_dialog")
  "Notification types that mean the session waits for the user.")

(defconst claude-code-ide-session-status--interrupt-marker
  "[Request interrupted by user"
  "Text that the CLI writes to the transcript after an interrupt.")

(defconst claude-code-ide-session-status--script
  "#!/bin/sh
# claude-code-ide session status: stores the input of one Claude Code hook
# for Emacs.  Arguments: EVENTS-DIR NONCE EVENT.  It prints nothing and
# always exits 0, so it never changes what the CLI does.
exec 2>/dev/null >/dev/null
(
  dir=$1
  nonce=$2
  event=$3
  mkdir -p \"$dir\" || exit 0
  tmp=$(mktemp \"$dir/.$nonce.in.XXXXXX\") || exit 0
  cat > \"$tmp\" || { rm -f \"$tmp\"; exit 0; }
  # Number the events of this launch in the order they are published
  lock=\"$dir/.$nonce.lock\"
  tries=0
  until mkdir \"$lock\"; do
    tries=$((tries + 1))
    # Break only the lock of a holder that died; Emacs removes old locks
    if [ \"$tries\" -eq 50 ]; then
      holder=$(cat \"$lock/pid\")
      if [ -n \"$holder\" ] && ! kill -0 \"$holder\"; then rm -rf \"$lock\"; fi
    fi
    if [ \"$tries\" -gt 400 ]; then rm -f \"$tmp\"; exit 0; fi
    sleep 0.01
  done
  echo $$ > \"$lock/pid\"
  seq=$(cat \"$dir/.$nonce.seq\")
  case $seq in ''|*[!0-9]*) seq=0 ;; esac
  seq=$((seq + 1))
  echo \"$seq\" > \"$dir/.$nonce.seq.tmp\" && mv -f \"$dir/.$nonce.seq.tmp\" \"$dir/.$nonce.seq\"
  mv \"$tmp\" \"$dir/$nonce-$(printf %08d \"$seq\")-$event.json\" || rm -f \"$tmp\"
  rm -rf \"$lock\"
)
cat >/dev/null
exit 0
"
  "Script that each status hook runs.")

;;; State

(defvar claude-code-ide-session-status--by-nonce (make-hash-table :test 'equal)
  "Status of each tracked launch, by nonce.")

(defvar claude-code-ide-session-status--by-session
  (make-hash-table :test 'eq :weakness 'key)
  "Status of each tracked session.")

(defvar claude-code-ide-session-status--watch nil
  "File notification descriptor of the events directory.")

(defvar claude-code-ide-session-status--drain-timer nil
  "Timer that reads the event files soon.")

(defvar claude-code-ide-session-status--tick-timer nil
  "Timer that reads events, checks quiet sessions and updates the buffer.")

(defconst claude-code-ide-session-status--buffer-name "*Claude Code Status*"
  "Name of the status buffer.")

(defvar claude-code-ide-session-status--urgent-count 0
  "Number of sessions that need the user, as of the last refresh.")

(defvar claude-code-ide-session-status-mode)

;;; Files

(defun claude-code-ide-session-status--events-directory (&optional pid)
  "Return the events directory of the Emacs process PID (default: this one)."
  (file-name-as-directory
   (expand-file-name (format "events-%d" (or pid (emacs-pid)))
                     (claude-code-ide-session-registry--report-directory))))

(defun claude-code-ide-session-status--script-file ()
  "Return the file of the hook script."
  (expand-file-name "claude-code-ide-status-hook.sh"
                    (claude-code-ide-session-registry--report-directory)))

(defun claude-code-ide-session-status--install-script ()
  "Write the hook script when it is missing or differs.
Several Emacs processes can share it, so it is written to a temporary
file and renamed."
  (let ((file (claude-code-ide-session-status--script-file)))
    (unless (and (file-exists-p file)
                 (equal (claude-code-ide-session-registry--file-string file)
                        claude-code-ide-session-status--script))
      (make-directory (file-name-directory file) t)
      (let ((tmp (make-temp-file (concat file "-"))))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region claude-code-ide-session-status--script nil tmp nil 'silent))
              (rename-file tmp file t))
          (when (file-exists-p tmp)
            (delete-file tmp)))))))

(defun claude-code-ide-session-status--command (nonce event)
  "Return the hook command that records EVENT for the launch NONCE."
  (mapconcat #'shell-quote-argument
             (list "sh" (claude-code-ide-session-status--script-file)
                   (claude-code-ide-session-status--events-directory) nonce event)
             " "))

(defun claude-code-ide-session-status--remove-launch-files (nonce)
  "Delete the event, counter and lock files of the launch NONCE."
  (let ((dir (claude-code-ide-session-status--events-directory)))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t (concat "\\`\\.?" (regexp-quote nonce) "[-.]")))
        (ignore-errors
          (if (file-directory-p file)
              (delete-directory file t)
            (delete-file file)))))))

(defun claude-code-ide-session-status--sweep-dead-directories ()
  "Delete the events directories of Emacs processes that no longer run."
  (let ((dir (claude-code-ide-session-registry--report-directory)))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\`events-[0-9]+\\'"))
        (let ((pid (string-to-number (substring (file-name-nondirectory file) 7))))
          (unless (or (= pid (emacs-pid))
                      (let ((command (alist-get 'comm (process-attributes pid))))
                        (and command (string-match-p "emacs" (downcase command)))))
            (ignore-errors (delete-directory file t))))))))

(defun claude-code-ide-session-status--read-event (file)
  "Return the hook input in FILE as an alist, or nil when it cannot be read."
  (condition-case nil
      (let ((text (claude-code-ide-session-registry--file-string file)))
        (if (and (fboundp 'json-available-p) (json-available-p))
            (json-parse-string text :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil)
          (claude-code-ide-session-registry--json-decode text)))
    (error nil)))

(defun claude-code-ide-session-status--remove-stale-locks ()
  "Delete counter locks older than three seconds.
A hook holds its lock for milliseconds; an older lock was left by a hook
that the CLI killed, and would stop the numbering of its launch."
  (let ((dir (claude-code-ide-session-status--events-directory)))
    (when (file-directory-p dir)
      (dolist (lock (directory-files dir t "\\`\\.[0-9a-f]+\\.lock\\'" t))
        (when-let* ((attributes (file-attributes lock)))
          (when (> (float-time (time-subtract nil (file-attribute-modification-time attributes)))
                   3)
            (ignore-errors (delete-directory lock t))))))))

;;; State machine

(defun claude-code-ide-session-status--one-line (text &optional width)
  "Return the first non-empty line of TEXT, cut to WIDTH (default 100)."
  (let* ((line (or (cl-find-if (lambda (line) (not (string-blank-p line)))
                               (split-string (or text "") "\n"))
                   ""))
         (line (string-trim line)))
    (truncate-string-to-width line (or width 100) nil nil "...")))

(defun claude-code-ide-session-status--tool-detail (name input)
  "Return a short description of the tool NAME with INPUT."
  (let ((value (lambda (field) (let ((v (alist-get field input))) (and (stringp v) v)))))
    (claude-code-ide-session-status--one-line
     (pcase name
       ("AskUserQuestion"
        (concat "Question: "
                (or (let ((question (car-safe (alist-get 'questions input))))
                      (and (consp question) (alist-get 'question question)))
                    "")))
       ("ExitPlanMode" "Plan approval")
       ("Bash" (concat "Bash: " (or (funcall value 'command) "")))
       ((or "Edit" "MultiEdit" "Write" "Read" "NotebookEdit")
        (format "%s %s" name (abbreviate-file-name (or (funcall value 'file_path) ""))))
       ("WebFetch" (format "WebFetch %s" (or (funcall value 'url) "")))
       (_ (format "Permission: %s" (or name "a tool")))))))

(defun claude-code-ide-session-status--set-phase (status phase detail now)
  "Set the PHASE and DETAIL of STATUS; a new PHASE starts at NOW."
  (unless (eq (claude-code-ide-session-status-phase status) phase)
    (setf (claude-code-ide-session-status-since status) now))
  (setf (claude-code-ide-session-status-phase status) phase
        (claude-code-ide-session-status-detail status) detail
        (claude-code-ide-session-status-guessed status) nil))

(defun claude-code-ide-session-status--end-main-turn (status)
  "Remove the blockers and tools in flight of STATUS's main agent.
Dialogs and tools of subagents can outlive the main turn."
  (claude-code-ide-session-status--remove-blockers
   status (lambda (blocker)
            (null (car-safe (claude-code-ide-session-status--blocker-tool blocker)))))
  (let ((inflight (claude-code-ide-session-status-inflight status)))
    (maphash (lambda (key _value)
               (unless (car key)
                 (remhash key inflight)))
             inflight)))

(defun claude-code-ide-session-status--add-blocker (status kind tool detail now)
  "Add a blocker of KIND for TOOL with DETAIL to STATUS at NOW."
  (setf (claude-code-ide-session-status-blockers status)
        (append (claude-code-ide-session-status-blockers status)
                (list (claude-code-ide-session-status--blocker-create
                       :kind kind :tool tool :detail detail :since now)))))

(defun claude-code-ide-session-status--remove-blockers (status predicate)
  "Remove the blockers of STATUS for which PREDICATE is non-nil."
  (setf (claude-code-ide-session-status-blockers status)
        (cl-remove-if predicate (claude-code-ide-session-status-blockers status))))

(defun claude-code-ide-session-status--tool-key (data)
  "Return the (AGENT-ID . TOOL-USE-ID) of the tool event DATA."
  (cons (alist-get 'agent_id data) (alist-get 'tool_use_id data)))

(defun claude-code-ide-session-status--match-tool (status data)
  "Return the key of the tool in flight that the permission request DATA is for.
The request has no tool use id, so the newest tool in flight of the same
agent with the same name and input that has no blocker yet is chosen."
  (let ((agent (alist-get 'agent_id data))
        (name (alist-get 'tool_name data))
        (input (alist-get 'tool_input data))
        (blocked (mapcar #'claude-code-ide-session-status--blocker-tool
                         (claude-code-ide-session-status-blockers status)))
        (best nil)
        (best-seq -1))
    (maphash (lambda (key value)
               (when (and (equal (car key) agent)
                          (equal (nth 1 value) name)
                          (equal (nth 2 value) input)
                          (not (member key blocked))
                          (> (car value) best-seq))
                 (setq best key best-seq (car value))))
             (claude-code-ide-session-status-inflight status))
    best))

(defun claude-code-ide-session-status--apply (status event data &optional now)
  "Change STATUS for the hook EVENT with the input DATA, at time NOW.
A session waits while it has blockers: a permission dialog, a question
menu, an elicitation or a waiting notification.  Each blocker ends only
with its own answer, the user's next prompt, the end of the turn or the
idle notification.  Tool events do not turn an idle session into a
working one, since the user's turn has come, unless the idle phase is
a guess of the interrupt check."
  (let* ((now (or now (float-time)))
         (phase (claude-code-ide-session-status-phase status))
         (guessed (claude-code-ide-session-status-guessed status))
         (path (alist-get 'transcript_path data)))
    (setf (claude-code-ide-session-status-last-event status) now)
    (when (stringp path)
      (setf (claude-code-ide-session-status-transcript-path status) path))
    (pcase event
      ("SessionStart"
       ;; Compaction happens inside a turn: the phase stays
       (unless (equal (alist-get 'source data) "compact")
         (clrhash (claude-code-ide-session-status-inflight status))
         (setf (claude-code-ide-session-status-blockers status) nil)
         (claude-code-ide-session-status--set-phase status 'idle "Ready" now)))
      ("UserPromptSubmit"
       (setf (claude-code-ide-session-status-blockers status) nil
             (claude-code-ide-session-status-transcript-offset status)
             (or (claude-code-ide-session-status--transcript-size status) 0))
       (claude-code-ide-session-status--set-phase status 'working "Working" now))
      ("PreToolUse"
       (let ((name (alist-get 'tool_name data)))
         (puthash (claude-code-ide-session-status--tool-key data)
                  (list (cl-incf (claude-code-ide-session-status-tool-seq status))
                        name (alist-get 'tool_input data))
                  (claude-code-ide-session-status-inflight status))
         (when (or guessed (not (eq phase 'idle)))
           (claude-code-ide-session-status--set-phase
            status 'working (format "Working: %s" (or name "tool")) now))))
      ("PermissionRequest"
       (claude-code-ide-session-status--add-blocker
        status 'permission (claude-code-ide-session-status--match-tool status data)
        (claude-code-ide-session-status--tool-detail (alist-get 'tool_name data)
                                                     (alist-get 'tool_input data))
        now))
      ((or "PostToolUse" "PostToolUseFailure")
       (let ((key (claude-code-ide-session-status--tool-key data)))
         (remhash key (claude-code-ide-session-status-inflight status))
         (claude-code-ide-session-status--remove-blockers
          status (lambda (blocker)
                   (equal (claude-code-ide-session-status--blocker-tool blocker) key)))
         (when (or guessed (eq phase 'starting))
           (claude-code-ide-session-status--set-phase status 'working "Working" now))))
      ("Notification"
       (let ((type (alist-get 'notification_type data)))
         (cond
          ((member type claude-code-ide-session-status--waiting-notifications)
           (unless (claude-code-ide-session-status-blockers status)
             (claude-code-ide-session-status--add-blocker
              status 'notification nil
              (claude-code-ide-session-status--one-line
               (or (alist-get 'message data) "Waiting for you"))
              now)))
          ((equal type "idle_prompt")
           (setf (claude-code-ide-session-status-blockers status) nil)
           (claude-code-ide-session-status--set-phase
            status 'idle "Waiting for your input" now)))))
      ("Stop"
       (claude-code-ide-session-status--end-main-turn status)
       (claude-code-ide-session-status--set-phase
        status 'idle
        (let ((text (claude-code-ide-session-status--one-line
                     (alist-get 'last_assistant_message data))))
          (if (string-empty-p text) "Turn ended" text))
        now))
      ("StopFailure"
       (claude-code-ide-session-status--end-main-turn status)
       (claude-code-ide-session-status--set-phase
        status 'error
        (claude-code-ide-session-status--one-line
         (format "Error: %s" (or (alist-get 'error data) (alist-get 'message data) "API error")))
        now))
      ("Elicitation"
       (claude-code-ide-session-status--add-blocker
        status 'elicitation nil
        (claude-code-ide-session-status--one-line
         (format "Input for %s: %s"
                 (or (alist-get 'mcp_server_name data) (alist-get 'mcp_server data) "MCP")
                 (or (alist-get 'message data) "")))
        now))
      ("ElicitationResult"
       (claude-code-ide-session-status--remove-blockers
        status (lambda (blocker)
                 (eq (claude-code-ide-session-status--blocker-kind blocker) 'elicitation)))))))

;;; Interrupts

(defun claude-code-ide-session-status--transcript-size (status)
  "Return the size of STATUS's transcript, or nil."
  (when-let* ((path (claude-code-ide-session-status-transcript-path status)))
    (file-attribute-size (file-attributes path))))

(defun claude-code-ide-session-status--transcript-news (status)
  "Return the text added to STATUS's transcript since the last read, or nil.
At most the last megabyte is read."
  (when-let* ((path (claude-code-ide-session-status-transcript-path status))
              (size (claude-code-ide-session-status--transcript-size status)))
    (let ((offset (claude-code-ide-session-status-transcript-offset status)))
      (if (not (and offset (> size offset)))
          (progn
            (setf (claude-code-ide-session-status-transcript-offset status) size)
            nil)
        (prog1 (with-temp-buffer
                 (let ((coding-system-for-read 'utf-8))
                   (insert-file-contents path nil (max offset (- size 1048576)) size))
                 (buffer-string))
          (setf (claude-code-ide-session-status-transcript-offset status) size))))))

(defun claude-code-ide-session-status--interrupt-record-p (text)
  "Return non-nil when TEXT has the CLI's record of an interrupt.
That is a user message whose text starts with the interrupt marker.
The marker inside a tool result, for example a file that Claude read,
does not count."
  (cl-some
   (lambda (line)
     (when (string-search claude-code-ide-session-status--interrupt-marker line)
       (let* ((record (ignore-errors (claude-code-ide-session-registry--json-decode line)))
              (message (and (consp record) (alist-get 'message record)))
              (content (and (consp message) (alist-get 'content message))))
         (and (equal (alist-get 'type record) "user")
              (cl-some (lambda (text)
                         (and (stringp text)
                              (string-prefix-p claude-code-ide-session-status--interrupt-marker text)))
                       (if (stringp content)
                           (list content)
                         (mapcar (lambda (part)
                                   (and (consp part)
                                        (equal (alist-get 'type part) "text")
                                        (alist-get 'text part)))
                                 content)))))))
   (split-string text "\n" t)))

(defconst claude-code-ide-session-status--busy-regexp
  "^[[:space:]]*[^[:alnum:][:space:]][[:space:]]*[[:alpha:]]+…[[:space:]]*("
  "Regexp for the spinner line that the CLI shows while it works.
For example \"✢ Misting… (3s · ↓ 120 tokens)\" or
\"· Flambéing… (running PostToolUse hook · 1s)\".")

(defun claude-code-ide-session-status--terminal-busy-p (status)
  "Return non-nil when STATUS's terminal shows the CLI's spinner line.
The last 30 lines above the blank rows at the end are read.  When the
buffer is gone, the answer is nil, which shows the session as needing
attention."
  (let ((buffer (and (claude-code-ide-session-status-session status)
                     (claude-code-ide-mcp-session-buffer
                      (claude-code-ide-session-status-session status)))))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (skip-chars-backward " \t\r\n ")
          (forward-line -30)
          (re-search-forward claude-code-ide-session-status--busy-regexp nil t))))))

(defun claude-code-ide-session-status--check-interrupt (status now)
  "Correct STATUS at NOW when it works or stopped although no hook said so.
An interrupt fires no hook.  When a working session is quiet for
`claude-code-ide-session-status-quiet-time' seconds, the CLI's interrupt
record in the new transcript text shows an interrupt during a tool.
An interrupt during an answer leaves no record, so a terminal without
the CLI's spinner line also counts.  Both checks err toward showing
the session as needing the user.  The result is a guess: when the
spinner shows again, or a tool event arrives, the session is working
again.  Return non-nil when STATUS changed."
  (let ((phase (claude-code-ide-session-status-phase status)))
    (cond
     ((and (eq phase 'idle)
           (claude-code-ide-session-status-guessed status))
      (when (claude-code-ide-session-status--terminal-busy-p status)
        (claude-code-ide-session-status--set-phase status 'working "Working" now)
        t))
     ((and (eq phase 'working)
           (null (claude-code-ide-session-status-blockers status))
           (> (- now (claude-code-ide-session-status-last-event status))
              claude-code-ide-session-status-quiet-time))
      (let* ((news (claude-code-ide-session-status--transcript-news status))
             (detail (cond
                      ((and news (claude-code-ide-session-status--interrupt-record-p news))
                       "Interrupted")
                      ((not (claude-code-ide-session-status--terminal-busy-p status))
                       "Interrupted?"))))
        (when detail
          (claude-code-ide-session-status--set-phase status 'idle detail now)
          (setf (claude-code-ide-session-status-guessed status) t)
          t))))))

;;; Reading events

(defun claude-code-ide-session-status--event-files ()
  "Return the event files of this Emacs, oldest first within each launch."
  (let ((dir (claude-code-ide-session-status--events-directory)))
    (when (file-directory-p dir)
      (sort (directory-files dir t "\\`[0-9a-f]+-[0-9]+-[A-Za-z]+\\.json\\'" t)
            #'string<))))

(defun claude-code-ide-session-status--drain ()
  "Apply and delete the waiting event files, at most 500 per call.
Files of launches that this Emacs does not track are deleted unread."
  (when (timerp claude-code-ide-session-status--drain-timer)
    (cancel-timer claude-code-ide-session-status--drain-timer))
  (setq claude-code-ide-session-status--drain-timer nil)
  (claude-code-ide-session-status--remove-stale-locks)
  (let ((files (claude-code-ide-session-status--event-files))
        (count 0)
        (changed nil))
    (while (and files (< count 500))
      (let* ((file (pop files))
             (name (file-name-nondirectory file)))
        (when (string-match "\\`\\([0-9a-f]+\\)-[0-9]+-\\([A-Za-z]+\\)\\.json\\'" name)
          (let ((status (gethash (match-string 1 name) claude-code-ide-session-status--by-nonce))
                (event (match-string 2 name)))
            (when status
              (when-let* ((data (claude-code-ide-session-status--read-event file)))
                (condition-case err
                    (progn
                      (claude-code-ide-session-status--apply status event data)
                      (setq changed t))
                  (error
                   (message "Claude session status: %s event failed: %s"
                            event (error-message-string err))))))))
        (ignore-errors (delete-file file))
        (cl-incf count)))
    (when files
      (claude-code-ide-session-status--schedule-drain))
    (when changed
      (claude-code-ide-session-status--refresh))))

(defun claude-code-ide-session-status--schedule-drain ()
  "Read the event files in a moment.
Later notifications do not move the time, so a stream of events is
still read at a steady rate."
  (unless (timerp claude-code-ide-session-status--drain-timer)
    (setq claude-code-ide-session-status--drain-timer
          (run-with-timer 0.1 nil #'claude-code-ide-session-status--drain))))

(defun claude-code-ide-session-status--on-file-change (_event)
  "Read the event files after a change in the events directory."
  (claude-code-ide-session-status--schedule-drain))

(defun claude-code-ide-session-status--tick ()
  "Read events, check quiet sessions for interrupts and update the buffer.
The reading also covers systems without file notification."
  (claude-code-ide-session-status--drain)
  (let ((now (float-time))
        (changed nil))
    (maphash (lambda (_nonce status)
               (when (ignore-errors (claude-code-ide-session-status--check-interrupt status now))
                 (setq changed t)))
             claude-code-ide-session-status--by-nonce)
    ;; Ages and the unobserved state change with time alone
    (when (or changed
              (> (hash-table-count claude-code-ide-session-status--by-nonce) 0)
              (get-buffer claude-code-ide-session-status--buffer-name))
      (claude-code-ide-session-status--refresh))))

(defun claude-code-ide-session-status--ensure-timers ()
  "Start the tick timer and the file watch."
  (unless (timerp claude-code-ide-session-status--tick-timer)
    (setq claude-code-ide-session-status--tick-timer
          (run-with-timer 2 2 #'claude-code-ide-session-status--tick)))
  (unless claude-code-ide-session-status--watch
    (let ((dir (claude-code-ide-session-status--events-directory)))
      (ignore-errors
        (make-directory dir t)
        (setq claude-code-ide-session-status--watch
              (file-notify-add-watch dir '(change)
                                     #'claude-code-ide-session-status--on-file-change))))))

(defun claude-code-ide-session-status--stop-timers ()
  "Stop the timers and the file watch."
  (dolist (var '(claude-code-ide-session-status--tick-timer
                 claude-code-ide-session-status--drain-timer))
    (when (timerp (symbol-value var))
      (cancel-timer (symbol-value var)))
    (set var nil))
  (when claude-code-ide-session-status--watch
    (ignore-errors (file-notify-rm-watch claude-code-ide-session-status--watch))
    (setq claude-code-ide-session-status--watch nil)))

;;; Tracking

(defun claude-code-ide-session-status--hooks (session key)
  "Start to track SESSION, the registry entry KEY, and return its hooks.
Each hook of `claude-code-ide-session-status--events' runs the hook
script with a new nonce for this launch.  When
`claude-code-ide-session-status-tracking' is nil, return no hooks."
  (when claude-code-ide-session-status-tracking
    (let* ((nonce (substring (replace-regexp-in-string
                              "-" "" (claude-code-ide-session-registry--uuid))
                             0 12))
           (now (float-time))
           (status (claude-code-ide-session-status-create
                    :session session :key key :nonce nonce
                    :since now :registered now :last-event now)))
      (claude-code-ide-session-status--install-script)
      (puthash nonce status claude-code-ide-session-status--by-nonce)
      (puthash session status claude-code-ide-session-status--by-session)
      (claude-code-ide-session-status--ensure-timers)
      (mapcar (lambda (event)
                (list event (claude-code-ide-session-status--command nonce event)))
              claude-code-ide-session-status--events))))

(defun claude-code-ide-session-status--forget (session)
  "Stop tracking SESSION and delete the files of its launch."
  (when-let* ((status (gethash session claude-code-ide-session-status--by-session)))
    (remhash session claude-code-ide-session-status--by-session)
    (remhash (claude-code-ide-session-status-nonce status)
             claude-code-ide-session-status--by-nonce)
    (claude-code-ide-session-status--remove-launch-files
     (claude-code-ide-session-status-nonce status)))
  (when (and (zerop (hash-table-count claude-code-ide-session-status--by-nonce))
             (not claude-code-ide-session-status-mode))
    (claude-code-ide-session-status--stop-timers))
  (claude-code-ide-session-status--refresh))

(defun claude-code-ide-session-status--on-end (session _reason)
  "Stop tracking SESSION, which ended."
  (claude-code-ide-session-status--forget session))

(defun claude-code-ide-session-status--set-tracking (on)
  "Pass status hooks to new instances when ON is non-nil; stop otherwise.
The end hook stays installed, so that sessions tracked before are
still forgotten when they end."
  (add-hook 'claude-code-ide-session-end-functions
            #'claude-code-ide-session-status--on-end)
  (if on
      (progn
        (add-hook 'claude-code-ide-session-registry-hook-functions
                  #'claude-code-ide-session-status--hooks)
        (claude-code-ide-session-status--sweep-dead-directories))
    (remove-hook 'claude-code-ide-session-registry-hook-functions
                 #'claude-code-ide-session-status--hooks)))

;;; Status of a session

(defun claude-code-ide-session-status--state (status now)
  "Return the state of STATUS at NOW.
That is `waiting' while it has blockers, `unobserved' when it sent no
event since it started, else its phase.  A nil STATUS is `untracked'."
  (cond
   ((null status) 'untracked)
   ((claude-code-ide-session-status-blockers status) 'waiting)
   ((and (eq (claude-code-ide-session-status-phase status) 'starting)
         (> (- now (claude-code-ide-session-status-registered status))
            claude-code-ide-session-status-start-timeout))
    'unobserved)
   (t (claude-code-ide-session-status-phase status))))

(defun claude-code-ide-session-status--detail (status state)
  "Return the detail of STATUS in STATE."
  (pcase state
    ('untracked "Started without status hooks")
    ('waiting (claude-code-ide-session-status--blocker-detail
               (car (claude-code-ide-session-status-blockers status))))
    ('unobserved "No events yet: check the terminal")
    ('starting "Starting")
    (_ (claude-code-ide-session-status-detail status))))

(defun claude-code-ide-session-status--state-since (status state)
  "Return the time STATUS entered STATE, or nil."
  (pcase state
    ('untracked nil)
    ('waiting (claude-code-ide-session-status--blocker-since
               (car (claude-code-ide-session-status-blockers status))))
    ('unobserved (claude-code-ide-session-status-registered status))
    (_ (claude-code-ide-session-status-since status))))

(defconst claude-code-ide-session-status--rank
  '((waiting . 0) (error . 1) (unobserved . 2) (idle . 3)
    (starting . 4) (working . 5) (untracked . 6))
  "Order of the states in the status buffer: what needs the user first.")

(defconst claude-code-ide-session-status--labels
  '((waiting "WAITING" claude-code-ide-session-status-waiting)
    (error "ERROR" claude-code-ide-session-status-waiting)
    (unobserved "NO EVENTS" claude-code-ide-session-status-waiting)
    (idle "your turn" claude-code-ide-session-status-idle)
    (starting "starting" claude-code-ide-session-status-quiet)
    (working "working" claude-code-ide-session-status-working)
    (untracked "untracked" claude-code-ide-session-status-quiet))
  "Label and face of each state.")

(defun claude-code-ide-session-status--urgent-p (state)
  "Return non-nil when STATE needs the user at once."
  (memq state '(waiting error unobserved)))

(defun claude-code-ide-session-status--rows (&optional now)
  "Return (SESSION STATE SINCE DETAIL) for each live session at NOW.
The rows are in the order of `claude-code-ide-session-status--rank',
oldest first within a state."
  (let* ((now (or now (float-time)))
         (rows (delq nil
                     (mapcar
                      (lambda (session)
                        (let ((buffer (claude-code-ide-mcp-session-buffer session)))
                          (when (and buffer (buffer-live-p buffer))
                            (let* ((status (gethash session
                                                    claude-code-ide-session-status--by-session))
                                   (state (claude-code-ide-session-status--state status now)))
                              (list session state
                                    (claude-code-ide-session-status--state-since status state)
                                    (claude-code-ide-session-status--detail status state))))))
                      (claude-code-ide-mcp--active-sessions)))))
    (sort rows
          (lambda (a b)
            (let ((rank-a (alist-get (nth 1 a) claude-code-ide-session-status--rank))
                  (rank-b (alist-get (nth 1 b) claude-code-ide-session-status--rank)))
              (if (/= rank-a rank-b)
                  (< rank-a rank-b)
                (< (or (nth 2 a) now) (or (nth 2 b) now))))))))

;;; Status buffer

(defun claude-code-ide-session-status--age (since now)
  "Return the time from SINCE to NOW, short."
  (if (null since)
      ""
    (let ((seconds (max 0 (- now since))))
      (cond
       ((< seconds 60) (format "%ds" seconds))
       ((< seconds 3600) (format "%dm" (/ seconds 60)))
       (t (format "%dh" (/ seconds 3600)))))))

(defun claude-code-ide-session-status--entries (rows now)
  "Return the `tabulated-list-entries' for ROWS at NOW."
  (mapcar (lambda (row)
              (pcase-let* ((`(,session ,state ,since ,detail) row)
                           (`(,label ,face) (alist-get state claude-code-ide-session-status--labels)))
                (list session
                      (vector (propertize label 'face face)
                              (claude-code-ide--session-display-name session)
                              (claude-code-ide-session-status--age since now)
                              (or detail "")))))
          rows))

(defvar claude-code-ide-session-status-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-code-ide-session-status-visit)
    (define-key map (kbd "w") #'claude-code-ide-visit-waiting-session)
    map)
  "Keymap of `claude-code-ide-session-status-list-mode'.")

(define-derived-mode claude-code-ide-session-status-list-mode tabulated-list-mode
  "Claude Status"
  "Live status of the Claude Code sessions of this Emacs.
The sessions that wait for you come first.

\\{claude-code-ide-session-status-list-mode-map}"
  (setq tabulated-list-format [("Status" 10 nil)
                               ("Session" 30 nil)
                               ("For" 5 nil :right-align t)
                               ("Detail" 0 nil)])
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'claude-code-ide-session-status--refresh nil t)
  (tabulated-list-init-header))

(defun claude-code-ide-session-status--refresh ()
  "Update the status buffer, when it exists, and the mode line count."
  (let* ((now (float-time))
         (rows (claude-code-ide-session-status--rows now))
         (count (cl-count-if (lambda (row)
                               (claude-code-ide-session-status--urgent-p (nth 1 row)))
                             rows)))
    (when-let* ((buffer (get-buffer claude-code-ide-session-status--buffer-name)))
      (with-current-buffer buffer
        (setq tabulated-list-entries (claude-code-ide-session-status--entries rows now))
        (tabulated-list-print t))
      (dolist (window (get-buffer-window-list buffer nil t))
        (when (window-parameter window 'window-side)
          (fit-window-to-buffer window claude-code-ide-session-status-max-height))))
    (unless (= count claude-code-ide-session-status--urgent-count)
      (setq claude-code-ide-session-status--urgent-count count)
      (force-mode-line-update t))))

(defun claude-code-ide-session-status--visit-session (session)
  "Show SESSION's terminal buffer and select its window."
  (let ((buffer (claude-code-ide-mcp-session-buffer session)))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "That Claude Code session has ended"))
    (claude-code-ide--touch-session session)
    (if-let* ((window (get-buffer-window buffer t)))
        (progn
          (select-frame-set-input-focus (window-frame window))
          (select-window window))
      (when-let* ((window (claude-code-ide--display-buffer-in-side-window buffer)))
        (select-window window)))))

(defun claude-code-ide-session-status-visit ()
  "Go to the session on this line."
  (interactive)
  (claude-code-ide-session-status--visit-session
   (or (tabulated-list-get-id) (user-error "No session on this line"))))

;;;###autoload
(defun claude-code-ide-visit-waiting-session ()
  "Go to the session that has waited longest for you.
Sessions that wait for an answer come first, then failed and silent
ones, then those whose turn ended."
  (interactive)
  (let ((row (cl-find-if (lambda (row)
                           (memq (nth 1 row) '(waiting error unobserved idle)))
                         (claude-code-ide-session-status--rows))))
    (if row
        (claude-code-ide-session-status--visit-session (car row))
      (message "No Claude Code session waits for you"))))

(defun claude-code-ide-session-status--lighter ()
  "Return the mode line text: the number of sessions that need you at once.
These are the waiting, failed and silent ones."
  (if (> claude-code-ide-session-status--urgent-count 0)
      (propertize (format " Claude[%d need you]" claude-code-ide-session-status--urgent-count)
                  'face 'claude-code-ide-session-status-waiting)
    " Claude"))

(defun claude-code-ide-session-status--display ()
  "Create the status buffer when needed and display it."
  (let ((buffer (get-buffer-create claude-code-ide-session-status--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'claude-code-ide-session-status-list-mode)
        (claude-code-ide-session-status-list-mode)))
    (claude-code-ide-session-status--refresh)
    (display-buffer buffer claude-code-ide-session-status-display-action)
    (claude-code-ide-session-status--refresh)))

;;;###autoload
(define-minor-mode claude-code-ide-session-status-mode
  "Show the live status of each Claude Code session in a buffer.
The buffer lists the sessions of this Emacs, the ones that wait for you
first, and updates itself.  RET goes to a session.  The mode line shows
how many sessions wait.  Turning the mode off closes the buffer.
The status comes from hooks that `claude-code-ide-session-registry-mode'
passes to the instances it starts, so this mode turns it on."
  :global t
  :group 'claude-code-ide
  :lighter (:eval (claude-code-ide-session-status--lighter))
  (if claude-code-ide-session-status-mode
      (progn
        (unless claude-code-ide-session-registry-mode
          (claude-code-ide-session-registry-mode 1))
        (claude-code-ide-session-status--ensure-timers)
        (claude-code-ide-session-status--display))
    (when-let* ((buffer (get-buffer claude-code-ide-session-status--buffer-name)))
      (dolist (window (get-buffer-window-list buffer nil t))
        (ignore-errors (delete-window window)))
      (kill-buffer buffer))
    (when (zerop (hash-table-count claude-code-ide-session-status--by-nonce))
      (claude-code-ide-session-status--stop-timers))
    (force-mode-line-update t)))

;;;###autoload
(defun claude-code-ide-show-session-status ()
  "Show the status buffer of the Claude Code sessions.
This turns on `claude-code-ide-session-status-mode'."
  (interactive)
  (if claude-code-ide-session-status-mode
      (claude-code-ide-session-status--display)
    (claude-code-ide-session-status-mode 1)))

(provide 'claude-code-ide-session-status)

;;; claude-code-ide-session-status.el ends here
