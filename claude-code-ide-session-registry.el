;;; claude-code-ide-session-registry.el --- Crash-safe registry of Claude Code sessions  -*- lexical-binding: t; -*-

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

;; `claude-code-ide-session-registry-mode' records every Claude Code
;; instance that Emacs starts in `claude-code-ide-state-file', a JSON
;; file.  After a crash, `claude-code-ide-restore-sessions' starts every
;; instance that was still open again, in its directory, with its
;; instance name, its model and its flags, resumed by its CLI session id.
;;
;; The file holds only what Emacs knows: the CLI session id, the project
;; directory, the buffer and instance names, the flags and the state of
;; each instance.  The conversation, its title and its model stay in the
;; CLI transcript under ~/.claude/projects, which is read when needed.
;;
;; Emacs starts each new instance with --session-id, so the id is known
;; at once.  A SessionStart hook, passed with --settings, writes the
;; current id to a report file when the CLI changes it (/clear, /resume,
;; or a start with -c or -r), and the registry reads it from there.
;;
;; All writes go through one queue.  The queue drains in order under a
;; lock directory, writes a temporary file and renames it over the state
;; file, and keeps the previous version as a ".bak" file.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'filenotify)
(require 'iso8601)
(require 'tabulated-list)
(require 'claude-code-ide)

(declare-function claude-code-ide-session-status--set-tracking "claude-code-ide-session-status" (on))
(declare-function claude-code-ide-session-status--forget "claude-code-ide-session-status" (session))

;;; Customization

(defcustom claude-code-ide-state-file
  (expand-file-name "~/.emacs-claude-code-ide-state.json")
  "File where `claude-code-ide-session-registry-mode' records sessions.
The previous version is kept next to it, with the suffix \".bak\"."
  :type 'file
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-registry-report-directory nil
  "Directory where the Claude CLI reports its current session id.
A SessionStart hook writes one file per instance here.  nil means a
directory next to `claude-code-ide-state-file', with the same name
plus \"-reports\"."
  :type '(choice (const :tag "Next to the state file" nil) directory)
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-registry-track-note
  "Emacs restarted this session to add status tracking.  Your background tasks and any unread messages from other sessions are gone.  Re-read your last state and re-arm the watches you had before you continue."
  "First message to a session that is restarted to get status hooks.
See `claude-code-ide-session-registry-track-sessions'.  nil sends no
message."
  :type '(choice (const :tag "No message" nil) string)
  :group 'claude-code-ide)

(defcustom claude-code-ide-restore-note
  "This session was resumed after a crash of Emacs.  Your background tasks and any unread messages from other sessions are gone.  Re-read your last state and re-arm the watches you had before you continue."
  "First message to each session that `claude-code-ide-restore-sessions' starts.
nil sends no message."
  :type '(choice (const :tag "No message" nil) string)
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-registry-lock-timeout 10
  "Seconds after which a lock on the state file is stale.
A writer holds the lock for milliseconds.  An older lock comes from an
Emacs that died while it wrote, and is removed."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-registry-lock-wait 2
  "Seconds to wait for the lock before a write is tried again later."
  :type 'number
  :group 'claude-code-ide)

;;; Data

(defconst claude-code-ide-session-registry--version 1
  "Version of the state file format.")

(defconst claude-code-ide-session-registry--states '(open closed archived)
  "States of a registry entry.
`open': the instance runs, or ran when Emacs crashed.
`closed': the instance ended or Emacs exited normally; it can be reopened.
`archived': the instance is done and is never reopened.")

(cl-defstruct (claude-code-ide-session-registry-entry
               (:constructor claude-code-ide-session-registry-entry-create)
               (:copier claude-code-ide-session-registry-entry-copy))
  "One Claude Code instance that Emacs started."
  (key nil :type string :documentation "Stable id of the entry; never changes.")
  (session-id nil :type (or null string) :documentation "Current CLI session id.")
  (transcript-path nil :type (or null string)
                   :documentation "Transcript file, as the CLI reported it.")
  (project-dir nil :type string)
  (buffer-name nil :type (or null string))
  (instance-name nil :type (or null string))
  (model nil :type (or null string) :documentation "The --model of the start flags.")
  (extra-flags "" :type string :documentation "Start flags without model and id flags.")
  (permission-mode nil :type (or null string))
  (state 'open :type symbol)
  (started-at nil :type (or null string))
  (last-seen-at nil :type (or null string))
  (closed-at nil :type (or null string))
  (closed-by nil :type (or null string)
             :documentation "\"exited\", \"killed\" or \"emacs-exit\".")
  (emacs-pid nil :type (or null integer))
  (host nil :type (or null string)))

(defconst claude-code-ide-session-registry--fields
  '((key . key)
    (session_id . session-id)
    (transcript_path . transcript-path)
    (project_dir . project-dir)
    (buffer_name . buffer-name)
    (instance_name . instance-name)
    (model . model)
    (extra_flags . extra-flags)
    (permission_mode . permission-mode)
    (state . state)
    (started_at . started-at)
    (last_seen_at . last-seen-at)
    (closed_at . closed-at)
    (closed_by . closed-by)
    (emacs_pid . emacs-pid)
    (host . host))
  "JSON key and struct slot of each field of an entry.")

(cl-defstruct (claude-code-ide-session-registry--flags
               (:constructor claude-code-ide-session-registry--flags-create)
               (:copier nil))
  "The parts of a CLI flag string that the registry stores separately."
  (rest "" :type string :documentation "The flags without model and id flags.")
  (model nil :type (or null string))
  (session-id nil :type (or null string)
              :documentation "Id that --resume or --session-id names.")
  (sets-session nil :type boolean
                :documentation "Non-nil when the flags choose the session.")
  (has-settings nil :type boolean :documentation "Non-nil when the flags pass --settings.")
  (permission-mode nil :type (or null string)))

(cl-defstruct (claude-code-ide-session-registry--report
               (:constructor claude-code-ide-session-registry--report-create)
               (:copier nil))
  "What the SessionStart hook of one instance reported last."
  (session-id nil :type string)
  (transcript-path nil :type (or null string)))

(define-error 'claude-code-ide-session-registry-error
  "Claude Code session registry error")
(define-error 'claude-code-ide-session-registry-lock-busy
  "State file is locked" 'claude-code-ide-session-registry-error)

;;; State

(defvar claude-code-ide-session-registry--queue nil
  "Changes not yet written, oldest first.
Each element is (LABEL . FUNCTION); FUNCTION takes the list of entries
and returns the changed list.")

(defvar claude-code-ide-session-registry--draining nil
  "Non-nil while the queue drains.
A change submitted meanwhile, from a timer or a hook, is only queued;
the running drain writes it.")

(defvar claude-code-ide-session-registry--retry-timer nil
  "Timer that drains the queue again after the lock was busy.")

(defvar claude-code-ide-session-registry--loaded-from-backup nil
  "Non-nil when the last load could not read the state file.
The next write then moves the unreadable file aside instead of making
it the backup.")

(defvar claude-code-ide-session-registry--keys
  (make-hash-table :test 'eq :weakness 'key)
  "Registry key of each session that this Emacs started.")

(defvar claude-code-ide-session-registry--started
  (make-hash-table :test 'eq :weakness 'key)
  "Sessions whose start the registry recorded.
Only their end changes an entry; a start that failed does not.")

(defvar claude-code-ide-session-registry--adopted
  (make-hash-table :test 'eq :weakness 'key)
  "Adoption of each session that ran before the registry.")

(defvar claude-code-ide-session-registry--adopt-timer nil
  "Timer that reads the session ids of adopted sessions again.")

(defvar claude-code-ide-session-registry--start-key nil
  "Key for the next instance that starts, when it reopens an entry.")

(defvar claude-code-ide-session-registry--exiting nil
  "Non-nil after Emacs started to exit and closed the open entries.")

(defvar claude-code-ide-session-registry--watch nil
  "File notification descriptor of the report directory.")

(defvar claude-code-ide-session-registry--transcript-cache
  (make-hash-table :test 'equal)
  "Values read from transcripts, keyed by kind, file, time and size.")

;;; Small helpers

(defun claude-code-ide-session-registry--now ()
  "Return the current time as an ISO 8601 string in UTC."
  (format-time-string "%FT%TZ" nil t))

(defun claude-code-ide-session-registry--uuid ()
  "Return a new random UUID (version 4) as a string."
  (let ((hex (secure-hash 'sha256 (format "%S %S %S %S" (current-time) (emacs-pid)
                                          (random) (random)))))
    (format "%s-%s-4%s-%x%s-%s"
            (substring hex 0 8)
            (substring hex 8 12)
            (substring hex 13 16)
            (logior 8 (logand 3 (string-to-number (substring hex 16 17) 16)))
            (substring hex 17 20)
            (substring hex 20 32))))

(defun claude-code-ide-session-registry--state-file ()
  "Return the absolute name of the state file."
  (expand-file-name claude-code-ide-state-file))

(defun claude-code-ide-session-registry--backup-file ()
  "Return the name of the backup of the state file."
  (concat (claude-code-ide-session-registry--state-file) ".bak"))

(defun claude-code-ide-session-registry--lock-directory ()
  "Return the name of the lock directory of the state file."
  (concat (claude-code-ide-session-registry--state-file) ".lock"))

(defun claude-code-ide-session-registry--report-directory ()
  "Return the directory where the CLI reports its session id."
  (file-name-as-directory
   (expand-file-name
    (or claude-code-ide-session-registry-report-directory
        (concat (file-name-sans-extension
                 (claude-code-ide-session-registry--state-file))
                "-reports")))))

(defun claude-code-ide-session-registry--report-file (key)
  "Return the report file of the entry KEY."
  (expand-file-name (concat key ".json")
                    (claude-code-ide-session-registry--report-directory)))

(defun claude-code-ide-session-registry--json-decode (text)
  "Parse the JSON TEXT into alists with symbol keys and lists."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (json-read-from-string text)))

(defun claude-code-ide-session-registry--file-string (file)
  "Return the contents of FILE, decoded as UTF-8."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (buffer-string)))

;;; Conversion between entries and JSON

(defun claude-code-ide-session-registry--entry-to-alist (entry)
  "Return ENTRY as an alist for `json-encode'."
  (mapcar (lambda (field)
            (let ((value (cl-struct-slot-value 'claude-code-ide-session-registry-entry
                                               (cdr field) entry)))
              (cons (car field) (if (and value (symbolp value))
                                    (symbol-name value)
                                  value))))
          claude-code-ide-session-registry--fields))

(defun claude-code-ide-session-registry--entry-from-alist (alist)
  "Return the entry that ALIST describes, or nil when it has no key."
  (when (stringp (alist-get 'key alist))
    (let ((entry (claude-code-ide-session-registry-entry-create)))
      (dolist (field claude-code-ide-session-registry--fields)
        (setf (cl-struct-slot-value 'claude-code-ide-session-registry-entry
                                    (cdr field) entry)
              (alist-get (car field) alist)))
      (let ((state (and (stringp (alist-get 'state alist))
                        (intern (alist-get 'state alist)))))
        (setf (claude-code-ide-session-registry-entry-state entry)
              (if (memq state claude-code-ide-session-registry--states) state 'closed)))
      (unless (stringp (claude-code-ide-session-registry-entry-extra-flags entry))
        (setf (claude-code-ide-session-registry-entry-extra-flags entry) ""))
      entry)))

(defun claude-code-ide-session-registry--encode (entries)
  "Return the JSON text of the state file for ENTRIES."
  (let ((json-encoding-pretty-print t))
    (concat (json-encode
             `((version . ,claude-code-ide-session-registry--version)
               (sessions . ,(vconcat (mapcar #'claude-code-ide-session-registry--entry-to-alist
                                             entries)))))
            "\n")))

;;; Reading and writing the file

(defun claude-code-ide-session-registry--read-file (file)
  "Return the entries in FILE, or nil when FILE does not exist.
Signal `claude-code-ide-session-registry-error' when FILE is unreadable."
  (when (file-exists-p file)
    (let ((data (condition-case err
                    (claude-code-ide-session-registry--json-decode
                     (claude-code-ide-session-registry--file-string file))
                  (error
                   (signal 'claude-code-ide-session-registry-error
                           (list file (error-message-string err)))))))
      (unless (and (consp data)
                   (eql (alist-get 'version data) claude-code-ide-session-registry--version)
                   (listp (alist-get 'sessions data)))
        (signal 'claude-code-ide-session-registry-error
                (list file "not a version 1 session registry")))
      (delq nil (mapcar #'claude-code-ide-session-registry--entry-from-alist
                        (alist-get 'sessions data))))))

(defun claude-code-ide-session-registry--load ()
  "Return the entries of the state file.
When the state file is unreadable, use its backup and report it."
  (setq claude-code-ide-session-registry--loaded-from-backup nil)
  (condition-case err
      (claude-code-ide-session-registry--read-file
       (claude-code-ide-session-registry--state-file))
    (claude-code-ide-session-registry-error
     (setq claude-code-ide-session-registry--loaded-from-backup t)
     (let ((backup (claude-code-ide-session-registry--backup-file)))
       (condition-case err2
           (prog1 (claude-code-ide-session-registry--read-file backup)
             (display-warning 'claude-code-ide
                              (format "Session registry: cannot read %s (%s); using %s"
                                      (nth 1 err) (nth 2 err) backup)))
         (claude-code-ide-session-registry-error
          (display-warning 'claude-code-ide
                           (format "Session registry: cannot read %s (%s) nor %s (%s); starting empty"
                                   (nth 1 err) (nth 2 err) backup (nth 2 err2)))
          nil))))))

(defun claude-code-ide-session-registry--write-file (entries)
  "Write ENTRIES to the state file atomically, and keep the old file as backup.
The new content goes to a temporary file in the same directory, which
is then renamed over the state file, so a reader sees the old or the
new file and never a part of one."
  (let* ((file (claude-code-ide-session-registry--state-file))
         (dir (file-name-directory file))
         (prefix (expand-file-name (concat "." (file-name-nondirectory file) "-") dir))
         (text (claude-code-ide-session-registry--encode entries))
         (tmp (make-temp-file prefix nil ".tmp"))
         (backup-tmp nil))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region text nil tmp nil 'silent))
          (when (file-exists-p file)
            (if claude-code-ide-session-registry--loaded-from-backup
                ;; Keep the unreadable file for inspection, and keep the
                ;; good backup that was just used
                (rename-file file (format "%s.corrupt-%s" file
                                          (format-time-string "%Y%m%dT%H%M%S"))
                             t)
              (setq backup-tmp (make-temp-file prefix nil ".bak-tmp"))
              (copy-file file backup-tmp t)
              (rename-file backup-tmp (claude-code-ide-session-registry--backup-file) t)))
          (rename-file tmp file t)
          (setq claude-code-ide-session-registry--loaded-from-backup nil))
      (when (file-exists-p tmp)
        (delete-file tmp))
      (when (and backup-tmp (file-exists-p backup-tmp))
        (delete-file backup-tmp)))))

;;; Lock

(defun claude-code-ide-session-registry--lock-stale-p (lock)
  "Return non-nil when LOCK is older than the lock timeout."
  (when-let* ((attributes (file-attributes lock)))
    (> (float-time (time-subtract nil (file-attribute-modification-time attributes)))
       claude-code-ide-session-registry-lock-timeout)))

(defun claude-code-ide-session-registry--call-with-lock (fn)
  "Call FN while this Emacs holds the lock on the state file.
The lock is a directory: creating it is atomic.  A stale lock is
removed.  Signal `claude-code-ide-session-registry-lock-busy' when the
lock stays taken for `claude-code-ide-session-registry-lock-wait'
seconds."
  (let ((lock (claude-code-ide-session-registry--lock-directory))
        (deadline (+ (float-time) claude-code-ide-session-registry-lock-wait))
        (acquired nil))
    (while (not acquired)
      (condition-case nil
          (progn
            (make-directory lock)
            (setq acquired t))
        (file-already-exists
         (cond
          ((claude-code-ide-session-registry--lock-stale-p lock)
           (ignore-errors (delete-directory lock t)))
          ((> (float-time) deadline)
           (signal 'claude-code-ide-session-registry-lock-busy (list lock)))
          (t (sleep-for 0.05))))))
    (unwind-protect
        (funcall fn)
      (ignore-errors (delete-directory lock t)))))

;;; Reports from the SessionStart hook

(defun claude-code-ide-session-registry--read-report (key)
  "Return the last report of the entry KEY, or nil."
  (let ((file (claude-code-ide-session-registry--report-file key)))
    (when (file-exists-p file)
      (let* ((data (ignore-errors
                     (claude-code-ide-session-registry--json-decode
                      (claude-code-ide-session-registry--file-string file))))
             (id (and (consp data) (alist-get 'session_id data))))
        (when (stringp id)
          (claude-code-ide-session-registry--report-create
           :session-id id
           :transcript-path (let ((path (alist-get 'transcript_path data)))
                              (and (stringp path) path))))))))

(defun claude-code-ide-session-registry--merge-reports (entries)
  "Return ENTRIES with the session ids that the CLI reported."
  (mapcar (lambda (entry)
            (let ((report (claude-code-ide-session-registry--read-report
                           (claude-code-ide-session-registry-entry-key entry))))
              (if (and report
                       (not (and (equal (claude-code-ide-session-registry--report-session-id report)
                                        (claude-code-ide-session-registry-entry-session-id entry))
                                 (equal (claude-code-ide-session-registry--report-transcript-path report)
                                        (claude-code-ide-session-registry-entry-transcript-path entry)))))
                  (let ((copy (claude-code-ide-session-registry-entry-copy entry)))
                    (setf (claude-code-ide-session-registry-entry-session-id copy)
                          (claude-code-ide-session-registry--report-session-id report)
                          (claude-code-ide-session-registry-entry-transcript-path copy)
                          (claude-code-ide-session-registry--report-transcript-path report))
                    copy)
                entry)))
          entries))

(defvar claude-code-ide-session-registry-hook-functions nil
  "Functions that add CLI hooks to the --settings of a new instance.
Each function is called with SESSION and KEY before the CLI starts,
and returns an alist of (EVENT . COMMANDS): EVENT is a hook event
name such as \"Stop\", COMMANDS a list of shell command strings.
The CLI accepts one --settings argument, so all hooks go into it.")

(defconst claude-code-ide-session-registry--hook-timeout 5
  "Seconds after which the CLI stops a hook and goes on.")

(defun claude-code-ide-session-registry--hook-settings (session key)
  "Return the --settings JSON for SESSION with the entry KEY.
The registry's SessionStart hook runs at start, resume, /clear and
compaction, and writes its input, which holds the session id, to the
report file.  A report that is read while it is written does not
parse and is ignored until the next read.  The hooks of
`claude-code-ide-session-registry-hook-functions' are added."
  (let* ((dir (claude-code-ide-session-registry--report-directory))
         (events (list (list "SessionStart"
                             (format "cat > %s"
                                     (shell-quote-argument
                                      (claude-code-ide-session-registry--report-file key)))))))
    (make-directory dir t)
    (run-hook-wrapped 'claude-code-ide-session-registry-hook-functions
                      (lambda (fn)
                        (dolist (pair (funcall fn session key))
                          (if-let* ((known (assoc (car pair) events)))
                              (setcdr known (append (cdr known) (cdr pair)))
                            (setq events (append events (list (cons (car pair) (cdr pair)))))))
                        nil))
    (json-encode
     `((hooks . ,(mapcar (lambda (pair)
                           (cons (intern (car pair))
                                 (vector
                                  `((hooks . ,(vconcat
                                               (mapcar (lambda (command)
                                                         `((type . "command")
                                                           (command . ,command)
                                                           (timeout . ,claude-code-ide-session-registry--hook-timeout)))
                                                       (cdr pair))))))))
                         events))))))

;;; The queue

(defun claude-code-ide-session-registry--schedule-retry ()
  "Drain the queue again in one second."
  (unless (timerp claude-code-ide-session-registry--retry-timer)
    (setq claude-code-ide-session-registry--retry-timer
          (run-with-timer 1 nil (lambda ()
                                  (setq claude-code-ide-session-registry--retry-timer nil)
                                  (claude-code-ide-session-registry--drain))))))

(defun claude-code-ide-session-registry--apply (entries events)
  "Return ENTRIES after each of EVENTS, in order.
An event that signals an error is reported and skipped."
  (dolist (event events entries)
    (condition-case err
        (setq entries (funcall (cdr event) entries))
      (error
       (display-warning 'claude-code-ide
                        (format "Session registry: change %s failed: %s"
                                (car event) (error-message-string err)))))))

(defun claude-code-ide-session-registry--drain ()
  "Write all queued changes to the state file, oldest first.
Each pass reads the file, applies the queued changes and writes it
back, all under the lock.  Changes that arrive during a pass are
written by the next pass.  When the lock stays busy, the changes stay
queued and a timer tries again."
  (unless claude-code-ide-session-registry--draining
    (let ((claude-code-ide-session-registry--draining t))
      (condition-case err
          (while claude-code-ide-session-registry--queue
            (make-directory (file-name-directory (claude-code-ide-session-registry--state-file)) t)
            (claude-code-ide-session-registry--call-with-lock
             (lambda ()
               (let* ((events (copy-sequence claude-code-ide-session-registry--queue))
                      (entries (claude-code-ide-session-registry--apply
                                (claude-code-ide-session-registry--merge-reports
                                 (claude-code-ide-session-registry--load))
                                events)))
                 (claude-code-ide-session-registry--write-file entries)
                 (setq claude-code-ide-session-registry--queue
                       (nthcdr (length events) claude-code-ide-session-registry--queue))))))
        (claude-code-ide-session-registry-lock-busy
         (message "Session registry: %s is locked; trying again" (nth 1 err))
         (claude-code-ide-session-registry--schedule-retry))
        (error
         (display-warning 'claude-code-ide
                          (format "Session registry: cannot write %s: %s"
                                  (claude-code-ide-session-registry--state-file)
                                  (error-message-string err))))))))

(defun claude-code-ide-session-registry--submit (label fn)
  "Queue the change FN, named LABEL, and drain the queue.
FN takes the list of entries and returns the changed list.  This is
the only way the state file changes."
  (setq claude-code-ide-session-registry--queue
        (nconc claude-code-ide-session-registry--queue (list (cons label fn))))
  (claude-code-ide-session-registry--drain))

;;; Changes

(defun claude-code-ide-session-registry--find (entries key)
  "Return the entry of ENTRIES with KEY, or nil."
  (cl-find key entries :key #'claude-code-ide-session-registry-entry-key :test #'equal))

(defun claude-code-ide-session-registry--update (entries keys fn)
  "Return ENTRIES where FN changed a copy of each entry whose key is in KEYS."
  (mapcar (lambda (entry)
            (if (member (claude-code-ide-session-registry-entry-key entry) keys)
                (let ((copy (claude-code-ide-session-registry-entry-copy entry)))
                  (funcall fn copy)
                  copy)
              entry))
          entries))

(defun claude-code-ide-session-registry--change-started (new)
  "Return a change that records the start of the entry NEW.
A new key adds NEW.  A known key, an entry that is reopened, takes the
values of NEW but keeps its first start time, and keeps its session id
when NEW has none."
  (lambda (entries)
    (let ((key (claude-code-ide-session-registry-entry-key new)))
      (if-let* ((old (claude-code-ide-session-registry--find entries key)))
          (claude-code-ide-session-registry--update
           entries (list key)
           (lambda (entry)
             (let ((old-id (claude-code-ide-session-registry-entry-session-id old))
                   (old-path (claude-code-ide-session-registry-entry-transcript-path old)))
               (cl-loop for (_ . slot) in claude-code-ide-session-registry--fields
                        unless (eq slot 'started-at)
                        do (setf (cl-struct-slot-value 'claude-code-ide-session-registry-entry
                                                       slot entry)
                                 (cl-struct-slot-value 'claude-code-ide-session-registry-entry
                                                       slot new)))
               (unless (claude-code-ide-session-registry-entry-session-id entry)
                 (setf (claude-code-ide-session-registry-entry-session-id entry) old-id))
               (when (equal (claude-code-ide-session-registry-entry-session-id entry) old-id)
                 (setf (claude-code-ide-session-registry-entry-transcript-path entry) old-path)))))
        (append entries (list new))))))

(defun claude-code-ide-session-registry--change-closed (keys closed-by)
  "Return a change that closes the open entries with KEYS.
CLOSED-BY says why.  Entries that are not open do not change."
  (lambda (entries)
    (let ((now (claude-code-ide-session-registry--now)))
      (claude-code-ide-session-registry--update
       entries keys
       (lambda (entry)
         (when (eq (claude-code-ide-session-registry-entry-state entry) 'open)
           (setf (claude-code-ide-session-registry-entry-state entry) 'closed
                 (claude-code-ide-session-registry-entry-closed-at entry) now
                 (claude-code-ide-session-registry-entry-closed-by entry) closed-by
                 (claude-code-ide-session-registry-entry-last-seen-at entry) now)))))))

(defun claude-code-ide-session-registry--change-renamed (key buffer-name instance-name)
  "Return a change that sets BUFFER-NAME and INSTANCE-NAME of the entry KEY."
  (lambda (entries)
    (claude-code-ide-session-registry--update
     entries (list key)
     (lambda (entry)
       (setf (claude-code-ide-session-registry-entry-buffer-name entry) buffer-name
             (claude-code-ide-session-registry-entry-instance-name entry) instance-name
             (claude-code-ide-session-registry-entry-last-seen-at entry)
             (claude-code-ide-session-registry--now))))))

(defun claude-code-ide-session-registry--change-archived (key)
  "Return a change that archives the entry KEY."
  (lambda (entries)
    (let ((now (claude-code-ide-session-registry--now)))
      (claude-code-ide-session-registry--update
       entries (list key)
       (lambda (entry)
         (unless (claude-code-ide-session-registry-entry-closed-at entry)
           (setf (claude-code-ide-session-registry-entry-closed-at entry) now))
         (setf (claude-code-ide-session-registry-entry-state entry) 'archived
               (claude-code-ide-session-registry-entry-last-seen-at entry) now))))))

(defun claude-code-ide-session-registry--change-forgotten (key)
  "Return a change that removes the entry KEY."
  (lambda (entries)
    (cl-remove key entries :key #'claude-code-ide-session-registry-entry-key :test #'equal)))

;;; Flags

(defun claude-code-ide-session-registry--parse-flags (flags)
  "Split the CLI flag string FLAGS into the parts that the registry keeps.
The model, the session flags (-c, --continue, -r, --resume,
--session-id, --fork-session) and their values are removed from the
rest; a restore adds the model and --resume again."
  (let ((tokens (claude-code-ide--split-command-line (or flags "")))
        (rest '())
        model session-id sets-session fork has-settings permission-mode)
    (while tokens
      (let ((token (pop tokens)))
        (cond
         ((equal token "--model") (setq model (pop tokens)))
         ;; A restore sets the name from the transcript
         ((member token '("--name" "-n")) (pop tokens))
         ((string-prefix-p "--name=" token))
         ((string-prefix-p "--model=" token) (setq model (substring token 8)))
         ((member token '("-c" "--continue")) (setq sets-session t))
         ((equal token "--fork-session") (setq sets-session t fork t))
         ((member token '("-r" "--resume"))
          (setq sets-session t)
          (when (and tokens (not (string-prefix-p "-" (car tokens))))
            (setq session-id (pop tokens))))
         ((string-prefix-p "--resume=" token)
          (setq sets-session t session-id (substring token 9)))
         ((equal token "--session-id")
          (setq sets-session t session-id (pop tokens)))
         ((string-prefix-p "--session-id=" token)
          (setq sets-session t session-id (substring token 13)))
         (t
          (cond
           ((or (equal token "--settings") (string-prefix-p "--settings=" token))
            (setq has-settings t))
           ((equal token "--permission-mode") (setq permission-mode (car tokens)))
           ((string-prefix-p "--permission-mode=" token)
            (setq permission-mode (substring token 18)))
           ((equal token "--dangerously-skip-permissions")
            (setq permission-mode "bypassPermissions")))
          (push token rest)))))
    (claude-code-ide-session-registry--flags-create
     :rest (mapconcat #'shell-quote-argument (nreverse rest) " ")
     :model model
     ;; A fork gets a new id, which only the report tells
     :session-id (unless fork session-id)
     :sets-session sets-session
     :has-settings has-settings
     :permission-mode permission-mode)))

(defun claude-code-ide-session-registry--own-id-p (spec flags)
  "Return non-nil when Emacs chooses the CLI session id for SPEC.
FLAGS is SPEC's parsed flag string.  A start that continues, resumes
or names a session itself gets its id from the CLI."
  (not (or (claude-code-ide-start-spec-continue spec)
           (claude-code-ide-start-spec-resume spec)
           (claude-code-ide-start-spec-resume-id spec)
           (claude-code-ide-session-registry--flags-sets-session flags))))

;;; Session lifecycle hooks

(defun claude-code-ide-session-registry--cli-args (session spec)
  "Return the CLI arguments that let the registry track SESSION.
SPEC describes the start.  A new conversation gets --session-id with
the entry key.  Every start gets the SessionStart hook that reports
the id, unless the flags pass their own --settings, which a second
--settings would replace."
  (let ((key (or claude-code-ide-session-registry--start-key
                 (claude-code-ide-session-registry--uuid)))
        (flags (claude-code-ide-session-registry--parse-flags
                (claude-code-ide-start-spec-extra-flags spec))))
    (puthash session key claude-code-ide-session-registry--keys)
    (append (when (claude-code-ide-session-registry--own-id-p spec flags)
              (list "--session-id" key))
            (unless (claude-code-ide-session-registry--flags-has-settings flags)
              (list "--settings" (claude-code-ide-session-registry--hook-settings session key))))))

(defun claude-code-ide-session-registry--on-start (session spec)
  "Record that SESSION started as SPEC describes."
  (when-let* ((key (gethash session claude-code-ide-session-registry--keys)))
    (let* ((flags (claude-code-ide-session-registry--parse-flags
                   (claude-code-ide-start-spec-extra-flags spec)))
           (buffer (claude-code-ide-mcp-session-buffer session))
           (now (claude-code-ide-session-registry--now)))
      (puthash session t claude-code-ide-session-registry--started)
      (claude-code-ide-session-registry--submit
       "started"
       (claude-code-ide-session-registry--change-started
        (claude-code-ide-session-registry-entry-create
         :key key
         :session-id (or (claude-code-ide-start-spec-resume-id spec)
                         (claude-code-ide-session-registry--flags-session-id flags)
                         (and (claude-code-ide-session-registry--own-id-p spec flags) key))
         :project-dir (claude-code-ide-start-spec-directory spec)
         :buffer-name (and (buffer-live-p buffer) (buffer-name buffer))
         :instance-name (claude-code-ide-start-spec-instance-name spec)
         :model (claude-code-ide-session-registry--flags-model flags)
         :extra-flags (claude-code-ide-session-registry--flags-rest flags)
         :permission-mode (claude-code-ide-session-registry--flags-permission-mode flags)
         :state 'open
         :started-at now
         :last-seen-at now
         :emacs-pid (emacs-pid)
         :host (system-name)))))))

(defun claude-code-ide-session-registry--on-end (session reason)
  "Record that SESSION ended for REASON (`exited' or `killed').
The end of a session whose start failed leaves its entry as it was:
a failed restore keeps the entry `open' for the next try."
  (when-let* ((key (gethash session claude-code-ide-session-registry--keys)))
    (remhash session claude-code-ide-session-registry--keys)
    (remhash session claude-code-ide-session-registry--adopted)
    (when (and (gethash session claude-code-ide-session-registry--started)
               (not claude-code-ide-session-registry--exiting))
      (remhash session claude-code-ide-session-registry--started)
      (claude-code-ide-session-registry--submit
       "ended"
       (claude-code-ide-session-registry--change-closed (list key) (symbol-name reason))))))

(defun claude-code-ide-session-registry--on-rename (session)
  "Record the new buffer and instance name of SESSION."
  (when-let* ((key (gethash session claude-code-ide-session-registry--keys)))
    (let ((buffer (claude-code-ide-mcp-session-buffer session)))
      (claude-code-ide-session-registry--submit
       "renamed"
       (claude-code-ide-session-registry--change-renamed
        key
        (and (buffer-live-p buffer) (buffer-name buffer))
        (claude-code-ide-mcp-session-instance-name session))))))

(defun claude-code-ide-session-registry--live-keys ()
  "Return the keys of the sessions that run in this Emacs."
  (let ((keys '()))
    (maphash (lambda (session key)
               (let ((buffer (claude-code-ide-mcp-session-buffer session)))
                 (when (and buffer (buffer-live-p buffer)
                            (not (claude-code-ide-mcp-session-cleanup-done session)))
                   (push key keys))))
             claude-code-ide-session-registry--keys)
    keys))

(defun claude-code-ide-session-registry--on-kill-emacs ()
  "Close the entries of the sessions that run in this Emacs.
After a normal exit they are `closed'; after a crash this never runs
and they stay `open', which is what a restore looks for."
  (ignore-errors (claude-code-ide-session-registry--refresh-adopted-ids))
  (let ((keys (claude-code-ide-session-registry--live-keys)))
    (setq claude-code-ide-session-registry--exiting t)
    (when keys
      (claude-code-ide-session-registry--submit
       "emacs-exit"
       (claude-code-ide-session-registry--change-closed keys "emacs-exit")))))

(defun claude-code-ide-session-registry--on-report-change (event)
  "Write the session id that the file notification EVENT reports."
  (let ((action (nth 1 event))
        (file (or (nth 3 event) (nth 2 event))))
    (when (and (memq action '(created changed renamed))
               (stringp file)
               (string-suffix-p ".json" file))
      (claude-code-ide-session-registry--submit "report" #'identity))))

(defun claude-code-ide-session-registry--start-watch ()
  "Watch the report directory, when file notification is available."
  (let ((dir (claude-code-ide-session-registry--report-directory)))
    (unless claude-code-ide-session-registry--watch
      (ignore-errors
        (make-directory dir t)
        (setq claude-code-ide-session-registry--watch
              (file-notify-add-watch dir '(change)
                                     #'claude-code-ide-session-registry--on-report-change))))))

(defun claude-code-ide-session-registry--stop-watch ()
  "Stop watching the report directory."
  (when claude-code-ide-session-registry--watch
    (ignore-errors (file-notify-rm-watch claude-code-ide-session-registry--watch))
    (setq claude-code-ide-session-registry--watch nil)))

;;; Reading the registry

(defun claude-code-ide-session-registry--entries ()
  "Return the current entries, with the reported session ids."
  (claude-code-ide-session-registry--merge-reports
   (claude-code-ide-session-registry--load)))

(defun claude-code-ide-session-registry--live-p (entry)
  "Return non-nil when ENTRY's instance runs in this Emacs."
  (member (claude-code-ide-session-registry-entry-key entry)
          (claude-code-ide-session-registry--live-keys)))

(defun claude-code-ide-session-registry--other-emacs-p (entry)
  "Return non-nil when another live Emacs on this host owns ENTRY."
  (let ((pid (claude-code-ide-session-registry-entry-emacs-pid entry)))
    (and (integerp pid)
         (/= pid (emacs-pid))
         (let ((command (alist-get 'comm (process-attributes pid))))
           (and command (string-match-p "emacs" (downcase command)))))))

(cl-defstruct (claude-code-ide-session-registry--cli-session
               (:constructor claude-code-ide-session-registry--cli-session-create)
               (:copier nil))
  "A Claude CLI process that runs, from its file in ~/.claude/sessions."
  (pid 0 :type integer)
  (session-id nil :type (or null string))
  (cwd nil :type (or null string))
  (name nil :type (or null string))
  (name-from-user nil :type boolean
                  :documentation "Non-nil when the user or --name set the name.")
  (status nil :type (or null string)
          :documentation "What the CLI reports it does: \"idle\", \"busy\" or \"shell\"."))

(defun claude-code-ide-session-registry--cli-session-file (pid)
  "Return the file that the Claude CLI process PID keeps about itself."
  (expand-file-name (format "sessions/%d.json" pid)
                    (or (getenv "CLAUDE_CONFIG_DIR") (expand-file-name "~/.claude"))))

(defun claude-code-ide-session-registry--read-cli-session (file)
  "Return the CLI session in FILE when its process runs, else nil."
  (let* ((data (ignore-errors
                 (claude-code-ide-session-registry--json-decode
                  (claude-code-ide-session-registry--file-string file))))
         (pid (and (consp data) (alist-get 'pid data))))
    (when (and (integerp pid) (process-attributes pid))
      (claude-code-ide-session-registry--cli-session-create
       :pid pid
       :session-id (let ((id (alist-get 'sessionId data))) (and (stringp id) id))
       :cwd (let ((cwd (alist-get 'cwd data))) (and (stringp cwd) cwd))
       :name (let ((name (alist-get 'name data))) (and (stringp name) name))
       :name-from-user (equal (alist-get 'nameSource data) "user")
       :status (let ((status (alist-get 'status data))) (and (stringp status) status))))))

(defun claude-code-ide-session-registry--live-cli-sessions ()
  "Return the CLI sessions that run on this host."
  (let ((dir (file-name-directory (claude-code-ide-session-registry--cli-session-file 0))))
    (when (file-directory-p dir)
      (delq nil (mapcar #'claude-code-ide-session-registry--read-cli-session
                        (directory-files dir t "\\`[0-9]+\\.json\\'"))))))

(defun claude-code-ide-session-registry--session-pid (session)
  "Return the PID of SESSION's CLI process, or nil."
  (or (claude-code-ide-mcp-session-cli-pid session)
      (let ((process (claude-code-ide-mcp-session-process session)))
        (and process (process-live-p process) (process-id process)))))

(defun claude-code-ide-session-registry--cli-session-of (session)
  "Return the CLI session that SESSION runs, or nil."
  (when-let* ((pid (claude-code-ide-session-registry--session-pid session)))
    (claude-code-ide-session-registry--read-cli-session
     (claude-code-ide-session-registry--cli-session-file pid))))

(cl-defstruct (claude-code-ide-session-registry--restore-context
               (:constructor claude-code-ide-session-registry--restore-context-create)
               (:copier nil))
  "What a restore checks each entry against."
  (live-ids nil :type list :documentation "Session ids of CLI processes that run.")
  (exit-keys nil :type list
             :documentation "Keys of the entries that the last Emacs closed at its exit."))

(defun claude-code-ide-session-registry--exit-keys (entries)
  "Return the keys of ENTRIES that the last Emacs on this host closed at its exit.
That Emacs no longer runs.  Older exits are left out, and so is an exit
after which another Emacs started sessions: that Emacs ran last."
  (let ((latest nil)
        (latest-pid nil))
    (dolist (entry entries)
      (when (and (equal (claude-code-ide-session-registry-entry-closed-by entry) "emacs-exit")
                 (equal (claude-code-ide-session-registry-entry-host entry) (system-name))
                 (not (eql (claude-code-ide-session-registry-entry-emacs-pid entry) (emacs-pid)))
                 (not (claude-code-ide-session-registry--other-emacs-p entry))
                 (string< (or latest "")
                          (or (claude-code-ide-session-registry-entry-closed-at entry) "")))
        (setq latest (claude-code-ide-session-registry-entry-closed-at entry)
              latest-pid (claude-code-ide-session-registry-entry-emacs-pid entry))))
    (when (and latest-pid
               (not (cl-some
                     (lambda (entry)
                       (let ((pid (claude-code-ide-session-registry-entry-emacs-pid entry)))
                         (and (equal (claude-code-ide-session-registry-entry-host entry) (system-name))
                              (not (eql pid latest-pid))
                              (not (eql pid (emacs-pid)))
                              (string< latest
                                       (or (claude-code-ide-session-registry-entry-started-at entry) "")))))
                     entries)))
      (mapcar #'claude-code-ide-session-registry-entry-key
              (cl-remove-if-not
               (lambda (entry)
                 (and (equal (claude-code-ide-session-registry-entry-closed-by entry) "emacs-exit")
                      (eql (claude-code-ide-session-registry-entry-emacs-pid entry) latest-pid)
                      (equal (claude-code-ide-session-registry-entry-state entry) 'closed)))
               entries)))))

(defun claude-code-ide-session-registry--restore-context (entries)
  "Return the restore context for ENTRIES now."
  (claude-code-ide-session-registry--restore-context-create
   :live-ids (delq nil (mapcar #'claude-code-ide-session-registry--cli-session-session-id
                               (claude-code-ide-session-registry--live-cli-sessions)))
   :exit-keys (claude-code-ide-session-registry--exit-keys entries)))

(defun claude-code-ide-session-registry--restorable-p (entry &optional context)
  "Return non-nil when ENTRY ran when its Emacs ended, and runs no more.
That is an `open' entry of an Emacs that crashed, or an entry that the
last Emacs closed at its exit; such an exit can also come from a
signal.  Entries of another host or of another live Emacs, entries
that run in this Emacs, and entries whose session id a running CLI
process has are left alone.  CONTEXT is a
`claude-code-ide-session-registry--restore-context'; nil computes one."
  (let ((context (or context
                     (claude-code-ide-session-registry--restore-context
                      (claude-code-ide-session-registry--entries)))))
    (and (or (and (eq (claude-code-ide-session-registry-entry-state entry) 'open)
                  (equal (claude-code-ide-session-registry-entry-host entry) (system-name))
                  (not (claude-code-ide-session-registry--other-emacs-p entry)))
             (member (claude-code-ide-session-registry-entry-key entry)
                     (claude-code-ide-session-registry--restore-context-exit-keys context)))
         (not (claude-code-ide-session-registry--live-p entry))
         (not (member (claude-code-ide-session-registry-entry-session-id entry)
                      (claude-code-ide-session-registry--restore-context-live-ids context))))))

(defun claude-code-ide-session-registry--restorable-entries ()
  "Return the entries that a restore starts again, oldest first.
A session id appears once, with its newest entry, so a restore never
resumes one conversation twice."
  (let* ((entries (claude-code-ide-session-registry--entries))
         (context (claude-code-ide-session-registry--restore-context entries))
         (newest (make-hash-table :test 'equal)))
    (dolist (entry (claude-code-ide-session-registry--sort
                    (cl-remove-if-not (lambda (entry)
                                        (claude-code-ide-session-registry--restorable-p entry context))
                                      entries)))
      (puthash (or (claude-code-ide-session-registry-entry-session-id entry)
                   (claude-code-ide-session-registry-entry-key entry))
               entry newest))
    (claude-code-ide-session-registry--sort (hash-table-values newest))))

;;; Sessions that ran before the registry

(cl-defstruct (claude-code-ide-session-registry--adoption
               (:constructor claude-code-ide-session-registry--adoption-create)
               (:copier nil))
  "A session that ran before the registry, and how to follow its id."
  (pid 0 :type integer :documentation "PID of its CLI process.")
  (session-id nil :type (or null string) :documentation "The id last recorded."))

(defun claude-code-ide-session-registry--change-session-id (key session-id)
  "Return a change that sets the SESSION-ID of the entry KEY."
  (lambda (entries)
    (claude-code-ide-session-registry--update
     entries (list key)
     (lambda (entry)
       (setf (claude-code-ide-session-registry-entry-session-id entry) session-id
             (claude-code-ide-session-registry-entry-transcript-path entry) nil
             (claude-code-ide-session-registry-entry-last-seen-at entry)
             (claude-code-ide-session-registry--now))))))

(defun claude-code-ide-session-registry--adopt-running-sessions ()
  "Record the sessions of this Emacs that run without a registry entry.
They started before `claude-code-ide-session-registry-mode' was on, so
their CLI has no hooks.  Their session id comes from the file that the
CLI process keeps in ~/.claude/sessions; it is read again every minute
and at exit, since /clear changes it.  Return the number adopted."
  (let ((live nil)
        (adopted 0))
    (dolist (session (claude-code-ide-mcp--active-sessions))
      (let ((buffer (claude-code-ide-mcp-session-buffer session))
            (dir (file-name-as-directory
                  (expand-file-name (claude-code-ide-mcp-session-project-dir session)))))
        (when (and buffer (buffer-live-p buffer)
                   (not (gethash session claude-code-ide-session-registry--keys)))
          (let* ((cli (or (claude-code-ide-session-registry--cli-session-of session)
                          ;; Without a PID: the only CLI that runs in its directory
                          (let ((same-dir
                                 (cl-remove-if-not
                                  (lambda (cli)
                                    (equal (file-name-as-directory
                                            (expand-file-name
                                             (or (claude-code-ide-session-registry--cli-session-cwd cli) "/")))
                                           dir))
                                  (or live (setq live (claude-code-ide-session-registry--live-cli-sessions))))))
                            (and (= (length same-dir) 1) (car same-dir)))))
                 (id (and cli (claude-code-ide-session-registry--cli-session-session-id cli))))
            (when id
              (let ((key (claude-code-ide-session-registry--uuid))
                    (now (claude-code-ide-session-registry--now)))
                (puthash session key claude-code-ide-session-registry--keys)
                (puthash session t claude-code-ide-session-registry--started)
                (puthash session (claude-code-ide-session-registry--adoption-create
                                  :pid (claude-code-ide-session-registry--cli-session-pid cli)
                                  :session-id id)
                         claude-code-ide-session-registry--adopted)
                (claude-code-ide-session-registry--submit
                 "adopted"
                 (claude-code-ide-session-registry--change-started
                  (claude-code-ide-session-registry-entry-create
                   :key key :session-id id :project-dir dir
                   :buffer-name (buffer-name buffer)
                   :instance-name (claude-code-ide-mcp-session-instance-name session)
                   :extra-flags "" :state 'open :started-at now :last-seen-at now
                   :emacs-pid (emacs-pid) :host (system-name))))
                (cl-incf adopted)))))))
    (when (and (> (hash-table-count claude-code-ide-session-registry--adopted) 0)
               (not (timerp claude-code-ide-session-registry--adopt-timer)))
      (setq claude-code-ide-session-registry--adopt-timer
            (run-with-timer 60 60 #'claude-code-ide-session-registry--refresh-adopted-ids)))
    adopted))

(defun claude-code-ide-session-registry--refresh-adopted-ids ()
  "Record the current session id of each adopted session that changed it."
  (maphash
   (lambda (session adoption)
     (let ((key (gethash session claude-code-ide-session-registry--keys))
           (cli (claude-code-ide-session-registry--read-cli-session
                 (claude-code-ide-session-registry--cli-session-file
                  (claude-code-ide-session-registry--adoption-pid adoption)))))
       (when-let* ((id (and key cli (claude-code-ide-session-registry--cli-session-session-id cli))))
         (unless (equal id (claude-code-ide-session-registry--adoption-session-id adoption))
           (setf (claude-code-ide-session-registry--adoption-session-id adoption) id)
           (claude-code-ide-session-registry--submit
            "adopted id" (claude-code-ide-session-registry--change-session-id key id))))))
   claude-code-ide-session-registry--adopted)
  (when (and (zerop (hash-table-count claude-code-ide-session-registry--adopted))
             (timerp claude-code-ide-session-registry--adopt-timer))
    (cancel-timer claude-code-ide-session-registry--adopt-timer)
    (setq claude-code-ide-session-registry--adopt-timer nil)))

(defun claude-code-ide-session-registry--untracked-sessions ()
  "Return the live sessions of this Emacs that run without status hooks.
These are the sessions that the registry adopted: they ran before
`claude-code-ide-session-registry-mode' was on."
  (let ((sessions '()))
    (maphash (lambda (session _adoption)
               (let ((buffer (claude-code-ide-mcp-session-buffer session)))
                 (when (and buffer (buffer-live-p buffer)
                            (not (claude-code-ide-mcp-session-cleanup-done session)))
                   (push session sessions))))
             claude-code-ide-session-registry--adopted)
    sessions))

(defun claude-code-ide-session-registry--wait-for-exit (pid)
  "Wait up to five seconds until the process PID ends; then send it SIGTERM.
Return non-nil when it ended."
  (let ((tries 0))
    (while (and (process-attributes pid) (< tries 50))
      (when (= tries 30)
        (ignore-errors (signal-process pid 'SIGTERM)))
      (sleep-for 0.1)
      (cl-incf tries))
    (not (process-attributes pid))))

(defun claude-code-ide-session-registry--track-session (session note &optional force)
  "Restart the untracked SESSION through the registry, so that its CLI gets hooks.
The CLI ends, and its conversation resumes in a new CLI with the same
instance name, title and model, with NOTE as its first message.  A
session that the CLI reports as busy is refused unless FORCE is
non-nil, since its running tools and background tasks end."
  (let* ((name (claude-code-ide--session-display-name session))
         (key (gethash session claude-code-ide-session-registry--keys))
         (cli (claude-code-ide-session-registry--cli-session-of session))
         (pid (and cli (claude-code-ide-session-registry--cli-session-pid cli))))
    (unless key
      (user-error "%s has no registry entry" name))
    (when (and (not force)
               (member (and cli (claude-code-ide-session-registry--cli-session-status cli))
                       '("busy" "shell")))
      (user-error "%s is busy; try again when it waits, or use a prefix argument" name))
    (claude-code-ide-session-registry--refresh-adopted-ids)
    (let ((entry (claude-code-ide-session-registry--find
                  (claude-code-ide-session-registry--entries) key)))
      (unless (and entry (claude-code-ide-session-registry-entry-session-id entry))
        (user-error "No Claude session id is known for %s" name))
      ;; Killing the buffer ends the CLI and closes the entry
      (let ((kill-buffer-query-functions nil))
        (kill-buffer (claude-code-ide-mcp-session-buffer session)))
      (when (and pid (not (claude-code-ide-session-registry--wait-for-exit pid)))
        (user-error "The old CLI of %s (PID %d) does not end" name pid))
      (claude-code-ide-session-registry--start-entry entry note)
      t)))

;;;###autoload
(defun claude-code-ide-session-registry-track-sessions (&optional force)
  "Restart the sessions that run without status hooks, so that they get them.
These sessions ran before `claude-code-ide-session-registry-mode' was
on; the status buffer shows them as UNTRACKED.  Each resumes its
conversation with its instance name, title and model, and gets
`claude-code-ide-session-registry-track-note' as its first message.
Their running tools and background tasks end.  Sessions that are busy
are skipped, unless FORCE (the prefix argument) is non-nil."
  (interactive "P")
  (let ((sessions (claude-code-ide-session-registry--untracked-sessions)))
    (cond
     ((null sessions) (message "Every Claude Code session has status hooks"))
     ((yes-or-no-p (format "Restart %d session%s to add status tracking?  Their background tasks end.  "
                           (length sessions) (if (cdr sessions) "s" "")))
      (let ((delay 0))
        (dolist (session sessions)
          (run-with-timer
           delay nil
           (lambda ()
             (condition-case err
                 (progn
                   (claude-code-ide-session-registry--track-session
                    session claude-code-ide-session-registry-track-note force)
                   (message "Restarted %s with status tracking"
                            (claude-code-ide--session-display-name session)))
               ((error quit)
                (message "Not restarted: %s" (if (eq (car err) 'quit) "quit"
                                               (error-message-string err)))))))
          (setq delay (+ delay 2))))))))

(defun claude-code-ide-session-registry--reopenable-p (entry)
  "Return non-nil when ENTRY can be started again by hand."
  (and (not (eq (claude-code-ide-session-registry-entry-state entry) 'archived))
       (not (claude-code-ide-session-registry--live-p entry))))

;;; Transcripts

(defun claude-code-ide-session-registry--project-slug (dir)
  "Return the name of the transcript directory of the project DIR.
The CLI replaces each character that is not a letter or a digit with -."
  (replace-regexp-in-string "[^A-Za-z0-9]" "-"
                            (directory-file-name (expand-file-name dir))))

(defun claude-code-ide-session-registry--transcript-file (entry)
  "Return ENTRY's transcript file, or nil when it does not exist."
  (let ((path (claude-code-ide-session-registry-entry-transcript-path entry))
        (id (claude-code-ide-session-registry-entry-session-id entry)))
    (cond
     ((and path (file-exists-p path)) path)
     (id
      (let ((file (expand-file-name
                   (concat id ".jsonl")
                   (expand-file-name
                    (claude-code-ide-session-registry--project-slug
                     (claude-code-ide-session-registry-entry-project-dir entry))
                    (expand-file-name "projects"
                                      (or (getenv "CLAUDE_CONFIG_DIR")
                                          (expand-file-name "~/.claude")))))))
        (and (file-exists-p file) file))))))

(defun claude-code-ide-session-registry--grep-last (file &rest args)
  "Return the last line that grep with ARGS prints for FILE, or nil.
Transcripts can be hundreds of megabytes, too large to read in full."
  (car (last (ignore-errors
               (apply #'process-lines-ignore-status "grep" (append args (list "--" file)))))))

(defun claude-code-ide-session-registry--cached (kind file fn)
  "Return FN applied to FILE, cached under KIND until FILE changes."
  (let* ((attributes (file-attributes file))
         (stamp (list kind file
                      (file-attribute-modification-time attributes)
                      (file-attribute-size attributes)))
         (value (gethash stamp claude-code-ide-session-registry--transcript-cache 'none)))
    (when (eq value 'none)
      (setq value (funcall fn file))
      (puthash stamp value claude-code-ide-session-registry--transcript-cache))
    value))

(defun claude-code-ide-session-registry--line-field (line field)
  "Return the string FIELD of the JSON object on LINE, or nil."
  (when line
    (let ((value (alist-get field (ignore-errors
                                    (claude-code-ide-session-registry--json-decode line)))))
      (and (stringp value) value))))

(defun claude-code-ide-session-registry--transcript-title (file)
  "Return the title of the transcript FILE: the /rename title or the summary."
  (claude-code-ide-session-registry--cached
   'title file
   (lambda (file)
     (or (claude-code-ide-session-registry--line-field
          (claude-code-ide-session-registry--grep-last file "-F" "\"type\":\"custom-title\"")
          'customTitle)
         (claude-code-ide-session-registry--line-field
          (claude-code-ide-session-registry--grep-last file "-F" "\"type\":\"summary\"")
          'summary)))))

(defun claude-code-ide-session-registry--transcript-model (file)
  "Return the model of the last assistant message in the transcript FILE."
  (claude-code-ide-session-registry--cached
   'model file
   (lambda (file)
     (let ((match (claude-code-ide-session-registry--grep-last
                   file "-o" "-E" "\"model\":\"claude-[^\"]+\"")))
       (and match
            (string-match "\"model\":\"\\([^\"]+\\)\"" match)
            (match-string 1 match))))))

(defun claude-code-ide-session-registry--title (entry)
  "Return ENTRY's title from its transcript, or nil."
  (when-let* ((file (claude-code-ide-session-registry--transcript-file entry)))
    (claude-code-ide-session-registry--transcript-title file)))

(defun claude-code-ide-session-registry--custom-title (entry)
  "Return the title that the user gave ENTRY's session with /rename or --name.
A resumed session would otherwise get a derived name, and other
sessions address it by its name."
  (when-let* ((file (claude-code-ide-session-registry--transcript-file entry)))
    (claude-code-ide-session-registry--cached
     'custom-title file
     (lambda (file)
       (claude-code-ide-session-registry--line-field
        (claude-code-ide-session-registry--grep-last file "-F" "\"type\":\"custom-title\"")
        'customTitle)))))

(defun claude-code-ide-session-registry--model (entry)
  "Return the model to resume ENTRY with: the transcript's, else the stored one."
  (or (when-let* ((file (claude-code-ide-session-registry--transcript-file entry)))
        (claude-code-ide-session-registry--transcript-model file))
      (claude-code-ide-session-registry-entry-model entry)))

(defun claude-code-ide-session-registry--last-seen (entry)
  "Return the time of ENTRY's last activity, or nil.
That is the transcript's modification time, else the last registry change."
  (if-let* ((file (claude-code-ide-session-registry--transcript-file entry)))
      (file-attribute-modification-time (file-attributes file))
    (when-let* ((seen (claude-code-ide-session-registry-entry-last-seen-at entry)))
      (ignore-errors (encode-time (iso8601-parse seen))))))

;;; Starting entries

(defun claude-code-ide-session-registry--describe (entry)
  "Return a short description of ENTRY for messages."
  (format "%s (%s)"
          (or (claude-code-ide-session-registry-entry-buffer-name entry)
              (claude-code-ide-session-registry-entry-key entry))
          (abbreviate-file-name (claude-code-ide-session-registry-entry-project-dir entry))))

(defun claude-code-ide-session-registry--start-entry (entry prompt)
  "Start ENTRY's conversation again, with PROMPT as its first message.
The instance starts in ENTRY's directory with its instance name, its
flags and its model, resumed by its session id."
  (let ((id (claude-code-ide-session-registry-entry-session-id entry))
        (dir (claude-code-ide-session-registry-entry-project-dir entry)))
    (unless id
      (user-error "No Claude session id is known for %s"
                  (claude-code-ide-session-registry--describe entry)))
    (unless (file-directory-p dir)
      (user-error "Directory %s does not exist" dir))
    (let* ((model (claude-code-ide-session-registry--model entry))
           (title (claude-code-ide-session-registry--custom-title entry))
           (flags (string-trim
                   (concat (claude-code-ide-session-registry-entry-extra-flags entry)
                           (when model (concat " --model " (shell-quote-argument model)))
                           (when title (concat " --name " (shell-quote-argument title))))))
           (claude-code-ide-session-registry--start-key
            (claude-code-ide-session-registry-entry-key entry)))
      (claude-code-ide--start-session
       nil nil
       (claude-code-ide-start-spec-create
        :directory dir
        :instance-name (claude-code-ide-session-registry-entry-instance-name entry)
        :resume-id id
        :extra-flags flags
        :prompt prompt)))))

(defun claude-code-ide-session-registry--label (entry)
  "Return the completion label of ENTRY."
  (replace-regexp-in-string
   "," ";"
   (format "%s  %s  %s  [%s]"
           (or (claude-code-ide-session-registry-entry-buffer-name entry) "?")
           (or (claude-code-ide-session-registry--title entry) "")
           (abbreviate-file-name (claude-code-ide-session-registry-entry-project-dir entry))
           (substring (claude-code-ide-session-registry-entry-key entry) 0 8))))

(defun claude-code-ide-session-registry--candidates (entries)
  "Return an alist of completion labels and ENTRIES."
  (mapcar (lambda (entry) (cons (claude-code-ide-session-registry--label entry) entry))
          entries))

(defun claude-code-ide-session-registry--read-entry (prompt entries)
  "Read one of ENTRIES with PROMPT."
  (unless entries
    (user-error "No matching Claude Code sessions in the registry"))
  (let ((candidates (claude-code-ide-session-registry--candidates entries)))
    (cdr (assoc (completing-read prompt candidates nil t) candidates))))

(defun claude-code-ide-session-registry--sort (entries)
  "Return ENTRIES sorted by start time, oldest first."
  (sort (copy-sequence entries)
        (lambda (a b)
          (string< (or (claude-code-ide-session-registry-entry-started-at a) "")
                   (or (claude-code-ide-session-registry-entry-started-at b) "")))))

;;; Commands

;;;###autoload
(cl-defstruct (claude-code-ide-session-registry--restore-run
               (:constructor claude-code-ide-session-registry--restore-run-create)
               (:copier nil))
  "The progress of one restore."
  (remaining 0 :type integer)
  (started 0 :type integer)
  (failed nil :type list :documentation "Descriptions of the failed starts, newest first."))

(defconst claude-code-ide-session-registry--restore-interval 0.5
  "Seconds between two starts of a restore.")

(defun claude-code-ide-session-registry--start-entries (entries prompt)
  "Start ENTRIES again, each from its own timer, with PROMPT as first message.
An error or a quit in one start does not stop the others, and Emacs
stays usable between the starts.  The last start reports the result."
  (let ((run (claude-code-ide-session-registry--restore-run-create
              :remaining (length entries)))
        (delay 0))
    (dolist (entry entries)
      (run-with-timer
       delay nil
       (lambda ()
         (condition-case err
             (progn
               (claude-code-ide-session-registry--start-entry entry prompt)
               (cl-incf (claude-code-ide-session-registry--restore-run-started run)))
           ((error quit)
            (push (format "%s: %s" (claude-code-ide-session-registry--describe entry)
                          (if (eq (car err) 'quit) "quit" (error-message-string err)))
                  (claude-code-ide-session-registry--restore-run-failed run))))
         (when (zerop (cl-decf (claude-code-ide-session-registry--restore-run-remaining run)))
           (let ((started (claude-code-ide-session-registry--restore-run-started run))
                 (failed (claude-code-ide-session-registry--restore-run-failed run)))
             (message "Restored %d Claude Code session%s%s"
                      started (if (= started 1) "" "s")
                      (if failed
                          (format "; failed: %s" (string-join (reverse failed) "; "))
                        ""))))))
      (setq delay (+ delay claude-code-ide-session-registry--restore-interval)))))

(defun claude-code-ide-restore-sessions (&optional pick)
  "Start again every Claude Code instance that ran when the last Emacs ended.
These are the `open' entries of `claude-code-ide-state-file' of an
Emacs that crashed, and the entries that the last Emacs closed at its
exit; see `claude-code-ide-session-registry--restorable-p'.  Sessions
that run again already are skipped.  Each instance resumes its
conversation in its directory, with its instance name, flags, model
and title, and gets `claude-code-ide-restore-note' as its first
message.  The instances start one after the other, each from its own
timer, so that one failure or quit does not stop the rest.  With a
prefix argument PICK, choose which ones."
  (interactive "P")
  (let ((entries (claude-code-ide-session-registry--restorable-entries)))
    (if (null entries)
        (message "No Claude Code sessions to restore")
      (when pick
        (let ((candidates (claude-code-ide-session-registry--candidates entries)))
          (setq entries (mapcar (lambda (label) (cdr (assoc label candidates)))
                                (completing-read-multiple "Restore sessions: "
                                                          candidates nil t)))))
      (message "Restoring %d Claude Code session%s"
               (length entries) (if (cdr entries) "s" ""))
      (claude-code-ide-session-registry--start-entries entries claude-code-ide-restore-note))))

;;;###autoload
(defun claude-code-ide-reopen-session (entry)
  "Start the registry ENTRY again and resume its conversation.
Interactively, choose among the entries that are not archived and do
not run in this Emacs."
  (interactive
   (list (claude-code-ide-session-registry--read-entry
          "Reopen Claude session: "
          (cl-remove-if-not #'claude-code-ide-session-registry--reopenable-p
                            (claude-code-ide-session-registry--entries)))))
  (claude-code-ide-session-registry--start-entry entry nil))

;;;###autoload
(defun claude-code-ide-archive-session (entry)
  "Archive the registry ENTRY: it is done and is never reopened."
  (interactive
   (list (claude-code-ide-session-registry--read-entry
          "Archive Claude session: "
          (cl-remove-if (lambda (entry)
                          (eq (claude-code-ide-session-registry-entry-state entry) 'archived))
                        (claude-code-ide-session-registry--entries)))))
  (let ((key (claude-code-ide-session-registry-entry-key entry)))
    (claude-code-ide-session-registry--submit
     "archived" (claude-code-ide-session-registry--change-archived key))
    (ignore-errors (delete-file (claude-code-ide-session-registry--report-file key)))
    (message "Archived %s" (claude-code-ide-session-registry--describe entry))))

;;;###autoload
(defun claude-code-ide-forget-session (entry)
  "Remove the registry ENTRY.  The transcript stays."
  (interactive
   (list (claude-code-ide-session-registry--read-entry
          "Forget Claude session: " (claude-code-ide-session-registry--entries))))
  (when (or (not (called-interactively-p 'any))
            (yes-or-no-p (format "Remove %s from the session registry? "
                                 (claude-code-ide-session-registry--describe entry))))
    (let ((key (claude-code-ide-session-registry-entry-key entry)))
      (claude-code-ide-session-registry--submit
       "forgotten" (claude-code-ide-session-registry--change-forgotten key))
      (ignore-errors (delete-file (claude-code-ide-session-registry--report-file key)))
      (message "Removed %s from the session registry"
               (claude-code-ide-session-registry--describe entry)))))

;;; List buffer

(defvar claude-code-ide-session-registry-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-code-ide-session-registry-list-open)
    (define-key map (kbd "a") #'claude-code-ide-session-registry-list-archive)
    (define-key map (kbd "d") #'claude-code-ide-session-registry-list-forget)
    (define-key map (kbd "R") #'claude-code-ide-restore-sessions)
    map)
  "Keymap of `claude-code-ide-session-registry-list-mode'.")

(define-derived-mode claude-code-ide-session-registry-list-mode tabulated-list-mode
  "Claude Sessions"
  "List of the Claude Code session registry.
State \"live\" means the session runs in this Emacs; \"open\" means it
ran when its Emacs crashed.

\\{claude-code-ide-session-registry-list-mode-map}"
  (setq tabulated-list-format [("State" 8 t)
                               ("Buffer" 34 t)
                               ("Directory" 36 t)
                               ("Last seen" 16 t)
                               ("Title" 0 nil)])
  (setq tabulated-list-sort-key '("Last seen" . t))
  (add-hook 'tabulated-list-revert-hook #'claude-code-ide-session-registry--list-refresh nil t)
  (tabulated-list-init-header))

(defun claude-code-ide-session-registry--list-refresh ()
  "Fill `tabulated-list-entries' from the registry."
  (setq tabulated-list-entries
        (mapcar (lambda (entry)
                  (let ((seen (claude-code-ide-session-registry--last-seen entry)))
                    (list entry
                          (vector (if (claude-code-ide-session-registry--live-p entry)
                                      "live"
                                    (symbol-name (claude-code-ide-session-registry-entry-state entry)))
                                  (or (claude-code-ide-session-registry-entry-buffer-name entry) "")
                                  (abbreviate-file-name
                                   (claude-code-ide-session-registry-entry-project-dir entry))
                                  (if seen (format-time-string "%Y-%m-%d %H:%M" seen) "")
                                  (or (claude-code-ide-session-registry--title entry) "")))))
                (claude-code-ide-session-registry--entries))))

;;;###autoload
(defun claude-code-ide-list-session-registry ()
  "Show the Claude Code session registry."
  (interactive)
  (let ((buffer (get-buffer-create "*Claude Code Sessions*")))
    (with-current-buffer buffer
      (claude-code-ide-session-registry-list-mode)
      (claude-code-ide-session-registry--list-refresh)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

(defun claude-code-ide-session-registry--list-entry ()
  "Return the entry at point."
  (or (tabulated-list-get-id) (user-error "No session on this line")))

(defun claude-code-ide-session-registry-list-open ()
  "Switch to the session at point, or reopen it when it does not run."
  (interactive)
  (let* ((entry (claude-code-ide-session-registry--list-entry))
         (key (claude-code-ide-session-registry-entry-key entry))
         (session (cl-loop for session being the hash-keys
                           of claude-code-ide-session-registry--keys
                           using (hash-values session-key)
                           when (equal session-key key) return session)))
    (if (and session (buffer-live-p (claude-code-ide-mcp-session-buffer session)))
        (pop-to-buffer (claude-code-ide-mcp-session-buffer session))
      (claude-code-ide-reopen-session entry))))

(defun claude-code-ide-session-registry-list-archive ()
  "Archive the session at point."
  (interactive)
  (claude-code-ide-archive-session (claude-code-ide-session-registry--list-entry))
  (revert-buffer))

(defun claude-code-ide-session-registry-list-forget ()
  "Remove the session at point from the registry."
  (interactive)
  (let ((entry (claude-code-ide-session-registry--list-entry)))
    (when (yes-or-no-p (format "Remove %s from the session registry? "
                               (claude-code-ide-session-registry--describe entry)))
      (claude-code-ide-forget-session entry)
      (revert-buffer))))

;;; Mode

;;;###autoload
(define-minor-mode claude-code-ide-session-registry-mode
  "Record Claude Code sessions in `claude-code-ide-state-file'.
Each instance that Emacs starts is recorded with its CLI session id,
directory, names and flags.  A normal exit of Emacs marks them
`closed'; after a crash they stay `open', and
`claude-code-ide-restore-sessions' starts them again."
  :global t
  :group 'claude-code-ide
  (if claude-code-ide-session-registry-mode
      (progn
        (add-hook 'claude-code-ide-cli-args-functions #'claude-code-ide-session-registry--cli-args)
        (add-hook 'claude-code-ide-session-start-functions #'claude-code-ide-session-registry--on-start)
        (add-hook 'claude-code-ide-session-end-functions #'claude-code-ide-session-registry--on-end)
        (add-hook 'claude-code-ide-session-rename-functions #'claude-code-ide-session-registry--on-rename)
        ;; Before the package's own cleanup, which ends every session
        (add-hook 'kill-emacs-hook #'claude-code-ide-session-registry--on-kill-emacs -50)
        (setq claude-code-ide-session-registry--exiting nil)
        (claude-code-ide-session-registry--start-watch)
        (require 'claude-code-ide-session-status)
        (claude-code-ide-session-status--set-tracking t)
        (let ((adopted (claude-code-ide-session-registry--adopt-running-sessions)))
          (when (> adopted 0)
            (message "%d running Claude Code session%s have no status hooks: M-x claude-code-ide-session-registry-track-sessions restarts them with hooks"
                     adopted (if (= adopted 1) "" "s"))))
        (let ((count (length (ignore-errors (claude-code-ide-session-registry--restorable-entries)))))
          (when (> count 0)
            (message "%d Claude Code session%s can be restored: M-x claude-code-ide-restore-sessions"
                     count (if (= count 1) "" "s")))))
    (remove-hook 'claude-code-ide-cli-args-functions #'claude-code-ide-session-registry--cli-args)
    (remove-hook 'claude-code-ide-session-start-functions #'claude-code-ide-session-registry--on-start)
    (remove-hook 'claude-code-ide-session-end-functions #'claude-code-ide-session-registry--on-end)
    (remove-hook 'claude-code-ide-session-rename-functions #'claude-code-ide-session-registry--on-rename)
    (remove-hook 'kill-emacs-hook #'claude-code-ide-session-registry--on-kill-emacs)
    (claude-code-ide-session-registry--stop-watch)
    (when (timerp claude-code-ide-session-registry--adopt-timer)
      (cancel-timer claude-code-ide-session-registry--adopt-timer)
      (setq claude-code-ide-session-registry--adopt-timer nil))
    (when (fboundp 'claude-code-ide-session-status--set-tracking)
      (claude-code-ide-session-status--set-tracking nil))))

(provide 'claude-code-ide-session-registry)

;;; claude-code-ide-session-registry.el ends here
