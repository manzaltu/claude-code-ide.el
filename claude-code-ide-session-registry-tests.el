;;; claude-code-ide-session-registry-tests.el --- Tests for the session registry  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `claude-code-ide-session-registry-mode' and for the start
;; spec and lifecycle hooks of the core that it uses.  The file is
;; loaded by claude-code-ide-tests.el, after its mocks, and uses its
;; helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'claude-code-ide)
(require 'claude-code-ide-session-registry)

(declare-function claude-code-ide-tests--make-session "claude-code-ide-tests")
(declare-function claude-code-ide-tests--clear-processes "claude-code-ide-tests")

(defmacro claude-code-ide-tests--with-registry (&rest body)
  "Run BODY with an empty session registry in a temporary directory.
BODY can refer to that directory as `registry-dir'."
  (declare (indent 0))
  `(let* ((registry-dir (make-temp-file "cci-registry-" t))
          (claude-code-ide-state-file (expand-file-name "state.json" registry-dir))
          (claude-code-ide-session-registry-report-directory nil)
          (claude-code-ide-session-registry--queue nil)
          (claude-code-ide-session-registry--draining nil)
          (claude-code-ide-session-registry--retry-timer nil)
          (claude-code-ide-session-registry--loaded-from-backup nil)
          (claude-code-ide-session-registry--exiting nil)
          (claude-code-ide-session-registry--start-key nil)
          (claude-code-ide-session-registry--keys
           (make-hash-table :test 'eq :weakness 'key))
          (claude-code-ide-session-registry--started
           (make-hash-table :test 'eq :weakness 'key)))
     (unwind-protect
         (progn ,@body)
       (when (timerp claude-code-ide-session-registry--retry-timer)
         (cancel-timer claude-code-ide-session-registry--retry-timer))
       (delete-directory registry-dir t))))

(defun claude-code-ide-tests--registry-entry (key &rest overrides)
  "Return an open registry entry KEY of this Emacs, changed by OVERRIDES."
  (apply #'claude-code-ide-session-registry-entry-create
         (append overrides
                 (list :key key
                       :session-id key
                       :project-dir "/tmp/registry-project/"
                       :state 'open
                       :started-at "2026-11-03T10:00:00Z"
                       :host (system-name)
                       :emacs-pid (emacs-pid)))))

(defun claude-code-ide-tests--registry-add (key)
  "Return a change that appends a new entry KEY."
  (lambda (entries)
    (append entries (list (claude-code-ide-tests--registry-entry key)))))

(defun claude-code-ide-tests--registry-file-keys (&optional file)
  "Return the entry keys in FILE, by default the state file."
  (mapcar #'claude-code-ide-session-registry-entry-key
          (claude-code-ide-session-registry--read-file
           (or file (claude-code-ide-session-registry--state-file)))))

(defun claude-code-ide-tests--registry-get (key)
  "Return the entry KEY of the state file."
  (claude-code-ide-session-registry--find (claude-code-ide-session-registry--load) key))

(defun claude-code-ide-tests--registry-spec (&rest overrides)
  "Return a start spec in the registry test project, changed by OVERRIDES."
  (apply #'claude-code-ide-start-spec-create
         (append overrides
                 (list :directory "/tmp/registry-project/"
                       :extra-flags ""))))

(ert-deftest claude-code-ide-test-registry-queue-reentrant-order ()
  "Changes submitted while the queue drains are written after, in order."
  (claude-code-ide-tests--with-registry
    (let ((real-write (symbol-function 'claude-code-ide-session-registry--write-file))
          (nested nil)
          (writes 0))
      (cl-letf (((symbol-function 'claude-code-ide-session-registry--write-file)
                 (lambda (entries)
                   (cl-incf writes)
                   ;; A timer or hook that fires during the write
                   (unless nested
                     (setq nested t)
                     (claude-code-ide-session-registry--submit
                      "b" (claude-code-ide-tests--registry-add "b"))
                     (claude-code-ide-session-registry--submit
                      "c" (claude-code-ide-tests--registry-add "c")))
                   (funcall real-write entries))))
        (claude-code-ide-session-registry--submit "a" (claude-code-ide-tests--registry-add "a"))
        (should (equal (claude-code-ide-tests--registry-file-keys) '("a" "b" "c")))
        ;; The nested changes were queued, not written inside the first pass
        (should (= writes 2))
        (should-not claude-code-ide-session-registry--queue)
        (should-not claude-code-ide-session-registry--draining)))))

(ert-deftest claude-code-ide-test-registry-queue-reentrant-during-lock-wait ()
  "A change submitted while the drain waits for the lock is in the same write."
  (claude-code-ide-tests--with-registry
    (let ((lock (claude-code-ide-session-registry--lock-directory))
          (waits 0))
      (make-directory lock)
      (cl-letf (((symbol-function 'sleep-for)
                 (lambda (&rest _)
                   (cl-incf waits)
                   (claude-code-ide-session-registry--submit
                    "b" (claude-code-ide-tests--registry-add "b"))
                   ;; The other writer finishes
                   (delete-directory lock))))
        (claude-code-ide-session-registry--submit "a" (claude-code-ide-tests--registry-add "a")))
      (should (= waits 1))
      (should (equal (claude-code-ide-tests--registry-file-keys) '("a" "b")))
      (should-not (file-exists-p lock)))))

(ert-deftest claude-code-ide-test-registry-lock-blocks-then-stale-lock-breaks ()
  "A held lock keeps the change queued; a stale lock is removed."
  (claude-code-ide-tests--with-registry
    (let ((lock (claude-code-ide-session-registry--lock-directory))
          (claude-code-ide-session-registry-lock-wait 0.1)
          (retries 0))
      (make-directory lock)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _) (cl-incf retries) nil))
                ((symbol-function 'message) #'ignore))
        (claude-code-ide-session-registry--submit "a" (claude-code-ide-tests--registry-add "a"))
        (should-not (file-exists-p claude-code-ide-state-file))
        (should (= (length claude-code-ide-session-registry--queue) 1))
        (should (= retries 1))
        ;; The other writer died long ago
        (set-file-times lock (time-subtract nil 3600))
        (claude-code-ide-session-registry--drain)
        (should (equal (claude-code-ide-tests--registry-file-keys) '("a")))
        (should-not claude-code-ide-session-registry--queue)
        (should-not (file-exists-p lock))))))

(ert-deftest claude-code-ide-test-registry-atomic-write ()
  "A failed write leaves the old file whole; a good one keeps the old as .bak."
  (claude-code-ide-tests--with-registry
    (claude-code-ide-session-registry--submit "a" (claude-code-ide-tests--registry-add "a"))
    (let ((real-rename (symbol-function 'rename-file))
          (file (claude-code-ide-session-registry--state-file)))
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (from to &optional ok)
                   (if (equal (expand-file-name to) file)
                       (error "Simulated crash before the rename")
                     (funcall real-rename from to ok))))
                ((symbol-function 'display-warning) #'ignore))
        (claude-code-ide-session-registry--submit "b" (claude-code-ide-tests--registry-add "b")))
      ;; The old file is complete, no temporary file is left, the change waits
      (should (equal (claude-code-ide-tests--registry-file-keys) '("a")))
      (should (equal (directory-files registry-dir nil "tmp") nil))
      (should (= (length claude-code-ide-session-registry--queue) 1))
      ;; The next drain writes it and keeps the old version as backup
      (claude-code-ide-session-registry--drain)
      (should (equal (claude-code-ide-tests--registry-file-keys) '("a" "b")))
      (should (equal (claude-code-ide-tests--registry-file-keys
                      (claude-code-ide-session-registry--backup-file))
                     '("a"))))))

(ert-deftest claude-code-ide-test-registry-backup-fallback ()
  "An unreadable state file falls back to .bak and is kept aside."
  (claude-code-ide-tests--with-registry
    (claude-code-ide-session-registry--submit "a" (claude-code-ide-tests--registry-add "a"))
    (claude-code-ide-session-registry--submit "b" (claude-code-ide-tests--registry-add "b"))
    (with-temp-file claude-code-ide-state-file
      (insert "{\"version\": 1, \"sessions\": [{\"key\""))
    (let ((warnings '()))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        (should (equal (mapcar #'claude-code-ide-session-registry-entry-key
                               (claude-code-ide-session-registry--entries))
                       '("a")))
        (should (string-match-p "using .*state\\.json\\.bak" (car warnings)))
        (claude-code-ide-session-registry--submit "c" (claude-code-ide-tests--registry-add "c"))))
    (should (equal (claude-code-ide-tests--registry-file-keys) '("a" "c")))
    ;; The good backup survived, and the unreadable file was kept
    (should (equal (claude-code-ide-tests--registry-file-keys
                    (claude-code-ide-session-registry--backup-file))
                   '("a")))
    (should (= 1 (length (directory-files registry-dir nil "\\.corrupt-"))))))

(ert-deftest claude-code-ide-test-registry-state-transitions ()
  "Start, kill, reopen, normal end, Emacs exit, rename, archive, forget."
  (claude-code-ide-tests--with-registry
    (let* ((buffer (generate-new-buffer "*claude-code[registry-project]*"))
           (session (make-claude-code-ide-mcp-session :buffer buffer))
           (args (claude-code-ide-session-registry--cli-args
                  session
                  (claude-code-ide-tests--registry-spec
                   :extra-flags "--model opus --permission-mode plan")))
           (key (gethash session claude-code-ide-session-registry--keys)))
      (unwind-protect
          (progn
            ;; A new conversation gets its id from Emacs, and the report hook
            (should (equal (cl-subseq args 0 2) (list "--session-id" key)))
            (should (equal (nth 2 args) "--settings"))
            (should (string-match-p (regexp-quote (concat key ".json")) (nth 3 args)))
            (claude-code-ide-session-registry--on-start
             session (claude-code-ide-tests--registry-spec
                      :extra-flags "--model opus --permission-mode plan"))
            (let ((entry (claude-code-ide-tests--registry-get key)))
              (should (eq (claude-code-ide-session-registry-entry-state entry) 'open))
              (should (equal (claude-code-ide-session-registry-entry-session-id entry) key))
              (should (equal (claude-code-ide-session-registry-entry-model entry) "opus"))
              (should (equal (claude-code-ide-session-registry-entry-extra-flags entry)
                             "--permission-mode plan"))
              (should (equal (claude-code-ide-session-registry-entry-permission-mode entry)
                             "plan"))
              (should (equal (claude-code-ide-session-registry-entry-buffer-name entry)
                             (buffer-name buffer))))

            (claude-code-ide-session-registry--on-end session 'killed)
            (let ((entry (claude-code-ide-tests--registry-get key)))
              (should (eq (claude-code-ide-session-registry-entry-state entry) 'closed))
              (should (equal (claude-code-ide-session-registry-entry-closed-by entry) "killed")))

            ;; Reopen: same key, resumed by id, so no --session-id
            (let* ((first-start (claude-code-ide-session-registry-entry-started-at
                                 (claude-code-ide-tests--registry-get key)))
                   (session2 (make-claude-code-ide-mcp-session :buffer buffer))
                   (spec (claude-code-ide-tests--registry-spec :resume-id key))
                   (args2 (let ((claude-code-ide-session-registry--start-key key))
                            (claude-code-ide-session-registry--cli-args session2 spec))))
              (should-not (member "--session-id" args2))
              (claude-code-ide-session-registry--on-start session2 spec)
              (let ((entry (claude-code-ide-tests--registry-get key)))
                (should (eq (claude-code-ide-session-registry-entry-state entry) 'open))
                (should-not (claude-code-ide-session-registry-entry-closed-by entry))
                (should (equal (claude-code-ide-session-registry-entry-started-at entry)
                               first-start)))
              (claude-code-ide-session-registry--on-end session2 'exited)
              (should (equal (claude-code-ide-session-registry-entry-closed-by
                              (claude-code-ide-tests--registry-get key))
                             "exited")))

            ;; Emacs exits normally while the session runs
            (let* ((session3 (make-claude-code-ide-mcp-session :buffer buffer))
                   (spec (claude-code-ide-tests--registry-spec :resume-id key)))
              (let ((claude-code-ide-session-registry--start-key key))
                (claude-code-ide-session-registry--cli-args session3 spec))
              (claude-code-ide-session-registry--on-start session3 spec)
              (setf (claude-code-ide-mcp-session-instance-name session3) "refactor")
              (claude-code-ide-session-registry--on-rename session3)
              (should (equal (claude-code-ide-session-registry-entry-instance-name
                              (claude-code-ide-tests--registry-get key))
                             "refactor"))
              (claude-code-ide-session-registry--on-kill-emacs)
              (should (equal (claude-code-ide-session-registry-entry-closed-by
                              (claude-code-ide-tests--registry-get key))
                             "emacs-exit"))
              ;; The package cleanup that follows does not change it
              (claude-code-ide-session-registry--on-end session3 'killed)
              (should (equal (claude-code-ide-session-registry-entry-closed-by
                              (claude-code-ide-tests--registry-get key))
                             "emacs-exit")))

            (claude-code-ide-archive-session (claude-code-ide-tests--registry-get key))
            (should (eq (claude-code-ide-session-registry-entry-state
                         (claude-code-ide-tests--registry-get key))
                        'archived))
            (claude-code-ide-forget-session (claude-code-ide-tests--registry-get key))
            (should-not (claude-code-ide-tests--registry-get key)))
        (kill-buffer buffer)))))

(ert-deftest claude-code-ide-test-registry-failed-restore-stays-open ()
  "A restore that fails before or during the start leaves the entry open."
  (claude-code-ide-tests--with-registry
    (claude-code-ide-session-registry--submit
     "setup" (lambda (_) (list (claude-code-ide-tests--registry-entry "crashed"))))
    (let* ((buffer (generate-new-buffer "*claude-failed*"))
           (failed (make-claude-code-ide-mcp-session :buffer buffer))
           (no-buffer (make-claude-code-ide-mcp-session))
           (spec (claude-code-ide-tests--registry-spec :resume-id "crashed")))
      (unwind-protect
          (let ((claude-code-ide-session-registry--start-key "crashed"))
            ;; The CLI exits during the startup delay: cleanup, no start record
            (claude-code-ide-session-registry--cli-args failed spec)
            (claude-code-ide-session-registry--on-end failed 'exited)
            ;; The terminal never came up: no buffer, no cleanup at all
            (claude-code-ide-session-registry--cli-args no-buffer spec)
            (should-not (claude-code-ide-session-registry--live-keys))
            (let ((entry (claude-code-ide-tests--registry-get "crashed")))
              (should (eq (claude-code-ide-session-registry-entry-state entry) 'open))
              (should (claude-code-ide-session-registry--restorable-p entry))))
        (kill-buffer buffer)))))

(ert-deftest claude-code-ide-test-registry-keeps-user-settings ()
  "Flags with their own --settings get no second --settings."
  (claude-code-ide-tests--with-registry
    (let ((args (claude-code-ide-session-registry--cli-args
                 (make-claude-code-ide-mcp-session)
                 (claude-code-ide-tests--registry-spec
                  :extra-flags "--settings /home/me/settings.json"))))
      (should (member "--session-id" args))
      (should-not (member "--settings" args)))))

(ert-deftest claude-code-ide-test-registry-continue-gets-id-from-report ()
  "A continued session gets no --session-id; the hook report gives its id."
  (claude-code-ide-tests--with-registry
    (let* ((session (make-claude-code-ide-mcp-session))
           (spec (claude-code-ide-tests--registry-spec :continue t))
           (args (claude-code-ide-session-registry--cli-args session spec))
           (key (gethash session claude-code-ide-session-registry--keys)))
      (should-not (member "--session-id" args))
      (claude-code-ide-session-registry--on-start session spec)
      (should-not (claude-code-ide-session-registry-entry-session-id
                   (claude-code-ide-tests--registry-get key)))
      ;; What the SessionStart hook writes
      (with-temp-file (claude-code-ide-session-registry--report-file key)
        (insert "{\"session_id\":\"cli-id-1\",\"transcript_path\":\"/x/cli-id-1.jsonl\",\"source\":\"startup\"}"))
      (claude-code-ide-session-registry--submit "report" #'identity)
      (let ((entry (claude-code-ide-tests--registry-get key)))
        (should (equal (claude-code-ide-session-registry-entry-session-id entry) "cli-id-1"))
        (should (equal (claude-code-ide-session-registry-entry-transcript-path entry)
                       "/x/cli-id-1.jsonl"))))))

(ert-deftest claude-code-ide-test-registry-hook-survives-command-split ()
  "The --settings argument reaches the CLI whole through both backend paths."
  (claude-code-ide-tests--with-registry
    (cl-letf (((symbol-function 'claude-code-ide-mcp-server-ensure-server) #'ignore))
      (let* ((session (make-claude-code-ide-mcp-session))
             (args (claude-code-ide-session-registry--cli-args
                    session (claude-code-ide-tests--registry-spec)))
             (key (gethash session claude-code-ide-session-registry--keys))
             (claude-code-ide-cli-extra-flags "")
             (parts (claude-code-ide--split-command-line
                     (claude-code-ide--build-claude-command nil nil "sid" args)))
             (settings (claude-code-ide-session-registry--json-decode
                        (cadr (member "--settings" parts))))
             (hook (car (alist-get 'hooks (car (alist-get 'SessionStart
                                                          (alist-get 'hooks settings)))))))
        (should (equal (car parts) claude-code-ide-cli-path))
        (should (equal (cadr (member "--session-id" parts)) key))
        (should (equal (alist-get 'command hook)
                       (concat "cat > " (shell-quote-argument
                                         (claude-code-ide-session-registry--report-file key)))))))))

(ert-deftest claude-code-ide-test-split-command-line ()
  "The backend command splitter keeps ;, & and |, and unquotes like a shell."
  (should (equal (claude-code-ide--split-command-line
                  (concat "claude --name "
                          (shell-quote-argument "Add calls; right now they queue")))
                 '("claude" "--name" "Add calls; right now they queue")))
  (should (equal (claude-code-ide--split-command-line "a 'b & c|d' \"e \\\"f\\\" $g\" h\\ i")
                 '("a" "b & c|d" "e \"f\" $g" "h i")))
  (should (equal (claude-code-ide--split-command-line "x '' \"\" y")
                 '("x" "" "" "y")))
  (should (equal (claude-code-ide--split-command-line "  ") '()))
  (should (equal (claude-code-ide--parse-command-string "claude -p 'a;b'")
                 '("claude" "-p" "a;b"))))

(ert-deftest claude-code-ide-test-registry-restore-title-with-separators ()
  "A restore passes a title with ;, & or | whole to the CLI."
  (let* ((claude-code-ide-cli-path "claude")
         (claude-code-ide-cli-extra-flags
          (concat "--model opus --name "
                  (shell-quote-argument "Add support for parallel calls; they queue & wait | now")))
         (parts (cdr (claude-code-ide--parse-command-string
                      (concat claude-code-ide-cli-path " " claude-code-ide-cli-extra-flags)))))
    (should (equal (cadr (member "--name" parts))
                   "Add support for parallel calls; they queue & wait | now"))
    (should (equal (cadr (member "--model" parts)) "opus"))))

(ert-deftest claude-code-ide-test-registry-parse-flags ()
  "Model and session flags are split off; other flags stay, quoted."
  (let ((flags (claude-code-ide-session-registry--parse-flags
                "--model opus --resume abc --append-system-prompt 'two words' --dangerously-skip-permissions")))
    (should (equal (claude-code-ide-session-registry--flags-model flags) "opus"))
    (should (equal (claude-code-ide-session-registry--flags-session-id flags) "abc"))
    (should (claude-code-ide-session-registry--flags-sets-session flags))
    (should (equal (claude-code-ide-session-registry--flags-permission-mode flags)
                   "bypassPermissions"))
    (should (equal (claude-code-ide--split-command-line (claude-code-ide-session-registry--flags-rest flags))
                   '("--append-system-prompt" "two words" "--dangerously-skip-permissions"))))
  ;; A restore sets the name itself, so a stored name is dropped
  (should (equal (claude-code-ide-session-registry--flags-rest
                  (claude-code-ide-session-registry--parse-flags "--name 'Old title' --verbose -n x"))
                 "--verbose"))
  (let ((flags (claude-code-ide-session-registry--parse-flags "-r --fork-session --model=haiku")))
    (should (equal (claude-code-ide-session-registry--flags-model flags) "haiku"))
    (should-not (claude-code-ide-session-registry--flags-session-id flags))
    (should (equal (claude-code-ide-session-registry--flags-rest flags) ""))))

(ert-deftest claude-code-ide-test-registry-restore ()
  "Restore starts only crashed sessions of this host, with their stored setup."
  (claude-code-ide-tests--with-registry
    (let* ((config-dir (expand-file-name "claude" registry-dir))
           (project (file-name-as-directory (expand-file-name "proj" registry-dir)))
           (transcript-dir (expand-file-name
                            (claude-code-ide-session-registry--project-slug project)
                            (expand-file-name "projects" config-dir)))
           (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" config-dir)
                                      process-environment))
           (claude-code-ide-restore-note "Resumed after a crash.")
           (starts '())
           (timers '()))
      (make-directory project t)
      (make-directory transcript-dir t)
      (with-temp-file (expand-file-name "id-crashed.jsonl" transcript-dir)
        (insert "{\"type\":\"custom-title\",\"customTitle\":\"DEAL-1/fix\"}\n"
                "{\"type\":\"user\",\"cwd\":\"x\"}\n"
                "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-old-1\"}}\n"
                "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-new-2\"}}\n"))
      (claude-code-ide-session-registry--submit
       "setup"
       (lambda (_)
         (list (claude-code-ide-tests--registry-entry
                "crashed" :session-id "id-crashed" :project-dir project
                :instance-name "refactor" :model "opus"
                :extra-flags "--permission-mode plan" :emacs-pid 999999999
                :started-at "2026-11-03T09:00:00Z")
               (claude-code-ide-tests--registry-entry
                "other-emacs" :project-dir project :emacs-pid 4242)
               (claude-code-ide-tests--registry-entry
                "closed" :project-dir project :state 'closed)
               (claude-code-ide-tests--registry-entry
                "other-host" :project-dir project :host "elsewhere"))))
      (cl-letf (((symbol-function 'process-attributes)
                 (lambda (pid) (when (eql pid 4242) '((comm . "Emacs")))))
                ((symbol-function 'claude-code-ide--start-session)
                 (lambda (continue resume spec)
                   (push (list continue resume spec claude-code-ide-session-registry--start-key)
                         starts)))
                ;; Each start runs from its own timer: collect them, then run them
                ((symbol-function 'run-with-timer)
                 (lambda (_secs _repeat fn &rest args)
                   (push (cons fn args) timers))))
        (claude-code-ide-restore-sessions)
        (dolist (timer (reverse timers))
          (apply (car timer) (cdr timer))))
      (should (= (length starts) 1))
      (pcase-let ((`(,continue ,resume ,spec ,key) (car starts)))
        (should-not continue)
        (should-not resume)
        (should (equal key "crashed"))
        (should (equal (claude-code-ide-start-spec-directory spec) project))
        (should (equal (claude-code-ide-start-spec-instance-name spec) "refactor"))
        (should (equal (claude-code-ide-start-spec-resume-id spec) "id-crashed"))
        ;; The transcript's last model wins over the stored --model
        (should (equal (claude-code-ide-start-spec-extra-flags spec)
                       "--permission-mode plan --model claude-new-2 --name DEAL-1/fix"))
        (should (equal (claude-code-ide-start-spec-prompt spec) "Resumed after a crash."))))))

(ert-deftest claude-code-ide-test-registry-transcript-title ()
  "The title is the last /rename title of the transcript."
  (claude-code-ide-tests--with-registry
    (let ((file (expand-file-name "t.jsonl" registry-dir)))
      (with-temp-file file
        (insert "{\"type\":\"custom-title\",\"customTitle\":\"first\"}\n"
                "{\"type\":\"user\",\"message\":{\"content\":\"\\\"type\\\":\\\"custom-title\\\"\"}}\n"
                "{\"type\":\"custom-title\",\"customTitle\":\"second, with \\\"quotes\\\"\"}\n"))
      (should (equal (claude-code-ide-session-registry--transcript-title file)
                     "second, with \"quotes\"")))))

(ert-deftest claude-code-ide-test-build-command-extra-args-and-prompt ()
  "Extra arguments are quoted; the prompt comes last, after --."
  (cl-letf (((symbol-function 'claude-code-ide-mcp-server-ensure-server) (lambda () t))
            ((symbol-function 'claude-code-ide-mcp-server-get-config)
             (lambda (_) '((mcpServers . ((emacs-tools . ((type . "http"))))))))
            ((symbol-function 'claude-code-ide-mcp-server-get-tool-names)
             (lambda (_) '("mcp__emacs-tools__a"))))
    (let* ((claude-code-ide-cli-path "claude")
           (claude-code-ide-cli-extra-flags "")
           (claude-code-ide-mcp-allowed-tools 'auto)
           (command (claude-code-ide--build-claude-command
                     nil nil "sid" (list "--settings" "{\"a\": 1}") "hello there"))
           (parts (claude-code-ide--split-command-line command)))
      (should (equal (last parts 2) '("--" "hello there")))
      (should (equal (cadr (member "--settings" parts)) "{\"a\": 1}"))
      (should (< (cl-position "--settings" parts :test #'equal)
                 (cl-position "--allowedTools" parts :test #'equal))))))

(ert-deftest claude-code-ide-test-start-session-with-spec ()
  "A spec starts without prompts and passes its id, args and prompt."
  (claude-code-ide-tests--clear-processes)
  (let* ((buffer (generate-new-buffer "*test-spec*"))
         (process (start-process "mock-claude" nil "sleep" "30"))
         (terminal-args nil)
         (started nil)
         (claude-code-ide-cli-args-functions
          (list (lambda (_session _spec) (list "--hooked"))))
         (claude-code-ide-session-start-functions
          (list (lambda (session spec) (setq started (cons session spec))))))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-code-ide--ensure-cli) (lambda () t))
                  ((symbol-function 'claude-code-ide--terminal-ensure-backend) #'ignore)
                  ((symbol-function 'claude-code-ide--read-instance-name)
                   (lambda (&rest _) (error "Must not prompt")))
                  ((symbol-function 'claude-code-ide-mcp-create-session)
                   (lambda (dir session-id &optional instance-name)
                     (claude-code-ide-tests--make-session
                      dir :session-id session-id :instance-name instance-name :port 12345)))
                  ((symbol-function 'claude-code-ide--create-terminal-session)
                   (lambda (&rest args) (setq terminal-args args) (cons buffer process)))
                  ((symbol-function 'claude-code-ide-mcp-server-session-started) #'ignore)
                  ((symbol-function 'claude-code-ide-mcp-server-update-session-buffer) #'ignore)
                  ((symbol-function 'claude-code-ide--setup-terminal-keybindings) #'ignore)
                  ((symbol-function 'claude-code-ide--display-buffer-in-side-window) #'ignore)
                  ((symbol-function 'claude-code-ide--maybe-install-global-advice) #'ignore)
                  ((symbol-function 'run-with-timer) #'ignore)
                  ((symbol-function 'sleep-for) #'ignore))
          (let ((claude-code-ide-terminal-backend 'ghostel)
                (claude-code-ide-cli-extra-flags "--model global"))
            (claude-code-ide--start-session
             nil nil (claude-code-ide-start-spec-create
                      :directory "/tmp/spec-project/" :instance-name "refactor"
                      :resume-id "cli-id" :extra-flags "--model opus" :prompt "note"))
            (should (equal (nth 1 terminal-args) "/tmp/spec-project/"))
            (should (equal (nthcdr 6 terminal-args)
                           '(("--resume" "cli-id" "--hooked") "note")))
            (let ((session (car started))
                  (spec (cdr started)))
              (should (equal (claude-code-ide-mcp-session-instance-name session) "refactor"))
              (should (equal (claude-code-ide-start-spec-extra-flags spec) "--model opus"))
              (should (equal (claude-code-ide-start-spec-resume-id spec) "cli-id")))))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (let ((kill-buffer-hook nil))
          (kill-buffer buffer)))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-free-instance-name ()
  "A stored name is kept when free and replaced by an auto name when taken."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (progn
        (claude-code-ide-tests--make-session "/tmp/free-name/" :instance-name "refactor")
        (should (equal (claude-code-ide--free-instance-name "/tmp/free-name/" "docs") "docs"))
        (should-not (claude-code-ide--free-instance-name "/tmp/free-name/" "refactor")))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-cleanup-session-end-reason ()
  "The end hook says `killed' for a live CLI and `exited' for a dead one."
  (claude-code-ide-tests--clear-processes)
  (let* ((reasons '())
         (claude-code-ide-session-end-functions
          (list (lambda (_session reason) (push reason reasons))))
         (live (start-process "mock-live" nil "sleep" "30"))
         (dead (start-process "mock-dead" nil "true")))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-code-ide-mcp--stop-session) #'ignore)
                  ((symbol-function 'claude-code-ide-mcp-server-session-ended) #'ignore))
          (while (process-live-p dead)
            (accept-process-output dead 0.05))
          (claude-code-ide--cleanup-session
           (claude-code-ide-tests--make-session "/tmp/end-reason/" :process live))
          (claude-code-ide--cleanup-session
           (claude-code-ide-tests--make-session "/tmp/end-reason/" :process dead))
          (should (equal (nreverse reasons) '(killed exited))))
      (when (process-live-p live)
        (delete-process live))
      (claude-code-ide-tests--clear-processes))))

(defun claude-code-ide-tests--cli-session-file (config-dir id &optional name source)
  "Write a CLI session file in CONFIG-DIR for this Emacs's PID with session ID.
NAME and SOURCE are its name and name source.  This Emacs runs, so the
file counts as a running CLI process."
  (make-directory (expand-file-name "sessions" config-dir) t)
  (with-temp-file (expand-file-name (format "sessions/%d.json" (emacs-pid)) config-dir)
    (insert (format "{\"pid\":%d,\"sessionId\":\"%s\",\"cwd\":\"/tmp\",\"name\":\"%s\",\"nameSource\":\"%s\"}"
                    (emacs-pid) id (or name "proj-2d") (or source "derived")))))

(ert-deftest claude-code-ide-test-registry-restore-after-exit ()
  "A restore takes the crashed sessions and those of the last Emacs exit.
Older exits, sessions that the user ended, and sessions whose id a
running CLI has are left alone."
  (claude-code-ide-tests--with-registry
    (let* ((config-dir (expand-file-name "claude" registry-dir))
           (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" config-dir) process-environment)))
      (claude-code-ide-tests--cli-session-file config-dir "id-running")
      (claude-code-ide-session-registry--submit
       "setup"
       (lambda (_)
         (list (claude-code-ide-tests--registry-entry
                "old-exit" :session-id "id-old" :state 'closed :closed-by "emacs-exit"
                :closed-at "2026-11-10T10:00:00Z" :emacs-pid 999999991)
               (claude-code-ide-tests--registry-entry
                "last-exit" :session-id "id-last" :state 'closed :closed-by "emacs-exit"
                :closed-at "2026-11-11T18:46:34Z" :emacs-pid 999999992)
               (claude-code-ide-tests--registry-entry
                "last-exit-running" :session-id "id-running" :state 'closed :closed-by "emacs-exit"
                :closed-at "2026-11-11T18:46:34Z" :emacs-pid 999999992)
               (claude-code-ide-tests--registry-entry
                "user-ended" :session-id "id-ended" :state 'closed :closed-by "exited"
                :closed-at "2026-11-11T18:00:00Z" :emacs-pid 999999992)
               (claude-code-ide-tests--registry-entry
                "crashed" :session-id "id-crashed" :emacs-pid 999999993))))
      (should (equal (sort (mapcar #'claude-code-ide-session-registry-entry-key
                                   (claude-code-ide-session-registry--restorable-entries))
                           #'string<)
                     '("crashed" "last-exit"))))))

(ert-deftest claude-code-ide-test-registry-restore-after-a-later-crash ()
  "An exit is not restored again when a later Emacs ran and crashed.
The later Emacs restored that exit's sessions, so each session id comes
back once, from the crashed Emacs's entry."
  (claude-code-ide-tests--with-registry
    (let* ((config-dir (expand-file-name "claude" registry-dir))
           (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" config-dir) process-environment)))
      (claude-code-ide-session-registry--submit
       "setup"
       (lambda (_)
         (list (claude-code-ide-tests--registry-entry
                "exit-a" :session-id "id-a" :state 'closed :closed-by "emacs-exit"
                :closed-at "2026-11-11T18:46:34Z" :emacs-pid 999999992)
               (claude-code-ide-tests--registry-entry
                "exit-b" :session-id "id-b" :state 'closed :closed-by "emacs-exit"
                :closed-at "2026-11-11T18:46:34Z" :emacs-pid 999999992)
               (claude-code-ide-tests--registry-entry
                "crashed-a" :session-id "id-a" :started-at "2026-11-11T19:30:00Z"
                :emacs-pid 999999993)
               (claude-code-ide-tests--registry-entry
                "crashed-c" :session-id "id-c" :started-at "2026-11-11T21:00:00Z"
                :emacs-pid 999999993))))
      (should (equal (sort (mapcar #'claude-code-ide-session-registry-entry-key
                                   (claude-code-ide-session-registry--restorable-entries))
                           #'string<)
                     '("crashed-a" "crashed-c"))))))

(ert-deftest claude-code-ide-test-registry-restore-one-entry-per-session ()
  "Two restorable entries with one session id restore once, from the newer."
  (claude-code-ide-tests--with-registry
    (let* ((config-dir (expand-file-name "claude" registry-dir))
           (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" config-dir) process-environment)))
      (claude-code-ide-session-registry--submit
       "setup"
       (lambda (_)
         (list (claude-code-ide-tests--registry-entry
                "older" :session-id "id-same" :started-at "2026-11-10T10:00:00Z"
                :emacs-pid 999999991)
               (claude-code-ide-tests--registry-entry
                "newer" :session-id "id-same" :started-at "2026-11-11T10:00:00Z"
                :emacs-pid 999999993))))
      (should (equal (mapcar #'claude-code-ide-session-registry-entry-key
                             (claude-code-ide-session-registry--restorable-entries))
                     '("newer"))))))

(ert-deftest claude-code-ide-test-registry-adopts-running-sessions ()
  "A session that ran before the registry is recorded with its CLI's id."
  (claude-code-ide-tests--clear-processes)
  (claude-code-ide-tests--with-registry
    (let* ((config-dir (expand-file-name "claude" registry-dir))
           (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" config-dir) process-environment))
           (claude-code-ide-session-registry--adopted (make-hash-table :test 'eq :weakness 'key))
           (claude-code-ide-session-registry--adopt-timer nil)
           (buffer (generate-new-buffer "*cc-adopt*"))
           (session (claude-code-ide-tests--make-session
                     "/tmp/adopt-project/" :buffer buffer :cli-pid (emacs-pid)
                     :instance-name "docs")))
      (unwind-protect
          (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
            (claude-code-ide-tests--cli-session-file config-dir "id-adopted")
            (should (= 1 (claude-code-ide-session-registry--adopt-running-sessions)))
            (should (= 0 (claude-code-ide-session-registry--adopt-running-sessions)))
            (let ((entry (car (claude-code-ide-session-registry--load))))
              (should (equal (claude-code-ide-session-registry-entry-session-id entry) "id-adopted"))
              (should (equal (claude-code-ide-session-registry-entry-project-dir entry) "/tmp/adopt-project/"))
              (should (equal (claude-code-ide-session-registry-entry-instance-name entry) "docs"))
              (should (equal (claude-code-ide-session-registry-entry-buffer-name entry) "*cc-adopt*"))
              (should (eq (claude-code-ide-session-registry-entry-state entry) 'open)))
            ;; /clear gives the CLI a new id
            (claude-code-ide-tests--cli-session-file config-dir "id-after-clear")
            (claude-code-ide-session-registry--refresh-adopted-ids)
            (should (equal (claude-code-ide-session-registry-entry-session-id
                            (car (claude-code-ide-session-registry--load)))
                           "id-after-clear"))
            (should (gethash session claude-code-ide-session-registry--keys)))
        (kill-buffer buffer)
        (claude-code-ide-tests--clear-processes)))))

(ert-deftest claude-code-ide-test-cli-environment-unset ()
  "Names in `claude-code-ide-cli-environment-unset' leave the CLI's environment only."
  (let* ((claude-code-ide-cli-environment-unset '("CCI_TEST_SECRET"))
         (claude-code-ide-no-flicker nil)
         (process-environment (cons "CCI_TEST_SECRET=from-emacs" process-environment))
         (env (claude-code-ide--cli-environment 4242)))
    (should (equal (car env) "CCI_TEST_SECRET"))
    (should (member "CLAUDE_CODE_SSE_PORT=4242" env))
    (let ((process-environment (append env process-environment)))
      (should (equal (with-temp-buffer
                       (call-process "sh" nil t nil "-c" "printf %s \"${CCI_TEST_SECRET-unset}\"")
                       (buffer-string))
                     "unset")))
    (should (equal (getenv "CCI_TEST_SECRET") "from-emacs"))))

(ert-deftest claude-code-ide-test-registry-restore-survives-failures ()
  "A quit or an error in one start of a restore does not stop the others."
  (let ((timers '())
        (started '())
        (messages '()))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (_secs _repeat fn &rest args) (push (cons fn args) timers)))
              ((symbol-function 'claude-code-ide-session-registry--start-entry)
               (lambda (entry _prompt)
                 (pcase (claude-code-ide-session-registry-entry-key entry)
                   ("a" (signal 'quit nil))
                   ("b" (error "Directory /x does not exist"))
                   (key (push key started)))))
              ((symbol-function 'message)
               (lambda (format-string &rest args) (push (apply #'format format-string args) messages))))
      (claude-code-ide-session-registry--start-entries
       (mapcar #'claude-code-ide-tests--registry-entry '("a" "b" "c")) "note")
      (should (= 3 (length timers)))
      (dolist (timer (reverse timers))
        (apply (car timer) (cdr timer))))
    (should (equal started '("c")))
    (should (string-match-p "\\`Restored 1 Claude Code session; failed: .*quit; .*Directory /x does not exist"
                            (car messages)))))

(ert-deftest claude-code-ide-test-registry-hooks-with-shell-characters ()
  "A report directory with ;, & and | in its path still gets the hooks."
  (claude-code-ide-tests--with-registry
    (cl-letf (((symbol-function 'claude-code-ide-mcp-server-ensure-server) #'ignore))
      (let* ((claude-code-ide-session-registry-report-directory
              (expand-file-name "a;b&c|d" registry-dir))
             (claude-code-ide-cli-extra-flags "")
             (args (claude-code-ide-session-registry--cli-args
                    (make-claude-code-ide-mcp-session) (claude-code-ide-tests--registry-spec)))
             (settings (cadr (member "--settings" args)))
             (parts (cdr (claude-code-ide--parse-command-string
                          (claude-code-ide--build-claude-command nil nil "sid" args)))))
        (should settings)
        ;; The hook command quotes the path for the shell that runs it
        (let* ((hooks (alist-get 'hooks (claude-code-ide-session-registry--json-decode settings)))
               (command (alist-get 'command
                                   (car (alist-get 'hooks (car (alist-get 'SessionStart hooks)))))))
          (should (string-prefix-p
                   (concat "cat > " (shell-quote-argument
                                     (expand-file-name "a;b&c|d" registry-dir)))
                   command)))
        (should (equal (cadr (member "--settings" parts)) settings))))))

(provide 'claude-code-ide-session-registry-tests)

;; Local Variables:
;; no-update-autoloads: t
;; autoload-compute-prefixes: nil
;; End:

;;; claude-code-ide-session-registry-tests.el ends here
