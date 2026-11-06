;;; claude-code-ide-session-status-tests.el --- Tests for the session status  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `claude-code-ide-session-status-mode': the hook script, the
;; event files, the state machine and the status buffer.  The event
;; sequences come from hook traces of a real CLI session.  The file is
;; loaded by claude-code-ide-tests.el, after its mocks, and uses its
;; helpers and those of the registry tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'claude-code-ide)
(require 'claude-code-ide-session-registry)
(require 'claude-code-ide-session-status)
(require 'claude-code-ide-session-registry-tests)

(declare-function claude-code-ide-tests--make-session "claude-code-ide-tests")
(declare-function claude-code-ide-tests--clear-processes "claude-code-ide-tests")

(defmacro claude-code-ide-tests--with-status (&rest body)
  "Run BODY with an empty registry and no tracked sessions.
Timers and file watches are not started."
  (declare (indent 0))
  `(claude-code-ide-tests--with-registry
     (let ((claude-code-ide-session-status--by-nonce (make-hash-table :test 'equal))
           (claude-code-ide-session-status--by-session (make-hash-table :test 'eq :weakness 'key))
           (claude-code-ide-session-status--urgent-count 0))
       (cl-letf (((symbol-function 'claude-code-ide-session-status--ensure-timers) #'ignore))
         ,@body))))

(defun claude-code-ide-tests--status (&optional now)
  "Return a new status record created at NOW."
  (let ((now (or now 0.0)))
    (claude-code-ide-session-status-create :nonce "abc" :since now :registered now :last-event now)))

(defun claude-code-ide-tests--feed (status events)
  "Apply EVENTS to STATUS.  Each is (EVENT JSON-TEXT); times count from 1."
  (let ((now 0.0))
    (dolist (event events)
      (claude-code-ide-session-status--apply
       status (car event)
       (claude-code-ide-session-registry--json-decode (cadr event))
       (cl-incf now)))
    status))

(defun claude-code-ide-tests--state (status)
  "Return the state and detail of STATUS at time 100."
  (let ((state (claude-code-ide-session-status--state status 100.0)))
    (list state (claude-code-ide-session-status--detail status state))))

(defconst claude-code-ide-tests--bash-input
  "{\"command\":\"touch made-by-test.txt\",\"description\":\"Create a file\"}")

(defun claude-code-ide-tests--tool (event id &optional input agent name)
  "Return a tool EVENT for tool use ID with INPUT, AGENT and NAME."
  (list event
        (format "{%s\"tool_name\":\"%s\",\"tool_use_id\":\"%s\",\"tool_input\":%s}"
                (if agent (format "\"agent_id\":\"%s\"," agent) "")
                (or name "Bash") id (or input claude-code-ide-tests--bash-input))))

(defun claude-code-ide-tests--permission (&optional input agent name)
  "Return a PermissionRequest for INPUT, AGENT and NAME (no tool use id)."
  (list "PermissionRequest"
        (format "{%s\"tool_name\":\"%s\",\"tool_input\":%s}"
                (if agent (format "\"agent_id\":\"%s\"," agent) "")
                (or name "Bash") (or input claude-code-ide-tests--bash-input))))

(ert-deftest claude-code-ide-test-status-permission-flow ()
  "Prompt, permission dialog, approval, end of turn (traced sequence)."
  (let ((status (claude-code-ide-tests--status)))
    (claude-code-ide-tests--feed status '(("SessionStart" "{\"source\":\"startup\"}")))
    (should (equal (claude-code-ide-tests--state status) '(idle "Ready")))
    (claude-code-ide-tests--feed
     status (list '("UserPromptSubmit" "{\"prompt\":\"run it\"}")
                  (claude-code-ide-tests--tool "PreToolUse" "t1")
                  (claude-code-ide-tests--permission)))
    (should (equal (claude-code-ide-tests--state status)
                   '(waiting "Bash: touch made-by-test.txt")))
    ;; The notification 6 s later adds nothing
    (claude-code-ide-tests--feed
     status '(("Notification" "{\"notification_type\":\"permission_prompt\",\"message\":\"Claude needs your permission\"}")))
    (should (= 1 (length (claude-code-ide-session-status-blockers status))))
    (claude-code-ide-tests--feed status (list (claude-code-ide-tests--tool "PostToolUse" "t1")))
    (should (eq (car (claude-code-ide-tests--state status)) 'working))
    (claude-code-ide-tests--feed
     status '(("Stop" "{\"last_assistant_message\":\"\\nThe command ran.\\nMore text\"}")))
    (should (equal (claude-code-ide-tests--state status) '(idle "The command ran.")))))

(ert-deftest claude-code-ide-test-status-question-menu ()
  "An AskUserQuestion menu waits with its question until the answer."
  (let* ((input "{\"questions\":[{\"question\":\"Which color do you choose?\",\"header\":\"Color\"}]}")
         (status (claude-code-ide-tests--feed
                  (claude-code-ide-tests--status)
                  (list '("UserPromptSubmit" "{}")
                        (claude-code-ide-tests--tool "PreToolUse" "q1" input nil "AskUserQuestion")
                        (claude-code-ide-tests--permission input nil "AskUserQuestion")))))
    (should (equal (claude-code-ide-tests--state status)
                   '(waiting "Question: Which color do you choose?")))
    (claude-code-ide-tests--feed
     status (list (claude-code-ide-tests--tool "PostToolUse" "q1" input nil "AskUserQuestion")))
    (should (eq (car (claude-code-ide-tests--state status)) 'working))))

(ert-deftest claude-code-ide-test-status-esc-denial ()
  "A denial with Esc fires no hook: the session waits until idle_prompt."
  (let ((status (claude-code-ide-tests--feed
                 (claude-code-ide-tests--status)
                 (list '("UserPromptSubmit" "{}")
                       (claude-code-ide-tests--tool "PreToolUse" "t1")
                       (claude-code-ide-tests--permission)))))
    (should (eq (car (claude-code-ide-tests--state status)) 'waiting))
    (claude-code-ide-tests--feed
     status '(("Notification" "{\"notification_type\":\"idle_prompt\",\"message\":\"Claude is waiting for your input\"}")))
    (should (equal (claude-code-ide-tests--state status) '(idle "Waiting for your input")))))

(ert-deftest claude-code-ide-test-status-two-dialogs ()
  "Each dialog ends only with its own tool, whatever the order of events."
  (let ((input-a "{\"command\":\"a\"}")
        (input-b "{\"command\":\"b\"}"))
    ;; Two requests; the second one is answered first
    (let ((status (claude-code-ide-tests--feed
                   (claude-code-ide-tests--status)
                   (list '("UserPromptSubmit" "{}")
                         (claude-code-ide-tests--tool "PreToolUse" "a" input-a)
                         (claude-code-ide-tests--tool "PreToolUse" "b" input-b)
                         (claude-code-ide-tests--permission input-a)
                         (claude-code-ide-tests--permission input-b)
                         (claude-code-ide-tests--tool "PostToolUse" "b" input-b)))))
      (should (equal (claude-code-ide-tests--state status) '(waiting "Bash: a")))
      (claude-code-ide-tests--feed status (list (claude-code-ide-tests--tool "PostToolUse" "a" input-a)))
      (should (eq (car (claude-code-ide-tests--state status)) 'working)))
    ;; A later PreToolUse does not take over the request of an earlier tool
    (let ((status (claude-code-ide-tests--feed
                   (claude-code-ide-tests--status)
                   (list '("UserPromptSubmit" "{}")
                         (claude-code-ide-tests--tool "PreToolUse" "a" input-a)
                         (claude-code-ide-tests--tool "PreToolUse" "b" input-b)
                         (claude-code-ide-tests--permission input-a)
                         (claude-code-ide-tests--tool "PostToolUse" "b" input-b)))))
      (should (eq (car (claude-code-ide-tests--state status)) 'waiting))
      (claude-code-ide-tests--feed status (list (claude-code-ide-tests--tool "PostToolUse" "a" input-a)))
      (should (eq (car (claude-code-ide-tests--state status)) 'working)))))

(ert-deftest claude-code-ide-test-status-elicitation ()
  "An elicitation waits through other tool events until its result."
  (let ((status (claude-code-ide-tests--feed
                 (claude-code-ide-tests--status)
                 (list '("UserPromptSubmit" "{}")
                       '("Elicitation" "{\"mcp_server_name\":\"jira\",\"message\":\"Pick a project\"}")
                       (claude-code-ide-tests--tool "PostToolUse" "x")))))
    (should (equal (claude-code-ide-tests--state status)
                   '(waiting "Input for jira: Pick a project")))
    (claude-code-ide-tests--feed status '(("ElicitationResult" "{}")))
    (should (eq (car (claude-code-ide-tests--state status)) 'working))))

(ert-deftest claude-code-ide-test-status-subagent-dialog-outlives-stop ()
  "A subagent's dialog keeps the session waiting after the main Stop."
  (let ((status (claude-code-ide-tests--feed
                 (claude-code-ide-tests--status)
                 (list '("UserPromptSubmit" "{}")
                       (claude-code-ide-tests--tool "PreToolUse" "s1" nil "agent-7")
                       (claude-code-ide-tests--permission nil "agent-7")
                       '("Stop" "{\"last_assistant_message\":\"Started a helper\"}")))))
    (should (eq (car (claude-code-ide-tests--state status)) 'waiting))
    (claude-code-ide-tests--feed status (list (claude-code-ide-tests--tool "PostToolUse" "s1" nil "agent-7")))
    ;; The turn had ended: the session is idle, not working
    (should (equal (claude-code-ide-tests--state status) '(idle "Started a helper")))))

(ert-deftest claude-code-ide-test-status-unmatched-notification-waits ()
  "A waiting notification without a known dialog still marks the session."
  (dolist (type '("worker_permission_prompt" "agent_needs_input" "elicitation_url_dialog"))
    (let ((status (claude-code-ide-tests--feed
                   (claude-code-ide-tests--status)
                   (list '("UserPromptSubmit" "{}")
                         (list "Notification"
                               (format "{\"notification_type\":\"%s\",\"message\":\"Help\"}" type))))))
      (should (equal (claude-code-ide-tests--state status) '(waiting "Help"))))))

(ert-deftest claude-code-ide-test-status-compact-clear-and-late-events ()
  "Compaction keeps the phase; /clear resets; late tool events keep idle."
  (let ((status (claude-code-ide-tests--feed
                 (claude-code-ide-tests--status)
                 (list '("UserPromptSubmit" "{}")
                       '("SessionStart" "{\"source\":\"compact\"}")))))
    (should (eq (car (claude-code-ide-tests--state status)) 'working))
    (claude-code-ide-tests--feed
     status (list (claude-code-ide-tests--tool "PreToolUse" "t1")
                  (claude-code-ide-tests--permission)
                  '("SessionStart" "{\"source\":\"clear\"}")))
    (should (equal (claude-code-ide-tests--state status) '(idle "Ready")))
    (claude-code-ide-tests--feed
     status (list '("UserPromptSubmit" "{}")
                  '("Stop" "{\"last_assistant_message\":\"Done\"}")
                  (claude-code-ide-tests--tool "PreToolUse" "late")
                  (claude-code-ide-tests--tool "PostToolUse" "late")))
    (should (equal (claude-code-ide-tests--state status) '(idle "Done")))
    (claude-code-ide-tests--feed status '(("StopFailure" "{\"error\":\"rate_limit\"}")))
    (should (equal (claude-code-ide-tests--state status) '(error "Error: rate_limit")))))

(ert-deftest claude-code-ide-test-status-stop-failure-with-dialog ()
  "An API error ends the main turn: its dialog closes and the state is error."
  (let ((status (claude-code-ide-tests--feed
                 (claude-code-ide-tests--status)
                 (list '("UserPromptSubmit" "{}")
                       (claude-code-ide-tests--tool "PreToolUse" "t1")
                       (claude-code-ide-tests--permission)
                       '("StopFailure" "{\"error\":\"overloaded\"}")))))
    (should (equal (claude-code-ide-tests--state status) '(error "Error: overloaded")))
    (should (zerop (hash-table-count (claude-code-ide-session-status-inflight status))))))

(ert-deftest claude-code-ide-test-status-tracking-off-and-registry-off ()
  "Tracking off starts no tracking; an ended session is forgotten in any case."
  (claude-code-ide-tests--with-status
    (let ((claude-code-ide-session-end-functions nil)
          (claude-code-ide-session-registry-hook-functions nil))
      (claude-code-ide-session-status--set-tracking t)
      (let ((session (make-claude-code-ide-mcp-session)))
        (let ((claude-code-ide-session-status-tracking nil))
          (should-not (claude-code-ide-session-status--hooks session "k0"))
          (should-not (gethash session claude-code-ide-session-status--by-session)))
        (should (claude-code-ide-session-status--hooks session "k1"))
        ;; The registry mode is turned off while the session runs
        (claude-code-ide-session-status--set-tracking nil)
        (should-not claude-code-ide-session-registry-hook-functions)
        (run-hook-with-args 'claude-code-ide-session-end-functions session 'exited)
        (should (zerop (hash-table-count claude-code-ide-session-status--by-nonce)))))))

(ert-deftest claude-code-ide-test-status-drain-survives-bad-files-and-locks ()
  "An unreadable event file is deleted; an old lock is removed."
  (claude-code-ide-tests--with-status
    (let* ((session (make-claude-code-ide-mcp-session))
           (_hooks (claude-code-ide-session-status--hooks session "k1"))
           (status (gethash session claude-code-ide-session-status--by-session))
           (nonce (claude-code-ide-session-status-nonce status))
           (dir (claude-code-ide-session-status--events-directory))
           (bad (expand-file-name (format "%s-00000001-Stop.json" nonce) dir))
           (lock (expand-file-name (format ".%s.lock" nonce) dir)))
      (make-directory dir t)
      (with-temp-file bad (insert "{\"last_assistant_message\":"))
      (make-directory lock)
      (set-file-times lock (time-subtract nil 60))
      (claude-code-ide-session-status--drain)
      (should-not (file-exists-p bad))
      (should-not (file-exists-p lock))
      (should (eq (claude-code-ide-session-status-phase status) 'starting)))))

(ert-deftest claude-code-ide-test-status-hook-script-dead-lock-holder ()
  "The script breaks the lock of a holder that died, and numbers on."
  (claude-code-ide-tests--with-status
    (claude-code-ide-session-status--install-script)
    (let* ((script (claude-code-ide-session-status--script-file))
           (dir (expand-file-name "events" registry-dir))
           (lock (expand-file-name ".n3.lock" dir)))
      (make-directory lock t)
      ;; A PID that is not running
      (with-temp-file (expand-file-name "pid" lock) (insert "999999\n"))
      (with-temp-buffer
        (should (= 0 (call-process "sh" nil t nil "-c"
                                   (format "echo '{}' | sh %s %s n3 Stop"
                                           (shell-quote-argument script)
                                           (shell-quote-argument dir)))))
        (should (equal (buffer-string) "")))
      (should (equal (directory-files dir nil "\\`n3-") '("n3-00000001-Stop.json")))
      (should-not (file-exists-p lock)))))

;;; Background tasks (shapes from traces of CLI 2.1.317)

(defun claude-code-ide-tests--feed-at (status events)
  "Apply EVENTS to STATUS.  Each is (TIME EVENT JSON-TEXT)."
  (dolist (event events status)
    (claude-code-ide-session-status--apply
     status (nth 1 event)
     (claude-code-ide-session-registry--json-decode (nth 2 event))
     (float (car event)))))

(defun claude-code-ide-tests--state-at (status now)
  "Return the state and detail of STATUS at NOW."
  (let ((state (claude-code-ide-session-status--state status (float now))))
    (list state (claude-code-ide-session-status--detail status state (float now)))))

(defun claude-code-ide-tests--agent-task (id &optional description)
  "Return the background_tasks JSON item of the subagent ID."
  (format "{\"id\":\"%s\",\"type\":\"subagent\",\"status\":\"running\",\"description\":\"%s\",\"agent_type\":\"general-purpose\"}"
          id (or description (concat "Task " id))))

(defun claude-code-ide-tests--shell-task (id &optional description)
  "Return the background_tasks JSON item of the shell ID."
  (format "{\"id\":\"%s\",\"type\":\"shell\",\"status\":\"running\",\"description\":\"%s\",\"command\":\"pytest\"}"
          id (or description "Run tests")))

(defun claude-code-ide-tests--stop (reply &rest tasks)
  "Return a Stop event text with REPLY and the background TASKS items."
  (format "{\"last_assistant_message\":\"%s\",\"background_tasks\":[%s],\"session_crons\":[]}"
          reply (string-join tasks ",")))

(defun claude-code-ide-tests--subagent (event id &optional type)
  "Return the text of a SubagentStart or SubagentStop EVENT for agent ID.
TYPE is the agent type; the CLI's own helper agents have \"\"."
  (format "{\"agent_id\":\"%s\",\"agent_type\":\"%s\"%s}"
          id (or type "general-purpose")
          (if (equal event "SubagentStop") ",\"background_tasks\":[]" "")))

(defun claude-code-ide-tests--agent-tool (_event agent id)
  "Return the text of a tool event of subagent AGENT for tool use ID.
The event name is only for the reader; the text is the same."
  (format "{\"agent_id\":\"%s\",\"agent_type\":\"general-purpose\",\"tool_name\":\"Bash\",\"tool_use_id\":\"%s\",\"tool_input\":{\"command\":\"ls\"}}"
          agent id))

(ert-deftest claude-code-ide-test-status-tasks-shell-idle-prompt ()
  "A background shell keeps the session on its tasks through idle_prompt."
  (let ((status (claude-code-ide-tests--feed-at
                 (claude-code-ide-tests--status)
                 `((1 "UserPromptSubmit" "{}")
                   (4 "Stop" ,(claude-code-ide-tests--stop
                               "Started the tests" (claude-code-ide-tests--shell-task "b1")))))))
    (should (equal (claude-code-ide-tests--state-at status 10) '(tasks "1 shell: Run tests")))
    ;; F6: idle_prompt fires while the shell runs
    (claude-code-ide-tests--feed-at
     status '((64 "Notification" "{\"notification_type\":\"idle_prompt\",\"message\":\"Claude is waiting for your input\"}")))
    (should (eq (car (claude-code-ide-tests--state-at status 70)) 'tasks))
    (should (equal (claude-code-ide-session-status-detail status) "Started the tests"))
    ;; The shell ends: the CLI wakes the agent, which reports
    (claude-code-ide-tests--feed-at
     status `((100 "UserPromptSubmit" "{}")
              (102 "Stop" ,(claude-code-ide-tests--stop "Tests passed"))))
    (should (equal (claude-code-ide-tests--state-at status 110) '(idle "Tests passed")))
    ;; idle_prompt keeps the reply
    (claude-code-ide-tests--feed-at
     status '((162 "Notification" "{\"notification_type\":\"idle_prompt\"}")))
    (should (equal (claude-code-ide-tests--state-at status 170) '(idle "Tests passed")))))

(ert-deftest claude-code-ide-test-status-tasks-subagent-lifecycle ()
  "A background subagent: start, internal helper stops, wake, stop."
  (let ((status (claude-code-ide-tests--feed-at
                 (claude-code-ide-tests--status)
                 `((1 "UserPromptSubmit" "{}")
                   (2 "PreToolUse" "{\"tool_name\":\"Agent\",\"tool_use_id\":\"m1\",\"tool_input\":{}}")
                   (3 "PostToolUse" "{\"tool_name\":\"Agent\",\"tool_use_id\":\"m1\",\"tool_input\":{}}")
                   (3 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                   (4 "Stop" ,(claude-code-ide-tests--stop "Started an agent"
                                                           (claude-code-ide-tests--agent-task "a1" "Scan the repo")))
                   (5 "SubagentStop" ,(claude-code-ide-tests--subagent "SubagentStop" "zz" ""))
                   (6 "PreToolUse" ,(claude-code-ide-tests--agent-tool "PreToolUse" "a1" "s1"))))))
    (should (equal (claude-code-ide-tests--state-at status 10) '(tasks "1 agent: Scan the repo")))
    (claude-code-ide-tests--feed-at
     status `((40 "PostToolUse" ,(claude-code-ide-tests--agent-tool "PostToolUse" "a1" "s1"))
              (41 "UserPromptSubmit" "{}")
              (41 "SubagentStop" ,(claude-code-ide-tests--subagent "SubagentStop" "a1"))))
    (should (eq (car (claude-code-ide-tests--state-at status 42)) 'working))
    (claude-code-ide-tests--feed-at
     status `((43 "Stop" ,(claude-code-ide-tests--stop "The agent found 3 files"))))
    (should (equal (claude-code-ide-tests--state-at status 50) '(idle "The agent found 3 files")))))

(ert-deftest claude-code-ide-test-status-tasks-terminal-orderings ()
  "An ended agent never runs again, whatever arrives later."
  (let ((status (claude-code-ide-tests--feed-at
                 (claude-code-ide-tests--status)
                 `((1 "UserPromptSubmit" "{}")
                   (2 "SubagentStop" ,(claude-code-ide-tests--subagent "SubagentStop" "a1"))
                   (3 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                   (4 "Stop" ,(claude-code-ide-tests--stop "Done" (claude-code-ide-tests--agent-task "a1")))))))
    (should (equal (claude-code-ide-tests--state-at status 10) '(idle "Done")))))

(ert-deftest claude-code-ide-test-status-tasks-snapshot-drops-killed ()
  "A snapshot that omits a running agent ends it, with its dialog."
  (let ((status (claude-code-ide-tests--feed-at
                 (claude-code-ide-tests--status)
                 `((1 "UserPromptSubmit" "{}")
                   (2 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                   (2 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a2"))
                   (3 "PreToolUse" ,(claude-code-ide-tests--agent-tool "PreToolUse" "a2" "s2"))
                   (4 "PermissionRequest" "{\"agent_id\":\"a2\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"ls\"}}")
                   (5 "Stop" ,(claude-code-ide-tests--stop "Waiting" (claude-code-ide-tests--agent-task "a1")))))))
    (should (equal (mapcar #'claude-code-ide-session-status--task-id
                           (claude-code-ide-session-status--running-tasks status))
                   '("a1")))
    (should (eq (car (claude-code-ide-tests--state-at status 10)) 'tasks))))

(ert-deftest claude-code-ide-test-status-tasks-dialogs-and-wakes ()
  "A wake is no answer to a subagent's dialog; a user prompt ends a main one."
  (let ((status (claude-code-ide-tests--feed-at
                 (claude-code-ide-tests--status)
                 `((1 "UserPromptSubmit" "{}")
                   (2 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                   (3 "Stop" ,(claude-code-ide-tests--stop "Started" (claude-code-ide-tests--agent-task "a1")))
                   (4 "PreToolUse" ,(claude-code-ide-tests--agent-tool "PreToolUse" "a1" "s1"))
                   (5 "PermissionRequest" "{\"agent_id\":\"a1\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"ls\"}}")
                   (6 "UserPromptSubmit" "{}")
                   (7 "Stop" ,(claude-code-ide-tests--stop "Still waiting" (claude-code-ide-tests--agent-task "a1")))))))
    (should (equal (claude-code-ide-tests--state-at status 10) '(waiting "Bash: ls")))
    (claude-code-ide-tests--feed-at
     status `((20 "PostToolUse" ,(claude-code-ide-tests--agent-tool "PostToolUse" "a1" "s1"))))
    (should (eq (car (claude-code-ide-tests--state-at status 21)) 'tasks)))
  ;; Esc denial of a main dialog fires nothing; the next prompt ends it
  (let ((status (claude-code-ide-tests--feed
                 (claude-code-ide-tests--status)
                 (list '("UserPromptSubmit" "{}")
                       (claude-code-ide-tests--tool "PreToolUse" "t1")
                       (claude-code-ide-tests--permission)
                       '("UserPromptSubmit" "{\"prompt\":\"do something else\"}")))))
    (should (eq (car (claude-code-ide-tests--state status)) 'working))))

(ert-deftest claude-code-ide-test-status-tasks-unmatched-permission ()
  "A subagent dialog without a known tool survives the main Stop."
  (let ((status (claude-code-ide-tests--feed-at
                 (claude-code-ide-tests--status)
                 `((1 "UserPromptSubmit" "{}")
                   (2 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                   (3 "PermissionRequest" "{\"agent_id\":\"a1\",\"tool_name\":\"Write\",\"tool_input\":{\"file_path\":\"/tmp/x\"}}")
                   (4 "Stop" ,(claude-code-ide-tests--stop "Started" (claude-code-ide-tests--agent-task "a1")))))))
    (should (eq (car (claude-code-ide-tests--state-at status 10)) 'waiting))
    (claude-code-ide-tests--feed-at
     status `((30 "SubagentStop" ,(claude-code-ide-tests--subagent "SubagentStop" "a1"))))
    (should (eq (car (claude-code-ide-tests--state-at status 31)) 'idle))))

(ert-deftest claude-code-ide-test-status-tasks-quiet-boundaries ()
  "Each task has its own clock; a busy sibling does not hide a quiet task."
  (let ((claude-code-ide-session-status-agent-quiet-time 600)
        (claude-code-ide-session-status-task-quiet-time 900))
    ;; A subagent that runs no tool: 600 s
    (let ((status (claude-code-ide-tests--feed-at
                   (claude-code-ide-tests--status)
                   `((1 "UserPromptSubmit" "{}")
                     (10 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                     (11 "Stop" ,(claude-code-ide-tests--stop "Started" (claude-code-ide-tests--agent-task "a1")))))))
      (should (eq (car (claude-code-ide-tests--state-at status 609)) 'tasks))
      (should (equal (claude-code-ide-tests--state-at status 611)
                     '(tasks-quiet "No activity for 10m: Task a1"))))
    ;; A subagent in one long tool call: 900 s
    (let ((status (claude-code-ide-tests--feed-at
                   (claude-code-ide-tests--status)
                   `((1 "UserPromptSubmit" "{}")
                     (10 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                     (11 "Stop" ,(claude-code-ide-tests--stop "Started" (claude-code-ide-tests--agent-task "a1")))
                     (20 "PreToolUse" ,(claude-code-ide-tests--agent-tool "PreToolUse" "a1" "s1"))))))
      (should (eq (car (claude-code-ide-tests--state-at status 919)) 'tasks))
      (should (eq (car (claude-code-ide-tests--state-at status 921)) 'tasks-quiet)))
    ;; A shell: 900 s from its start; a busy sibling agent does not hide it
    (let ((status (claude-code-ide-tests--feed-at
                   (claude-code-ide-tests--status)
                   `((1 "UserPromptSubmit" "{}")
                     (10 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a2"))
                     (11 "Stop" ,(claude-code-ide-tests--stop "Started"
                                                              (claude-code-ide-tests--shell-task "b1")
                                                              (claude-code-ide-tests--agent-task "a2")))
                     (905 "PreToolUse" ,(claude-code-ide-tests--agent-tool "PreToolUse" "a2" "s2"))))))
      (should (eq (car (claude-code-ide-tests--state-at status 910)) 'tasks))
      (should (equal (claude-code-ide-tests--state-at status 912)
                     '(tasks-quiet "No activity for 15m: Run tests")))
      (should (claude-code-ide-session-status--urgent-p 'tasks-quiet)))))

(ert-deftest claude-code-ide-test-status-tasks-snapshot-keeps-age ()
  "A later snapshot keeps a task's start and does not count as activity."
  (let ((claude-code-ide-session-status-agent-quiet-time 600)
        (status (claude-code-ide-tests--feed-at
                 (claude-code-ide-tests--status)
                 `((1 "UserPromptSubmit" "{}")
                   (10 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                   (11 "Stop" ,(claude-code-ide-tests--stop "Started" (claude-code-ide-tests--agent-task "a1")))
                   (300 "UserPromptSubmit" "{}")
                   (301 "Stop" ,(claude-code-ide-tests--stop "Still running" (claude-code-ide-tests--agent-task "a1")))))))
    (should (= 10.0 (claude-code-ide-session-status--task-first-seen
                     (car (claude-code-ide-session-status--running-tasks status)))))
    (should (eq (car (claude-code-ide-tests--state-at status 611)) 'tasks-quiet))))

(ert-deftest claude-code-ide-test-status-tasks-reset-and-compact ()
  "Compaction keeps the tasks; /clear drops them."
  (let ((status (claude-code-ide-tests--feed-at
                 (claude-code-ide-tests--status)
                 `((1 "UserPromptSubmit" "{}")
                   (2 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                   (3 "Stop" ,(claude-code-ide-tests--stop "Started" (claude-code-ide-tests--agent-task "a1")))
                   (4 "SessionStart" "{\"source\":\"compact\"}")))))
    (should (eq (car (claude-code-ide-tests--state-at status 10)) 'tasks))
    (claude-code-ide-tests--feed-at status '((20 "SessionStart" "{\"source\":\"clear\"}")))
    (should (equal (claude-code-ide-tests--state-at status 21) '(idle "Ready")))))

(ert-deftest claude-code-ide-test-status-tasks-interrupt-with-busy-subagent ()
  "Subagent events do not hide an interrupt of the main agent."
  (claude-code-ide-tests--with-status
    (let* ((claude-code-ide-session-status-quiet-time 5)
           (terminal (generate-new-buffer "*cc-terminal*"))
           (status (claude-code-ide-tests--status)))
      (unwind-protect
          (progn
            (setf (claude-code-ide-session-status-session status)
                  (make-claude-code-ide-mcp-session :buffer terminal))
            (with-current-buffer terminal
              (insert "  ⎿  Interrupted · What should Claude do instead?\n> \n"))
            (claude-code-ide-tests--feed-at
             status `((1 "UserPromptSubmit" "{}")
                      (2 "SubagentStart" ,(claude-code-ide-tests--subagent "SubagentStart" "a1"))
                      (8 "PreToolUse" ,(claude-code-ide-tests--agent-tool "PreToolUse" "a1" "s1"))
                      (9 "PostToolUse" ,(claude-code-ide-tests--agent-tool "PostToolUse" "a1" "s1"))))
            (should (claude-code-ide-session-status--check-interrupt status 10.0))
            ;; The guess stands against further subagent tool events
            (claude-code-ide-tests--feed-at
             status `((11 "PreToolUse" ,(claude-code-ide-tests--agent-tool "PreToolUse" "a1" "s2"))))
            (should (claude-code-ide-session-status-guessed status))
            (should (eq (car (claude-code-ide-tests--state-at status 12)) 'tasks)))
        (kill-buffer terminal)))))

(ert-deftest claude-code-ide-test-status-tasks-labels-and-order ()
  "The labels say who waits for whom; quiet tasks rank above a finished turn."
  (should (equal (mapcar (lambda (state)
                           (car (alist-get state claude-code-ide-session-status--labels)))
                         '(waiting error unobserved tasks-quiet idle tasks starting working untracked))
                 '("NEEDS ANSWER" "FAILED" "NO SIGNAL" "TASKS QUIET" "WAITS FOR YOU"
                   "WAITS ON TASKS" "STARTING" "WORKING" "UNTRACKED")))
  (should (< (alist-get 'tasks-quiet claude-code-ide-session-status--rank)
             (alist-get 'idle claude-code-ide-session-status--rank)
             (alist-get 'tasks claude-code-ide-session-status--rank)))
  (should-not (claude-code-ide-session-status--urgent-p 'tasks))
  (should-not (claude-code-ide-session-status--urgent-p 'idle))
  ;; The Status column fits every label
  (with-temp-buffer
    (claude-code-ide-session-status-list-mode)
    (let ((width (nth 1 (aref tabulated-list-format 0))))
      (dolist (entry claude-code-ide-session-status--labels)
        (should (<= (string-width (nth 1 entry)) width))))))

(ert-deftest claude-code-ide-test-status-unobserved-and-untracked ()
  "A session without events is unobserved after the timeout."
  (let ((status (claude-code-ide-tests--status 0.0))
        (claude-code-ide-session-status-start-timeout 15))
    (should (eq (claude-code-ide-session-status--state status 10.0) 'starting))
    (should (eq (claude-code-ide-session-status--state status 20.0) 'unobserved))
    (should (eq (claude-code-ide-session-status--state nil 20.0) 'untracked))))

(ert-deftest claude-code-ide-test-status-interrupt-from-transcript ()
  "A quiet working session reads only new transcript text for an interrupt."
  (claude-code-ide-tests--with-status
    (let* ((transcript (expand-file-name "t.jsonl" registry-dir))
           (claude-code-ide-session-status-quiet-time 5)
           (terminal (generate-new-buffer "*cc-terminal*"))
           (status (claude-code-ide-tests--status)))
      (unwind-protect
          (progn
            (setf (claude-code-ide-session-status-session status)
                  (make-claude-code-ide-mcp-session :buffer terminal))
            (with-current-buffer terminal
              (insert "⏺ Reading the files\n\n✢ Misting… (12s · ↓ 330 tokens)\n\n> \n"))
            (with-temp-file transcript
              (insert "{\"text\":\"[Request interrupted by user] from an old turn\"}\n"))
            (claude-code-ide-tests--feed
             status (list (list "SessionStart" (format "{\"source\":\"startup\",\"transcript_path\":\"%s\"}"
                                                       transcript))
                          '("UserPromptSubmit" "{}")))
            ;; Not quiet long enough; then quiet, but the spinner shows work
            ;; and the old marker is before the offset
            (should-not (claude-code-ide-session-status--check-interrupt status 3.0))
            (should-not (claude-code-ide-session-status--check-interrupt status 20.0))
            (should (eq (claude-code-ide-session-status-phase status) 'working))
            ;; The marker inside a tool result (a file Claude read) does not count
            (write-region (concat "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":"
                                  "[{\"type\":\"tool_result\",\"content\":\"[Request interrupted by user] in a file\"}]}}\n")
                          nil transcript t 'silent)
            (should-not (claude-code-ide-session-status--check-interrupt status 25.0))
            ;; An interrupt during a tool writes the CLI's record (measured shape)
            (write-region (concat "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":"
                                  "[{\"type\":\"text\",\"text\":\"[Request interrupted by user for tool use]\"}]}}\n")
                          nil transcript t 'silent)
            (should (claude-code-ide-session-status--check-interrupt status 30.0))
            (should (equal (claude-code-ide-tests--state status) '(idle "Interrupted")))
            ;; An interrupt during an answer leaves no record: the spinner is gone
            (claude-code-ide-tests--feed status '(("UserPromptSubmit" "{}")))
            (should-not (claude-code-ide-session-status--check-interrupt status 40.0))
            (with-current-buffer terminal
              (erase-buffer)
              (insert "⏺ Partial answer\n  ⎿  Interrupted · What should Claude do instead?\n\n> \n"))
            (should (claude-code-ide-session-status--check-interrupt status 50.0))
            (should (equal (claude-code-ide-tests--state status) '(idle "Interrupted?")))
            ;; The guess ends when the spinner shows again, even below many
            ;; blank rows, or when a tool event arrives
            (with-current-buffer terminal
              (insert "✳ Whisking… (3s · thinking)\n" (make-string 45 ?\n)))
            (should (claude-code-ide-session-status--check-interrupt status 52.0))
            (should (eq (car (claude-code-ide-tests--state status)) 'working))
            (with-current-buffer terminal (erase-buffer))
            (should (claude-code-ide-session-status--check-interrupt status 60.0))
            (claude-code-ide-tests--feed status (list (claude-code-ide-tests--tool "PreToolUse" "t9")))
            (should (eq (car (claude-code-ide-tests--state status)) 'working)))
        (kill-buffer terminal)))))

(ert-deftest claude-code-ide-test-status-hook-script ()
  "The script numbers parallel events without gaps, prints nothing, exits 0."
  (claude-code-ide-tests--with-status
    (claude-code-ide-session-status--install-script)
    (let* ((script (claude-code-ide-session-status--script-file))
           (dir (expand-file-name "events" registry-dir))
           (output
            (with-temp-buffer
              (list (call-process
                     "sh" nil t nil "-c"
                     (format "i=0; while [ $i -lt 20 ]; do echo '{\"n\":1}' | sh %s %s n1 Stop & i=$((i+1)); done; wait"
                             (shell-quote-argument script) (shell-quote-argument dir)))
                    (buffer-string))))
           (files (directory-files dir nil "\\`n1-[0-9]+-Stop\\.json\\'")))
      (should (equal output '(0 "")))
      (should (equal (mapcar (lambda (file) (string-to-number (substring file 3 11))) files)
                     (number-sequence 1 20)))
      (should (equal (claude-code-ide-session-status--read-event (expand-file-name (car files) dir))
                     '((n . 1))))
      ;; A directory that cannot be made: still no output and exit 0
      (with-temp-buffer
        (should (= 0 (call-process "sh" nil t nil "-c"
                                   (format "echo '{}' | sh %s /dev/null/x n2 Stop"
                                           (shell-quote-argument script)))))
        (should (equal (buffer-string) ""))))))

(ert-deftest claude-code-ide-test-status-drain-applies-in-order ()
  "Events are applied in sequence order; unknown launches are deleted."
  (claude-code-ide-tests--with-status
    (let* ((session (make-claude-code-ide-mcp-session))
           (hooks (claude-code-ide-session-status--hooks session "key-1"))
           (status (gethash session claude-code-ide-session-status--by-session))
           (nonce (claude-code-ide-session-status-nonce status))
           (dir (claude-code-ide-session-status--events-directory)))
      (should (equal (mapcar #'car hooks) claude-code-ide-session-status--events))
      (should (string-match-p (regexp-quote nonce) (cadr (assoc "Stop" hooks))))
      (make-directory dir t)
      (dolist (event `((2 "PermissionRequest" ,(cadr (claude-code-ide-tests--permission)))
                       (1 "PreToolUse" ,(cadr (claude-code-ide-tests--tool "PreToolUse" "t1")))
                       (3 "Stop" "{\"last_assistant_message\":\"Done\"}")))
        (with-temp-file (expand-file-name (format "%s-%08d-%s.json" nonce (car event) (cadr event)) dir)
          (insert (nth 2 event))))
      (with-temp-file (expand-file-name "ffffffffffff-00000001-Stop.json" dir)
        (insert "{}"))
      (claude-code-ide-session-status--drain)
      ;; The request matched the tool, so Stop closed it: idle
      (should (equal (claude-code-ide-tests--state status) '(idle "Done")))
      (should-not (directory-files dir nil "\\.json\\'"))
      ;; The end of the session removes it and its files
      (with-temp-file (expand-file-name (format ".%s.seq" nonce) dir) (insert "3"))
      (claude-code-ide-session-status--on-end session 'exited)
      (should-not (gethash nonce claude-code-ide-session-status--by-nonce))
      (should-not (directory-files dir nil (regexp-quote nonce))))))

(ert-deftest claude-code-ide-test-status-settings-argument ()
  "Tracking adds every status hook, with a timeout, to the one --settings."
  (claude-code-ide-tests--with-status
    (let ((claude-code-ide-session-registry-hook-functions
           (list #'claude-code-ide-session-status--hooks))
          (claude-code-ide-cli-extra-flags ""))
      (cl-letf (((symbol-function 'claude-code-ide-mcp-server-ensure-server) #'ignore))
        (let* ((session (make-claude-code-ide-mcp-session))
               (args (claude-code-ide-session-registry--cli-args
                      session (claude-code-ide-tests--registry-spec)))
               (parts (split-string-shell-command
                       (claude-code-ide--build-claude-command nil nil "sid" args)))
               (hooks (alist-get 'hooks (claude-code-ide-session-registry--json-decode
                                         (cadr (member "--settings" parts))))))
          (should (= 1 (cl-count "--settings" parts :test #'equal)))
          (dolist (event claude-code-ide-session-status--events)
            (let ((commands (alist-get 'hooks (car (alist-get (intern event) hooks)))))
              (should commands)
              (dolist (command commands)
                (should (equal (alist-get 'timeout command) 5)))))
          ;; SessionStart has the id report and the status event
          (should (= 2 (length (alist-get 'hooks (car (alist-get 'SessionStart hooks))))))
          (should (gethash session claude-code-ide-session-status--by-session)))))))

(ert-deftest claude-code-ide-test-status-buffer-order-and-visit ()
  "Sessions are grouped by project, the most urgent project first.
RET goes to the session on the line, or to the first session of a
project line."
  (claude-code-ide-tests--clear-processes)
  (claude-code-ide-tests--with-status
    (let* ((buffers (list (generate-new-buffer "*cc-work*")
                          (generate-new-buffer "*cc-wait*")
                          (generate-new-buffer "*cc-plain*")
                          (generate-new-buffer "*cc-wait-2*")))
           (working (claude-code-ide-tests--make-session "/tmp/status-a/" :buffer (nth 0 buffers)))
           (waiting (claude-code-ide-tests--make-session "/tmp/status-b/" :buffer (nth 1 buffers)))
           (plain (claude-code-ide-tests--make-session "/tmp/status-c/" :buffer (nth 2 buffers)))
           (sibling (claude-code-ide-tests--make-session "/tmp/status-b/" :buffer (nth 3 buffers)
                                                         :instance-name "docs"))
           (visited nil))
      (unwind-protect
          (progn
            (claude-code-ide-session-status--hooks working "k1")
            (claude-code-ide-session-status--hooks waiting "k2")
            (claude-code-ide-tests--feed (gethash working claude-code-ide-session-status--by-session)
                                         '(("UserPromptSubmit" "{}")))
            (claude-code-ide-tests--feed (gethash waiting claude-code-ide-session-status--by-session)
                                         (list '("UserPromptSubmit" "{}")
                                               (claude-code-ide-tests--tool "PreToolUse" "t1")
                                               (claude-code-ide-tests--permission)))
            ;; The two untracked sessions have no time, so their order is open
            (let ((rows (mapcar (lambda (row) (list (car row) (nth 1 row)))
                                (claude-code-ide-session-status--rows))))
              (should (equal (cl-subseq rows 0 2)
                             (list (list waiting 'waiting) (list working 'working))))
              (should-not (cl-set-exclusive-or (cl-subseq rows 2)
                                               (list (list plain 'untracked) (list sibling 'untracked))
                                               :test #'equal)))
            (cl-letf (((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                       (lambda (buffer) (setq visited buffer) (selected-window)))
                      ((symbol-function 'display-buffer) #'ignore))
              (claude-code-ide-show-session-status)
              (should (= claude-code-ide-session-status--urgent-count 1))
              (should (string-match-p "1 need you" (claude-code-ide-session-status--lighter)))
              (with-current-buffer claude-code-ide-session-status--buffer-name
                ;; One header per project, then its sessions
                (goto-char (point-min))
                (let ((ids nil))
                  (while (not (eobp))
                    (push (tabulated-list-get-id) ids)
                    (forward-line 1))
                  (setq ids (nreverse ids))
                  (should (equal (mapcar (lambda (id)
                                           (if (claude-code-ide-session-status--group-p id)
                                               (claude-code-ide-session-status--group-directory id)
                                             id))
                                         ids)
                                 (list "/tmp/status-b/" waiting sibling
                                       "/tmp/status-a/" working
                                       "/tmp/status-c/" plain))))
                (goto-char (point-min))
                (should (string-prefix-p "status-b  2 sessions"
                                         (buffer-substring-no-properties
                                          (point) (line-end-position))))
                ;; RET on the project line goes to its first session
                (claude-code-ide-session-status-visit))
              (should (eq visited (nth 1 buffers)))
              (setq visited nil)
              ;; Visiting selects a window, which changes the current buffer
              (with-current-buffer claude-code-ide-session-status--buffer-name
                (goto-char (point-min))
                (forward-line 2)
                (should (eq (tabulated-list-get-id) sibling))
                (claude-code-ide-session-status-visit))
              (should (eq visited (nth 3 buffers)))
              (setq visited nil)
              (claude-code-ide-visit-waiting-session)
              (should (eq visited (nth 1 buffers)))))
        (when claude-code-ide-session-status-mode
          (cl-letf (((symbol-function 'claude-code-ide-session-status--stop-timers) #'ignore))
            (claude-code-ide-session-status-mode -1)))
        (when claude-code-ide-session-registry-mode
          (claude-code-ide-session-registry-mode -1))
        (mapc #'kill-buffer buffers)
        (claude-code-ide-tests--clear-processes)))))

(provide 'claude-code-ide-session-status-tests)

;; Local Variables:
;; no-update-autoloads: t
;; autoload-compute-prefixes: nil
;; End:

;;; claude-code-ide-session-status-tests.el ends here
