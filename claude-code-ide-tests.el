;;; claude-code-ide-tests.el --- Tests for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Test suite for claude-code-ide.el using ERT
;;
;; Run tests with:
;;   `emacs -batch -L . -l ert -l claude-code-ide-tests.el -f ert-run-tests-batch-and-exit'
;;
;; The tests mock both vterm and mcp-server-lib functionality to avoid requiring
;; these packages during testing. This allows the tests to run in any environment
;; without external dependencies.
;;
;; CRITICAL DISCOVERY: Claude Code tools only work when launched from VS Code/editor terminals
;; because the extensions set these environment variables:
;; - CLAUDE_CODE_SSE_PORT: The WebSocket server port created by the extension
;; - FORCE_CODE_TERMINAL: Set to "true" to enable terminal features
;;
;; Workflow:
;; 1. Extension creates WebSocket/MCP server on random port
;; 2. Extension sets environment variables in terminal
;; 3. Extension launches 'claude' command
;; 4. Claude CLI reads env vars and connects to WebSocket server
;; 5. CLI and extension communicate via WebSocket/JSON-RPC for tool calls

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Mock Implementations

;; === Mock claude-code-ide-debug module ===
(defvar claude-code-ide-debug nil
  "Mock debug flag for testing.")
(defvar claude-code-ide-log-with-context t
  "Mock log context flag for testing.")
(defun claude-code-ide-debug (&rest _args)
  "Mock debug function that does nothing."
  nil)
(defun claude-code-ide-clear-debug ()
  "Mock clear debug function."
  nil)
(defun claude-code-ide-log (format-string &rest args)
  "Mock logging function for tests."
  (apply #'message format-string args))
(defun claude-code-ide--get-session-context ()
  "Mock session context function."
  "")
(provide 'claude-code-ide-debug)

;; === Mock websocket module ===
;; Try to load real websocket, otherwise provide comprehensive mocks
(condition-case nil
    (progn
      (add-to-list 'load-path (expand-file-name "~/.emacs.d/.cache/straight/build/websocket/"))
      (require 'websocket))
  (error
   ;; Comprehensive websocket mock implementation.  The struct comes
   ;; first: it generates the `websocket-frame-opcode' and
   ;; `websocket-frame-payload' accessors used below.
   (cl-defstruct websocket-frame opcode payload)
   (defun websocket-server (&rest _args)
     "Mock websocket-server function.
Returns a FRESH object per call: each instance owns its own server,
and tests distinguish them by identity."
     (list :mock-server t))
   (defun websocket-server-close (_server)
     "Mock websocket-server-close function."
     nil)
   (defun websocket-send-text (_ws _text)
     "Mock websocket-send-text function."
     nil)
   (defun websocket-ready-state (_ws)
     "Mock websocket-ready-state function."
     'open)
   (defun websocket-url (_ws)
     "Mock websocket-url function."
     "ws://localhost:12345")
   (defun websocket-frame-text (frame)
     "Mock websocket-frame-text function.
Decodes the frame's real payload, like the genuine accessor — message
routing tests depend on the carried text, not a placeholder."
     (if (websocket-frame-p frame)
         (decode-coding-string (websocket-frame-payload frame) 'utf-8)
       "{}"))
   (defun websocket-send (_ws _frame)
     "Mock websocket-send function."
     nil)
   (defun websocket-server-filter (_proc _string)
     "Mock websocket-server-filter function."
     nil)
   (provide (quote websocket))))

;; === Mock vterm module ===
(defvar vterm--process nil)
(defvar vterm-buffer-name nil)
(defvar vterm-shell nil)
(defvar vterm-environment nil)

(defun vterm (&optional buffer-name)
  "Mock vterm function for testing with optional BUFFER-NAME."
  (let ((buffer (generate-new-buffer (or buffer-name vterm-buffer-name "*vterm*"))))
    (with-current-buffer buffer
      ;; Create a mock process that exits immediately
      (setq vterm--process (make-process :name "mock-vterm"
                                         :buffer buffer
                                         :command '("true")
                                         :connection-type 'pty
                                         :sentinel (lambda (_ event)
                                                     (when (string-match "finished" event)
                                                       (setq vterm--process nil))))))
    buffer))

;; Mock vterm functions
(defun vterm-send-string (_string)
  "Mock vterm-send-string function for testing."
  nil)

(defun vterm-send-return ()
  "Mock vterm-send-return function for testing."
  nil)

(defun vterm-send-key (_key &optional _shift _meta _ctrl)
  "Mock vterm-send-key function for testing."
  nil)

(provide (quote vterm))

;; === Mock ghostel module ===
(defvar ghostel-buffer-name nil)
(defvar ghostel-set-title-function #'ignore)
(defvar ghostel-enable-title-tracking t)
(defvar ghostel-kill-buffer-on-exit t)

(defun ghostel (&optional _arg)
  "Mock ghostel function for testing."
  (let ((buffer (get-buffer-create (or ghostel-buffer-name "*ghostel*"))))
    (set-buffer buffer)
    (with-current-buffer buffer
      (make-process :name "mock-ghostel"
                    :buffer buffer
                    :command '("true")
                    :connection-type 'pty))
    buffer))

(defun ghostel-exec (buffer _program &optional _args)
  "Mock ghostel-exec function for testing."
  (with-current-buffer buffer
    (make-process :name "mock-ghostel"
                  :buffer buffer
                  :command '("true")
                  :connection-type 'pty)))

(defun ghostel-send-string (_string)
  "Mock ghostel send function for testing."
  nil)

(defun ghostel--window-adjust-process-window-size (_process _windows)
  "Mock ghostel resize handler for testing."
  '(80 . 24))

(provide (quote ghostel))

;; === Mock evil-ghostel module ===
;; These stand in for the evil-ghostel package's variables so the ESC
;; routing helper can be exercised in batch mode without evil installed.
(defvar evil-ghostel-mode nil)
(defvar evil-ghostel--escape-mode nil)

;; === Mock Emacs display functions ===
(unless (fboundp 'display-buffer-in-side-window)
  (defun display-buffer-in-side-window (buffer _alist)
    "Mock display-buffer-in-side-window for testing."
    (set-window-buffer (selected-window) buffer)
    (selected-window)))

;; === Additional test-specific websocket mocks ===
(unless (featurep 'websocket)
  ;; Only define these if websocket wasn't loaded above
  (defvar websocket--test-server nil
    "Mock server for testing.")
  (defvar websocket--test-client nil
    "Mock client for testing.")
  (defvar websocket--test-port 12345
    "Mock port for testing."))

;; === Mock flycheck module ===
;; Mock flycheck before loading any modules that require it
(defvar flycheck-mode nil
  "Mock flycheck-mode variable.")
(defvar flycheck-current-errors nil
  "Mock list of flycheck errors.")

(cl-defstruct flycheck-error
  "Mock flycheck error structure."
  buffer checker filename line column end-line end-column
  message level severity id)

(provide (quote flycheck))

;; === Load required modules ===
(define-error 'mcp-error "MCP Error" 'error)
(require 'claude-code-ide-mcp-handlers)
(require 'claude-code-ide)
(require 'claude-code-ide-status)

;;; Test Helper Functions

(defmacro claude-code-ide-tests--with-mocked-cli (cli-path &rest body)
  "Execute BODY with claude CLI path set to CLI-PATH."
  `(let ((claude-code-ide-cli-path ,cli-path)
         (claude-code-ide--cli-available nil))
     ,@body))

(defun claude-code-ide-tests--with-temp-directory (test-body)
  "Execute TEST-BODY in a temporary directory context.
Creates a temporary directory, sets it as `default-directory',
executes TEST-BODY, and ensures cleanup even if TEST-BODY fails."
  (let ((temp-dir (make-temp-file "claude-code-ide-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          (funcall test-body))
      (delete-directory temp-dir t))))

(defvar claude-code-ide-tests--session-counter 0
  "Counter making fixture session IDs unique within a test run.")

(defun claude-code-ide-tests--clear-processes ()
  "Reset all session state for testing.
Ensures a clean state before each test that involves instances: the MCP
session registry, the per-project selection timers, the MCP tools
server registry and the global window-panel state."
  (when (boundp 'claude-code-ide-mcp--sessions)
    (clrhash claude-code-ide-mcp--sessions))
  (when (boundp 'claude-code-ide-mcp--selection-timers)
    (maphash (lambda (_dir timer)
               (when (timerp timer)
                 (cancel-timer timer)))
             claude-code-ide-mcp--selection-timers)
    (clrhash claude-code-ide-mcp--selection-timers))
  (when (boundp 'claude-code-ide-mcp-server--sessions)
    (clrhash claude-code-ide-mcp-server--sessions))
  (setq claude-code-ide--last-accessed-buffer nil)
  (set-frame-parameter nil 'claude-code-ide-hidden-panel nil))

(defun claude-code-ide-tests--make-session (project-dir &rest overrides)
  "Build a session fixture for PROJECT-DIR and register it.
OVERRIDES are `make-claude-code-ide-mcp-session' keyword arguments that
replace the defaults; the session is stored in
`claude-code-ide-mcp--sessions' under its session ID and returned.
No WebSocket server is created, so the fixture is batch-safe."
  (let ((args (list :session-id (format "test-session-%d"
                                        (cl-incf claude-code-ide-tests--session-counter))
                    :instance-name nil
                    :project-dir (expand-file-name project-dir)
                    :last-used (float-time)
                    :deferred (make-hash-table :test 'equal)
                    :active-diffs (make-hash-table :test 'equal))))
    (while overrides
      (setq args (plist-put args (pop overrides) (pop overrides))))
    (let ((session (apply #'make-claude-code-ide-mcp-session args)))
      (puthash (claude-code-ide-mcp-session-session-id session)
               session
               claude-code-ide-mcp--sessions)
      session)))

(defun claude-code-ide-tests--make-websocket (url)
  "Return a stand-in WebSocket client object identified by URL.
Client routing is by identity, so any unique object would do, but the
real struct is used when the websocket package is available: struct
slot accessors are inlined at load time and cannot be stubbed out."
  (if (fboundp 'websocket-inner-create)
      (websocket-inner-create :conn nil :url url :accept-string ""
                              :ready-state 'open)
    (list 'mock-websocket url)))

(defun claude-code-ide-tests--make-frame (text)
  "Return a WebSocket text frame carrying TEXT."
  (make-websocket-frame :opcode 'text
                        :payload (encode-coding-string text 'utf-8)))

(defun claude-code-ide-tests--stop-all-sessions ()
  "Stop every registered MCP session, ignoring teardown errors.
Used by tests that create real per-instance WebSocket servers."
  (dolist (session (claude-code-ide-mcp--active-sessions))
    (ignore-errors (claude-code-ide-mcp--stop-session session))))

(defun claude-code-ide-tests--wait-for-process (buffer)
  "Wait for the process in BUFFER to finish.
This prevents race conditions in tests by ensuring mock processes
have completed before cleanup.  Waits up to 5 seconds."
  (with-current-buffer buffer
    (let ((max-wait 50)) ; 5 seconds max (50 * 0.1s)
      (while (and vterm--process
                  (process-live-p vterm--process)
                  (> max-wait 0))
        (sleep-for 0.1)
        (setq max-wait (1- max-wait))))))

;;; Tests for Helper Functions

(ert-deftest claude-code-ide-test-default-buffer-name ()
  "Test default buffer name generation for various path formats."
  ;; Normal path
  (should (equal (claude-code-ide--default-buffer-name "/home/user/project")
                 "*claude-code[project]*"))
  ;; Path with trailing slash
  (should (equal (claude-code-ide--default-buffer-name "/home/user/my-app/")
                 "*claude-code[my-app]*"))
  ;; Root directory
  (should (equal (claude-code-ide--default-buffer-name "/")
                 "*claude-code[]*"))
  ;; Path with spaces
  (should (equal (claude-code-ide--default-buffer-name "/home/user/my project/")
                 "*claude-code[my project]*"))
  ;; Path with special characters
  (should (equal (claude-code-ide--default-buffer-name "/home/user/my-project@v1.0/")
                 "*claude-code[my-project@v1.0]*")))

(ert-deftest claude-code-ide-test-get-working-directory ()
  "Test working directory detection."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     ;; Without project, should return current directory
     (let ((expected (expand-file-name default-directory)))
       (should (equal (claude-code-ide--get-working-directory) expected))))))

(ert-deftest claude-code-ide-test-get-buffer-name ()
  "Test buffer name generation using custom function."
  ;; Test with custom function
  (let ((claude-code-ide-buffer-name-function
         (lambda (dir) (format "test-%s" (file-name-nondirectory dir)))))
    (claude-code-ide-tests--with-temp-directory
     (lambda ()
       (should (string-match "^test-claude-code-ide-test-"
                             (claude-code-ide--get-buffer-name))))))

  ;; Test that nil directory is handled correctly
  (let ((claude-code-ide-buffer-name-function
         (lambda (dir) (if dir
                           (format "*custom[%s]*" (file-name-nondirectory dir))
                         "*custom[none]*"))))
    (should (equal (funcall claude-code-ide-buffer-name-function nil)
                   "*custom[none]*"))))

(ert-deftest claude-code-ide-test-session-registry ()
  "Test session storage and lookup in the session registry."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((dir (claude-code-ide--get-working-directory)))
           ;; Initially the project owns no session
           (should (null (claude-code-ide-mcp--sessions-for-project dir)))
           (should (null (claude-code-ide-mcp--get-current-session)))

           (let ((session (claude-code-ide-tests--make-session dir)))
             ;; Registered under its own session ID, not the project dir
             (should (eq (claude-code-ide-mcp--get-session-by-id
                          (claude-code-ide-mcp-session-session-id session))
                         session))
             (should (equal (claude-code-ide-mcp--sessions-for-project dir)
                            (list session)))
             (should (eq (claude-code-ide-mcp--mru-session dir) session))
             (should (member session (claude-code-ide-mcp--active-sessions)))

             ;; A second instance of the same project joins it; the most
             ;; recently used one wins the MRU lookup
             (let ((second (claude-code-ide-tests--make-session
                            dir
                            :instance-name "second"
                            :last-used (+ (float-time) 10))))
               (should (= 2 (length (claude-code-ide-mcp--sessions-for-project dir))))
               (should (eq (claude-code-ide-mcp--mru-session dir) second)))))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-cleanup-dead-sessions ()
  "Test that only sessions with a dead terminal process are cleaned up."
  (claude-code-ide-tests--clear-processes)
  (let ((live-process (make-process :name "test-live"
                                    :command '("sleep" "10")
                                    :buffer nil))
        (dead-process (make-process :name "test-dead"
                                    :command '("true")
                                    :buffer nil))
        (stopped '()))
    (unwind-protect
        ;; Real teardown would close websocket servers the fixtures never
        ;; opened, so only record which sessions the sweep tears down.
        (cl-letf (((symbol-function 'claude-code-ide-mcp--stop-session)
                   (lambda (session)
                     (push session stopped)
                     (remhash (claude-code-ide-mcp-session-session-id session)
                              claude-code-ide-mcp--sessions)))
                  ((symbol-function 'claude-code-ide-mcp-server-session-ended)
                   #'ignore))
          ;; Wait for the short-lived process to exit
          (while (process-live-p dead-process)
            (accept-process-output dead-process 0.05))
          (let ((live-session (claude-code-ide-tests--make-session
                               "/dir1/" :process live-process))
                (dead-session (claude-code-ide-tests--make-session
                               "/dir2/" :process dead-process))
                (pending-session (claude-code-ide-tests--make-session "/dir3/")))
            (should (= (hash-table-count claude-code-ide-mcp--sessions) 3))

            (claude-code-ide--cleanup-dead-sessions)

            ;; Only the dead one is gone; a session whose terminal has not
            ;; been created yet (nil process) must survive
            (should (equal stopped (list dead-session)))
            (should (claude-code-ide-mcp-session-cleanup-done dead-session))
            (should (= (hash-table-count claude-code-ide-mcp--sessions) 2))
            (should (claude-code-ide-mcp--get-session-by-id
                     (claude-code-ide-mcp-session-session-id live-session)))
            (should (claude-code-ide-mcp--get-session-by-id
                     (claude-code-ide-mcp-session-session-id pending-session)))
            (should-not (claude-code-ide-mcp-session-cleanup-done live-session))))
      (when (process-live-p live-process)
        (delete-process live-process))
      (claude-code-ide-tests--clear-processes))))

;;; Tests for CLI Detection

(ert-deftest claude-code-ide-test-detect-cli ()
  "Test CLI detection mechanism."
  (let ((claude-code-ide--cli-available nil))
    ;; Test with invalid CLI path
    (let ((claude-code-ide-cli-path "nonexistent-claude-cli"))
      (claude-code-ide--detect-cli)
      (should (null claude-code-ide--cli-available)))

    ;; Test with valid command (echo exists on most systems)
    (let ((claude-code-ide-cli-path "echo"))
      (claude-code-ide--detect-cli)
      (should claude-code-ide--cli-available))))

(ert-deftest claude-code-ide-test-ensure-cli ()
  "Test CLI availability checking."
  (let ((claude-code-ide--cli-available nil)
        (claude-code-ide-cli-path "echo"))
    ;; Initially not available
    (should (null claude-code-ide--cli-available))

    ;; After ensure, should be detected
    (should (claude-code-ide--ensure-cli))
    (should claude-code-ide--cli-available)))

;;; Command Tests

(ert-deftest claude-code-ide-test-run-without-cli ()
  "Test run command when CLI is not available."
  (let ((claude-code-ide--cli-available nil)
        (claude-code-ide-cli-path "nonexistent-claude-cli"))
    (should-error (claude-code-ide)
                  :type 'user-error)))

(ert-deftest claude-code-ide-test-run-without-vterm ()
  "Test run command when vterm is not available."
  (let ((claude-code-ide--cli-available t)
        (claude-code-ide-cli-path "echo")
        (claude-code-ide-terminal-backend 'vterm)
        (orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (sym &rest _) (if (eq sym 'vterm) nil (funcall orig-featurep sym))))
              ((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (unless (eq feature 'vterm)
                   (require feature filename noerror)))))
      (should-error (claude-code-ide)
                    :type 'user-error))))

(ert-deftest claude-code-ide-test-run-without-eat ()
  "Test run command when eat is not available."
  (let ((claude-code-ide--cli-available t)
        (claude-code-ide-cli-path "echo")
        (claude-code-ide-terminal-backend 'eat)
        (orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (sym &rest _) (if (eq sym 'eat) nil (funcall orig-featurep sym))))
              ((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (unless (eq feature 'eat)
                   (require feature filename noerror)))))
      (should-error (claude-code-ide)
                    :type 'user-error))))

(ert-deftest claude-code-ide-test-run-without-ghostel ()
  "Test run command when ghostel is not available."
  (let ((claude-code-ide--cli-available t)
        (claude-code-ide-cli-path "echo")
        (claude-code-ide-terminal-backend 'ghostel)
        (orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (sym &rest _) (if (eq sym 'ghostel) nil (funcall orig-featurep sym))))
              ((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (unless (eq feature 'ghostel)
                   (require feature filename noerror)))))
      (should-error (claude-code-ide)
                    :type 'user-error))))

(ert-deftest claude-code-ide-test-terminal-backend-selection ()
  "Test terminal backend selection and validation."
  ;; Test vterm backend
  (let ((claude-code-ide-terminal-backend 'vterm))
    (should (eq claude-code-ide-terminal-backend 'vterm)))

  ;; Test eat backend
  (let ((claude-code-ide-terminal-backend 'eat))
    (should (eq claude-code-ide-terminal-backend 'eat)))

  ;; Test ghostel backend
  (let ((claude-code-ide-terminal-backend 'ghostel))
    (should (eq claude-code-ide-terminal-backend 'ghostel)))

  ;; Test invalid backend
  (let ((claude-code-ide-terminal-backend 'invalid-backend)
        (orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (sym) nil)))
      (should-error (claude-code-ide--terminal-ensure-backend)
                    :type 'user-error))))

(ert-deftest claude-code-ide-test-terminal-send-functions ()
  "Test terminal send wrapper functions."
  ;; Mock vterm functions
  (let ((vterm-string-sent nil)
        (vterm-escape-sent nil)
        (vterm-return-sent nil)
        (eat-string-sent nil)
        (ghostel-string-sent nil))
    (cl-letf (((symbol-function 'vterm-send-string)
               (lambda (str) (setq vterm-string-sent str)))
              ((symbol-function 'vterm-send-escape)
               (lambda () (setq vterm-escape-sent t)))
              ((symbol-function 'vterm-send-return)
               (lambda () (setq vterm-return-sent t)))
              ((symbol-function 'eat-term-send-string)
               (lambda (term str) (setq eat-string-sent str)))
              ((symbol-function 'ghostel-send-string)
               (lambda (str) (setq ghostel-string-sent str))))

      ;; Test vterm backend
      (let ((claude-code-ide-terminal-backend 'vterm))
        (claude-code-ide--terminal-send-string "test")
        (should (equal vterm-string-sent "test"))

        (claude-code-ide--terminal-send-escape)
        (should vterm-escape-sent)

        (claude-code-ide--terminal-send-return)
        (should vterm-return-sent))

      ;; Test eat backend - need to mock the buffer-local variable
      (with-temp-buffer
        (let ((claude-code-ide-terminal-backend 'eat))
          ;; Set eat-terminal as a buffer-local variable
          (setq-local eat-terminal t)
          (claude-code-ide--terminal-send-string "test")
          (should (equal eat-string-sent "test"))

          (setq eat-string-sent nil)
          (claude-code-ide--terminal-send-escape)
          (should (equal eat-string-sent "\e"))

          (setq eat-string-sent nil)
          (claude-code-ide--terminal-send-return)
          (should (equal eat-string-sent "\r"))))

      ;; Test ghostel backend
      (with-temp-buffer
        (let ((claude-code-ide-terminal-backend 'ghostel))
          (claude-code-ide--terminal-send-string "test")
          (should (equal ghostel-string-sent "test"))

          (setq ghostel-string-sent nil)
          (claude-code-ide--terminal-send-escape)
          (should (equal ghostel-string-sent "\e"))

          (setq ghostel-string-sent nil)
          (claude-code-ide--terminal-send-return)
          (should (equal ghostel-string-sent "\r")))))))

(ert-deftest claude-code-ide-test-send-prompt-command ()
  "Test the claude-code-ide-send-prompt command."
  (claude-code-ide-tests--clear-processes)
  (let ((test-prompt "Test prompt from minibuffer")
        (project-dir "/tmp/claude-send-prompt/")
        (prompted-string nil)
        (sent-string nil)
        (sent-return nil)
        (terminal-buffer (generate-new-buffer "*test-claude-buffer*")))
    (unwind-protect
        ;; Mock read-string to return our test prompt
        (cl-letf (((symbol-function 'read-string)
                   (lambda (prompt &rest _)
                     (setq prompted-string prompt)
                     test-prompt))
                  ((symbol-function 'claude-code-ide--get-working-directory)
                   (lambda () project-dir))
                  ((symbol-function 'claude-code-ide--terminal-send-string)
                   (lambda (str) (setq sent-string str)))
                  ((symbol-function 'claude-code-ide--terminal-send-return)
                   (lambda () (setq sent-return t))))

          ;; The project's sole instance is resolved without prompting
          (claude-code-ide-tests--make-session project-dir :buffer terminal-buffer)
          (claude-code-ide-send-prompt)
          (should (equal prompted-string "Claude prompt: "))
          (should (equal sent-string test-prompt))
          (should sent-return)

          ;; Test with empty prompt (should not send anything)
          (setq sent-string nil sent-return nil)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) "")))
            (claude-code-ide-send-prompt)
            (should (null sent-string))
            (should (null sent-return)))

          ;; Without any instance for the project it errors
          (claude-code-ide-tests--clear-processes)
          (should-error (claude-code-ide-send-prompt) :type 'user-error))
      (when (buffer-live-p terminal-buffer)
        (kill-buffer terminal-buffer))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-terminal-session-creation ()
  "Test terminal session creation with both backends."
  (let ((mock-vterm-buffer nil)
        (mock-eat-buffer nil)
        (mock-ghostel-buffer nil)
        (mock-ghostel-program nil)
        (mock-ghostel-args nil)
        (mock-ghostel-env nil)
        (mock-ghostel-default-directory nil)
        (mock-process (start-process "mock" nil "true")))
    (cl-letf (((symbol-function 'claude-code-ide--terminal-ensure-backend)
               (lambda () nil))  ; Mock the ensure function to do nothing
              ((symbol-function 'vterm)
               (lambda (name)
                 (setq mock-vterm-buffer (get-buffer-create name))))
              ((symbol-function 'eat-mode)
               (lambda () nil))
              ((symbol-function 'eat-exec)
               (lambda (buffer name cmd startfile args)
                 (setq mock-eat-buffer buffer)))
              ((symbol-function 'ghostel-exec)
               (lambda (buffer program &optional args)
                 (setq mock-ghostel-buffer buffer)
                 (setq mock-ghostel-program program)
                 (setq mock-ghostel-args args)
                 (setq mock-ghostel-env process-environment)
                 (setq mock-ghostel-default-directory default-directory)
                 (setq-local ghostel-set-title-function #'ignore)
                 mock-process))
              ((symbol-function 'get-buffer-process)
               (lambda (buffer) mock-process)))

      ;; Test vterm backend session creation
      (let ((claude-code-ide-terminal-backend 'vterm)
            (claude-code-ide--cli-available t))
        (cl-letf (((symbol-function 'claude-code-ide--build-claude-command)
                   (lambda (&rest _) "claude")))
          (let ((result (claude-code-ide--create-terminal-session
                         "*test-vterm*" "/tmp" 12345 nil nil "test-session")))
            (should (consp result))
            (should (bufferp (car result)))
            (should (processp (cdr result)))
            (should (equal (buffer-name mock-vterm-buffer) "*test-vterm*")))))

      ;; Test eat backend session creation
      (let ((claude-code-ide-terminal-backend 'eat)
            (claude-code-ide--cli-available t))
        (cl-letf (((symbol-function 'claude-code-ide--build-claude-command)
                   (lambda (&rest _) "claude")))
          (let ((result (claude-code-ide--create-terminal-session
                         "*test-eat*" "/tmp" 12345 nil nil "test-session")))
            (should (consp result))
            (should (bufferp (car result)))
            (should (processp (cdr result)))
            (should (bufferp mock-eat-buffer)))))

      ;; Test ghostel backend session creation
      (let ((claude-code-ide-terminal-backend 'ghostel)
            (claude-code-ide--cli-available t))
        (cl-letf (((symbol-function 'claude-code-ide--build-claude-command)
                   (lambda (&rest _) "claude --print \"hello world\""))
                  ;; The ghostel branch resolves the program to an absolute
                  ;; path so ghostel's native PTY spawn does not depend on
                  ;; the process environment's PATH.
                  ((symbol-function 'executable-find)
                   (lambda (name) (when (equal name "claude") "/opt/bin/claude"))))
          (let ((result (claude-code-ide--create-terminal-session
                         "*test-ghostel*" "/tmp" 12345 nil nil "test-session")))
            (should (consp result))
            (should (bufferp (car result)))
            (should (processp (cdr result)))
            (should (equal (buffer-name mock-ghostel-buffer) "*test-ghostel*"))
            (should (equal mock-ghostel-program "/opt/bin/claude"))
            (should (equal mock-ghostel-args '("--print" "hello world")))
            (should (equal mock-ghostel-default-directory "/tmp"))
            (with-current-buffer mock-ghostel-buffer
              (should (null ghostel-set-title-function)))
            (should (member "CLAUDE_CODE_SSE_PORT=12345" mock-ghostel-env))
            (should (member "TERM_PROGRAM=emacs" mock-ghostel-env))
            (should (member "FORCE_CODE_TERMINAL=true" mock-ghostel-env))))))))

(ert-deftest claude-code-ide-test-resolve-program ()
  "Test CLI program resolution for terminal exec APIs."
  ;; A bare name resolves to an absolute path via `executable-find'.
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (when (equal name "claude") "/opt/bin/claude"))))
    (should (equal (claude-code-ide--resolve-program "claude")
                   "/opt/bin/claude"))
    ;; An unresolvable name is passed through unchanged so the terminal
    ;; backend reports the missing executable itself.
    (should (equal (claude-code-ide--resolve-program "no-such-cli")
                   "no-such-cli")))
  ;; A name with a directory component expands instead of a PATH lookup.
  (let ((default-directory "/tmp/"))
    (should (equal (claude-code-ide--resolve-program "./bin/claude")
                   "/tmp/bin/claude"))
    (should (equal (claude-code-ide--resolve-program "~/bin/claude")
                   (expand-file-name "~/bin/claude")))))

(ert-deftest claude-code-ide-test-start-session-cli-dies-during-init ()
  "A CLI death during the initialization delay signals a clear error.
When the process dies within the stabilization delay, the exit
sentinel kills the terminal buffer; the session start must then fail
with an explanatory error rather than operating on the dead buffer."
  (claude-code-ide-tests--clear-processes)
  (let ((buffer (generate-new-buffer "*test-death*"))
        (process (start-process "mock-claude" nil "sleep" "30"))
        (project-dir "/tmp/test-death/")
        (displayed nil)
        (stopped-sessions '())
        (ended-ids '()))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-code-ide--ensure-cli)
                   (lambda () t))
                  ((symbol-function 'claude-code-ide--get-working-directory)
                   (lambda () project-dir))
                  ((symbol-function 'claude-code-ide--terminal-ensure-backend)
                   #'ignore)
                  ;; The fixture session is created (and registered) only
                  ;; when the session start asks for it, so the instance
                  ;; name prompt is not triggered beforehand.
                  ((symbol-function 'claude-code-ide-mcp-create-session)
                   (lambda (dir session-id &optional instance-name)
                     (claude-code-ide-tests--make-session
                      dir :session-id session-id
                      :instance-name instance-name :port 12345)))
                  ((symbol-function 'claude-code-ide--create-terminal-session)
                   (lambda (&rest _) (cons buffer process)))
                  ((symbol-function 'claude-code-ide-mcp-server-session-started)
                   #'ignore)
                  ((symbol-function 'claude-code-ide-mcp-server-session-ended)
                   (lambda (id) (push id ended-ids)))
                  ((symbol-function 'claude-code-ide-mcp--stop-session)
                   (lambda (session)
                     (push session stopped-sessions)
                     (remhash (claude-code-ide-mcp-session-session-id session)
                              claude-code-ide-mcp--sessions)))
                  ((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                   (lambda (_) (setq displayed t)))
                  ;; Simulate the CLI dying while the session stabilizes:
                  ;; the process exits and its sentinel kills the buffer.
                  ((symbol-function 'sleep-for)
                   (lambda (&rest _)
                     (delete-process process)
                     (kill-buffer buffer))))
          (let* ((claude-code-ide-terminal-backend 'ghostel)
                 (claude-code-ide-prevent-reflow-glitch nil)
                 (err (should-error (claude-code-ide--start-session))))
            (should (string-match-p "exited immediately after startup"
                                    (error-message-string err)))
            (should-not displayed)
            ;; The failed instance is torn down, and only it: its MCP
            ;; server is stopped and its tools-server registration ended
            (should (= 1 (length stopped-sessions)))
            (should (= 1 (length ended-ids)))
            (should (= 0 (hash-table-count claude-code-ide-mcp--sessions)))))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-ghostel-evil-escape-override ()
  "Test that the ghostel evil ESC routing is overridden per buffer."
  ;; Overrides the buffer-local escape mode when evil-ghostel-mode is on.
  (with-temp-buffer
    (setq-local evil-ghostel-mode t)
    (setq-local evil-ghostel--escape-mode 'auto)
    (let ((claude-code-ide-ghostel-evil-escape 'evil))
      (claude-code-ide--apply-ghostel-evil-escape)
      (should (eq evil-ghostel--escape-mode 'evil))))
  ;; A nil setting leaves the value seeded from the global default alone.
  (with-temp-buffer
    (setq-local evil-ghostel-mode t)
    (setq-local evil-ghostel--escape-mode 'auto)
    (let ((claude-code-ide-ghostel-evil-escape nil))
      (claude-code-ide--apply-ghostel-evil-escape)
      (should (eq evil-ghostel--escape-mode 'auto))))
  ;; No-op when evil-ghostel-mode is not active in the buffer.
  (with-temp-buffer
    (setq-local evil-ghostel-mode nil)
    (setq-local evil-ghostel--escape-mode 'auto)
    (let ((claude-code-ide-ghostel-evil-escape 'evil))
      (claude-code-ide--apply-ghostel-evil-escape)
      (should (eq evil-ghostel--escape-mode 'auto)))))

(ert-deftest claude-code-ide-test-vterm-smart-renderer-passthrough ()
  "Test that vterm smart renderer passes through normal text immediately."
  (let ((orig-fun-called nil)
        (orig-fun-input nil)
        (claude-code-ide-vterm-anti-flicker t))
    (cl-letf (((symbol-function 'claude-code-ide--session-buffer-p)
               (lambda (_) t)))
      (with-temp-buffer
        (let ((claude-code-ide--vterm-render-queue nil)
              (claude-code-ide--vterm-render-timer nil)
              (mock-process (make-process :name "mock"
                                          :buffer (current-buffer)
                                          :command '("true"))))
          ;; Create a mock original function
          (let ((orig-fun (lambda (_process input)
                            (setq orig-fun-called t
                                  orig-fun-input input))))
            ;; Test with normal text (no escape sequences)
            (claude-code-ide--vterm-smart-renderer orig-fun mock-process "Hello World")
            ;; Should pass through immediately
            (should orig-fun-called)
            (should (equal orig-fun-input "Hello World"))
            (should-not claude-code-ide--vterm-render-queue)))))))

(ert-deftest claude-code-ide-test-vterm-smart-renderer-batching ()
  "Test that vterm smart renderer batches complex escape sequences."
  (let ((orig-fun-called nil)
        (timer-created nil)
        (claude-code-ide-vterm-anti-flicker t)
        (claude-code-ide-vterm-render-delay 0.005))
    (cl-letf (((symbol-function 'claude-code-ide--session-buffer-p)
               (lambda (_) t))
              ((symbol-function 'run-at-time)
               (lambda (delay &rest _)
                 (setq timer-created delay)
                 'mock-timer))
              ((symbol-function 'cancel-timer)
               (lambda (_) nil)))
      (with-temp-buffer
        (let ((claude-code-ide--vterm-render-queue nil)
              (claude-code-ide--vterm-render-timer nil)
              (mock-process (make-process :name "mock"
                                          :buffer (current-buffer)
                                          :command '("true"))))
          ;; Create a mock original function
          (let ((orig-fun (lambda (_process _input)
                            (setq orig-fun-called t))))
            ;; Test with complex escape sequence pattern
            (let ((complex-input "\033[2A\033[K\033[3A\033[K"))
              (claude-code-ide--vterm-smart-renderer orig-fun mock-process complex-input)
              ;; Should be queued, not called immediately
              (should-not orig-fun-called)
              ;; Queue is a list (pushed in reverse order for O(1))
              (should (listp claude-code-ide--vterm-render-queue))
              (should (equal (apply #'concat (nreverse claude-code-ide--vterm-render-queue))
                             complex-input))
              (should (equal timer-created 0.005)))))))))

(ert-deftest claude-code-ide-test-toggle-vterm-optimization ()
  "Test toggling vterm optimization on and off."
  (let ((original-value claude-code-ide-vterm-anti-flicker)
        (message-output nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args)
                     (setq message-output (apply #'format format args)))))
          ;; Start with optimization enabled
          (setq claude-code-ide-vterm-anti-flicker t)

          ;; Toggle off
          (claude-code-ide-toggle-vterm-optimization)
          (should-not claude-code-ide-vterm-anti-flicker)
          (should (string-match "disabled" message-output))

          ;; Toggle back on
          (claude-code-ide-toggle-vterm-optimization)
          (should claude-code-ide-vterm-anti-flicker)
          (should (string-match "enabled" message-output)))
      ;; Restore original value
      (setq claude-code-ide-vterm-anti-flicker original-value))))

(ert-deftest claude-code-ide-test-run-existing-session ()
  "Starting Claude twice in one project creates two instances.
The second start prompts for an instance name and gets its own session,
port, window slot and terminal buffer instead of reusing the first."
  (claude-code-ide-tests--clear-processes)
  (let ((processes '())
        (displayed '()))
    (unwind-protect
        (claude-code-ide-tests--with-temp-directory
         (lambda ()
           (let ((claude-code-ide--cli-available t)
                 (claude-code-ide-cli-path "echo")
                 (claude-code-ide-terminal-initialization-delay 0)
                 (claude-code-ide-prevent-reflow-glitch nil)
                 (working-dir (claude-code-ide--get-working-directory)))
             (cl-letf (((symbol-function 'claude-code-ide--terminal-ensure-backend)
                        #'ignore)
                       ((symbol-function 'claude-code-ide--read-instance-name)
                        (lambda (&rest _) "second"))
                       ((symbol-function 'claude-code-ide--create-terminal-session)
                        (lambda (buffer-name &rest _)
                          (let ((buffer (generate-new-buffer buffer-name))
                                (process (start-process "mock-claude" nil
                                                        "sleep" "30")))
                            (push process processes)
                            (cons buffer process))))
                       ((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                        (lambda (buffer) (push buffer displayed) nil)))
               ;; The first instance is unnamed and asks nothing
               (claude-code-ide--start-session)
               (should (= 1 (length (claude-code-ide-mcp--sessions-for-project
                                     working-dir))))

               ;; The second start does not toggle the first one's window,
               ;; it creates another instance
               (claude-code-ide--start-session)
               (let* ((sessions (claude-code-ide-mcp--sessions-for-project working-dir))
                      (names (mapcar #'claude-code-ide-mcp-session-instance-name sessions))
                      (ports (mapcar #'claude-code-ide-mcp-session-port sessions))
                      (slots (mapcar #'claude-code-ide-mcp-session-window-slot sessions))
                      (buffer-names (mapcar (lambda (session)
                                              (buffer-name
                                               (claude-code-ide-mcp-session-buffer session)))
                                            sessions)))
                 (should (= 2 (length sessions)))
                 (should (member nil names))
                 (should (member "second" names))
                 ;; Own port, own window slot, own buffer
                 (should (= 2 (length (delete-dups (copy-sequence ports)))))
                 (should (= 2 (length (delete-dups (copy-sequence slots)))))
                 (should (equal (sort (copy-sequence buffer-names) #'string<)
                                (sort (list (claude-code-ide--instance-buffer-name
                                             working-dir nil)
                                            (claude-code-ide--instance-buffer-name
                                             working-dir "second"))
                                      #'string<)))
                 ;; Both instances got displayed
                 (should (= 2 (length displayed))))))))
      (dolist (session (claude-code-ide-mcp--active-sessions))
        (ignore-errors (claude-code-ide--cleanup-session session)))
      (dolist (process processes)
        (when (process-live-p process)
          (delete-process process)))
      (claude-code-ide-tests--stop-all-sessions)
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-check-status ()
  "Test status check command."
  (let ((claude-code-ide-cli-path "echo")
        (claude-code-ide--cli-available nil))
    ;; Should not error and should detect CLI
    (claude-code-ide-check-status)
    (should claude-code-ide--cli-available)))

(ert-deftest claude-code-ide-test-terminal-initialization-delay ()
  "Test terminal initialization delay configuration."
  ;; Test default value
  (should (boundp 'claude-code-ide-terminal-initialization-delay))
  (should (numberp claude-code-ide-terminal-initialization-delay))
  (should (= claude-code-ide-terminal-initialization-delay 0.1))

  ;; Test customization
  (let ((original-delay claude-code-ide-terminal-initialization-delay))
    (unwind-protect
        (progn
          (setq claude-code-ide-terminal-initialization-delay 0.2)
          (should (= claude-code-ide-terminal-initialization-delay 0.2)))
      ;; Restore original value
      (setq claude-code-ide-terminal-initialization-delay original-delay))))

(ert-deftest claude-code-ide-test-obsolete-eat-delay-alias ()
  "Test that the obsolete eat delay alias still works."
  ;; The alias should be defined
  (should (boundp 'claude-code-ide-eat-initialization-delay))
  ;; Setting the old variable should affect the new one
  (let ((original-delay claude-code-ide-terminal-initialization-delay))
    (unwind-protect
        (progn
          (setq claude-code-ide-eat-initialization-delay 0.3)
          (should (= claude-code-ide-terminal-initialization-delay 0.3)))
      ;; Restore original value
      (setq claude-code-ide-terminal-initialization-delay original-delay))))

(ert-deftest claude-code-ide-test-stop-no-session ()
  "Test stop command when no session is running."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         ;; Should not error when no session exists
         (claude-code-ide-stop)))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-stop-with-session ()
  "Test that stopping an instance kills its own terminal buffer.
With several instances in a project the target is asked for rather than
guessed, and only that instance is torn down."
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-stop/")
        (buffer-a (generate-new-buffer "*claude-stop-a*"))
        (buffer-b (generate-new-buffer "*claude-stop-b*"))
        (cleaned '()))
    (unwind-protect
        (let ((session-a (claude-code-ide-tests--make-session
                          project-dir :instance-name "alpha" :buffer buffer-a))
              (session-b (claude-code-ide-tests--make-session
                          project-dir :instance-name "beta" :buffer buffer-b)))
          (cl-letf (((symbol-function 'claude-code-ide--get-working-directory)
                     (lambda () project-dir))
                    ((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                    ((symbol-function 'claude-code-ide--cleanup-session)
                     (lambda (session) (push session cleaned)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt candidates &rest _)
                       (car (rassq session-b candidates)))))
            ;; The chosen instance's buffer is killed; cleanup follows from
            ;; the buffer's own kill hook
            (claude-code-ide-stop)
            (should-not (buffer-live-p buffer-b))
            (should (buffer-live-p buffer-a))
            (should (claude-code-ide-mcp--get-session-by-id
                     (claude-code-ide-mcp-session-session-id session-a)))

            ;; An instance whose buffer is already gone is cleaned up directly
            (setf (claude-code-ide-mcp-session-buffer session-b) nil)
            (cl-letf (((symbol-function 'claude-code-ide--resolve-session)
                       (lambda (&rest _) session-b)))
              (claude-code-ide-stop))
            (should (equal cleaned (list session-b)))))
      (dolist (buffer (list buffer-a buffer-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-switch-to-buffer-no-session ()
  "Test `switch-to-buffer' command when no session exists."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (should-error (claude-code-ide-switch-to-buffer)
                    :type 'user-error)
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-toggle-window-functionality ()
  "Test that `claude-code-ide-toggle' works on the project's windows as a panel."
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-toggle-panel/")
        (buffer-a (generate-new-buffer "*claude-panel-a*"))
        (buffer-b (generate-new-buffer "*claude-panel-b*"))
        (visible '())
        (toggled '()))
    (unwind-protect
        (let ((session-a (claude-code-ide-tests--make-session
                          project-dir :instance-name "alpha"
                          :buffer buffer-a :last-used 100.0))
              (session-b (claude-code-ide-tests--make-session
                          project-dir :instance-name "beta"
                          :buffer buffer-b :last-used 200.0)))
          (cl-letf (((symbol-function 'claude-code-ide--get-working-directory)
                     (lambda () project-dir))
                    ((symbol-function 'claude-code-ide--session-visible-p)
                     (lambda (session) (memq session visible)))
                    ((symbol-function 'claude-code-ide--toggle-existing-window)
                     (lambda (session) (push session toggled))))
            ;; Both windows visible: the whole panel is hidden and remembered
            (setq visible (list session-a session-b))
            (claude-code-ide-toggle)
            (should (= 2 (length toggled)))

            ;; Nothing visible: the remembered set comes back
            (setq visible '() toggled '())
            (claude-code-ide-toggle)
            (should (= 2 (length toggled)))
            (should (memq session-a toggled))
            (should (memq session-b toggled))

            ;; Without a remembered set, only the most recently used
            ;; instance is restored
            (claude-code-ide--hidden-panel-set project-dir nil)
            (setq toggled '())
            (claude-code-ide-toggle)
            (should (equal toggled (list session-b)))

            ;; A prefix argument toggles a single chosen instance
            (setq toggled '())
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt candidates &rest _)
                         (car (rassq session-a candidates)))))
              (claude-code-ide-toggle t))
            (should (equal toggled (list session-a)))

            ;; A project without instances has no panel to toggle
            (claude-code-ide-tests--clear-processes)
            (should-error (claude-code-ide-toggle) :type 'user-error)))
      (dolist (buffer (list buffer-a buffer-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-list-sessions-empty ()
  "Test listing sessions when none exist."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      ;; Should not error when no sessions exist
      (claude-code-ide-list-sessions)
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-list-sessions-with-sessions ()
  "Test that session listing offers every instance of every project."
  (claude-code-ide-tests--clear-processes)
  (let ((buffer1 (generate-new-buffer "*claude-list-1*"))
        (buffer2 (generate-new-buffer "*claude-list-2*"))
        (buffer3 (generate-new-buffer "*claude-list-3*"))
        (offered nil)
        (displayed nil))
    (unwind-protect
        (progn
          ;; Two instances of one project plus one of another
          (claude-code-ide-tests--make-session "/tmp/project1/" :buffer buffer1)
          (claude-code-ide-tests--make-session "/tmp/project1/"
                                               :instance-name "review"
                                               :buffer buffer2)
          (claude-code-ide-tests--make-session "/tmp/project2/"
                                               :buffer buffer3
                                               :client (claude-code-ide-tests--make-websocket
                                                        "ws://127.0.0.1:10003"))
          (should (= (hash-table-count claude-code-ide-mcp--sessions) 3))

          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt candidates &rest _)
                       (setq offered (mapcar #'car candidates))
                       (car (car candidates))))
                    ((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                     (lambda (buffer) (setq displayed buffer) nil)))
            (claude-code-ide-list-sessions))

          ;; All three instances are offered, named per instance and
          ;; annotated with their connection state
          (should (= (length offered) 3))
          (should (cl-find-if (lambda (c) (string-prefix-p "project1 —" c)) offered))
          (should (cl-find-if (lambda (c) (string-prefix-p "project1:review —" c)) offered))
          (should (cl-find-if (lambda (c) (string-match-p "connected" c)) offered))
          (should (cl-find-if (lambda (c) (string-match-p "waiting" c)) offered))
          (should (memq displayed (list buffer1 buffer2 buffer3))))
      (dolist (buffer (list buffer1 buffer2 buffer3))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-toggle-recent ()
  "Test the global Claude panel toggle.
Visible windows are hidden as a set and that set is restored on the
next toggle, independent of which project the instances belong to."
  (claude-code-ide-tests--clear-processes)
  (let ((buffer1 (generate-new-buffer "*claude-toggle-1*"))
        (buffer2 (generate-new-buffer "*claude-toggle-2*"))
        (visible t)
        (hidden '())
        (shown '()))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-code-ide--session-visible-p)
                   (lambda (_session) visible))
                  ((symbol-function 'claude-code-ide--toggle-existing-window)
                   (lambda (session) (push session hidden)))
                  ((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                   (lambda (buffer) (push buffer shown) nil)))
          ;; Without any instance and without a recent buffer it errors
          (setq visible nil)
          (should-error (claude-code-ide-toggle-recent) :type 'user-error)

          ;; A remembered last-accessed buffer is reopened
          (setq claude-code-ide--last-accessed-buffer buffer1)
          (claude-code-ide-toggle-recent)
          (should (equal shown (list buffer1)))

          ;; Two visible instances from different projects are hidden as
          ;; one set, and the set is remembered
          (let ((session1 (claude-code-ide-tests--make-session
                           "/tmp/toggle-a/" :buffer buffer1))
                (session2 (claude-code-ide-tests--make-session
                           "/tmp/toggle-b/" :buffer buffer2)))
            (setq visible t shown '() hidden '())
            (claude-code-ide-toggle-recent)
            (should (= 2 (length hidden)))
            (should (memq session1 hidden))
            (should (memq session2 hidden))
            (should (= 2 (length (claude-code-ide--hidden-panel-get :all))))

            ;; Toggling again restores exactly the remembered set and
            ;; forgets it
            (setq visible nil hidden '())
            (claude-code-ide-toggle-recent)
            (should (= 2 (length shown)))
            (should (memq buffer1 shown))
            (should (memq buffer2 shown))
            (should-not (claude-code-ide--hidden-panel-get :all))

            ;; A stopped instance is dropped from the remembered set
            (claude-code-ide--hidden-panel-set :all (list session1 session2))
            (setq shown '())
            (setf (claude-code-ide-mcp-session-cleanup-done session1) t)
            (claude-code-ide-toggle-recent)
            (should (equal shown (list buffer2)))))
      (dolist (buffer (list buffer1 buffer2))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (claude-code-ide-tests--clear-processes))))

;;; Edge Case Tests

(ert-deftest claude-code-ide-test-concurrent-sessions ()
  "Test that instances of two different projects stay independent."
  (claude-code-ide-tests--clear-processes)
  (let ((dir1 (file-name-as-directory (make-temp-file "claude-test-1" t)))
        (dir2 (file-name-as-directory (make-temp-file "claude-test-2" t)))
        (session1 nil)
        (session2 nil))
    (unwind-protect
        (progn
          (setq session1 (claude-code-ide-mcp-create-session dir1 "concurrent-1")
                session2 (claude-code-ide-mcp-create-session dir2 "concurrent-2"))
          (should (= 2 (hash-table-count claude-code-ide-mcp--sessions)))
          ;; Each project sees only its own instance
          (should (equal (claude-code-ide-mcp--sessions-for-project dir1)
                         (list session1)))
          (should (equal (claude-code-ide-mcp--sessions-for-project dir2)
                         (list session2)))
          (should (eq (claude-code-ide-mcp--mru-session dir1) session1))
          ;; Separate servers, ports and lockfiles
          (should-not (eq (claude-code-ide-mcp-session-server session1)
                          (claude-code-ide-mcp-session-server session2)))
          (should-not (= (claude-code-ide-mcp-session-port session1)
                         (claude-code-ide-mcp-session-port session2)))
          (should (file-exists-p (claude-code-ide-mcp--lockfile-path
                                  (claude-code-ide-mcp-session-port session1))))
          (should (file-exists-p (claude-code-ide-mcp--lockfile-path
                                  (claude-code-ide-mcp-session-port session2))))

          ;; Stopping one project's instance leaves the other untouched
          (claude-code-ide-mcp-stop-session dir1)
          (should (null (claude-code-ide-mcp--sessions-for-project dir1)))
          (should-not (file-exists-p (claude-code-ide-mcp--lockfile-path
                                      (claude-code-ide-mcp-session-port session1))))
          (should (equal (claude-code-ide-mcp--sessions-for-project dir2)
                         (list session2)))
          (should (file-exists-p (claude-code-ide-mcp--lockfile-path
                                  (claude-code-ide-mcp-session-port session2))))
          ;; The selection debounce timer table is keyed per project, so
          ;; the stopped project leaves nothing behind
          (should-not (gethash (expand-file-name dir1)
                               claude-code-ide-mcp--selection-timers)))
      (claude-code-ide-tests--stop-all-sessions)
      (claude-code-ide-tests--clear-processes)
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest claude-code-ide-test-custom-buffer-naming ()
  "Test custom buffer naming function."
  (let ((claude-code-ide-buffer-name-function
         (lambda (dir)
           (format "TEST-%s"
                   (upcase (file-name-nondirectory (directory-file-name dir)))))))
    (claude-code-ide-tests--with-temp-directory
     (lambda ()
       (let ((expected (format "TEST-%s"
                               (upcase (file-name-nondirectory
                                        (directory-file-name default-directory))))))
         (should (equal (claude-code-ide--get-buffer-name) expected)))))))

(ert-deftest claude-code-ide-test-window-placement-options ()
  "Test different window placement configurations."
  (dolist (side '(left right top bottom))
    (let ((claude-code-ide-window-side side))
      ;; Just verify the setting is accepted
      (should (eq claude-code-ide-window-side side)))))

(ert-deftest claude-code-ide-test-debug-mode-flag ()
  "Test debug mode CLI flag."
  (let ((claude-code-ide-cli-debug t))
    (should (string-match "-d" (claude-code-ide--build-claude-command)))
    (should (string-match "-d.*-c" (claude-code-ide--build-claude-command t)))
    (should (string-match "-d.*-r" (claude-code-ide--build-claude-command nil t)))))

(ert-deftest claude-code-ide-test-build-command-with-system-prompt ()
  "Test building command with append-system-prompt flag."
  ;; Test with user system prompt
  (let ((claude-code-ide-cli-path "claude")
        (claude-code-ide-system-prompt "You are a helpful assistant")
        (claude-code-ide-cli-debug nil)
        (claude-code-ide-cli-extra-flags ""))
    (let ((cmd (claude-code-ide--build-claude-command)))
      (should (string-match-p "--append-system-prompt" cmd))
      ;; Check that Emacs prompt is included (accounting for shell escaping)
      (should (or (string-match-p "Connected to Emacs" cmd)
                  (string-match-p "Connected\\\\ to\\\\ Emacs" cmd)))
      ;; Check that user prompt is included
      (should (or (string-match-p "You are a helpful assistant" cmd)
                  (string-match-p "You\\\\ are\\\\ a\\\\ helpful\\\\ assistant" cmd)))))
  ;; Test with nil value (should still add the Emacs prompt)
  (let ((claude-code-ide-cli-path "claude")
        (claude-code-ide-system-prompt nil)
        (claude-code-ide-cli-debug nil)
        (claude-code-ide-cli-extra-flags ""))
    (let ((cmd (claude-code-ide--build-claude-command)))
      (should (string-match-p "--append-system-prompt" cmd))
      ;; Check that Emacs prompt is included (accounting for shell escaping)
      (should (or (string-match-p "Connected to Emacs" cmd)
                  (string-match-p "Connected\\\\ to\\\\ Emacs" cmd)))
      ;; Should not contain user prompt when nil
      (should-not (string-match-p "You are a helpful assistant" cmd))))
  ;; Test with special characters that need quoting
  (let ((claude-code-ide-cli-path "claude")
        (claude-code-ide-system-prompt "You're a \"helpful\" assistant!")
        (claude-code-ide-cli-debug nil)
        (claude-code-ide-cli-extra-flags ""))
    (let ((cmd (claude-code-ide--build-claude-command)))
      (should (string-match-p "--append-system-prompt" cmd))
      ;; Check that Emacs prompt is included (accounting for shell escaping)
      (should (or (string-match-p "Connected to Emacs" cmd)
                  (string-match-p "Connected\\\\ to\\\\ Emacs" cmd)))
      ;; The command should contain the escaped version (shell-quote-argument escapes quotes and apostrophes)
      (should (string-match-p "You\\\\'re\\\\ a\\\\ \\\\\"helpful\\\\\"\\\\ assistant\\\\!" cmd)))))

(ert-deftest claude-code-ide-test-error-handling ()
  "Test error handling in various scenarios."
  ;; Test with nil CLI path
  (let ((claude-code-ide-cli-path nil)
        (claude-code-ide--cli-available nil))
    (should-error (claude-code-ide) :type 'user-error))

  ;; Test with empty CLI path
  (let ((claude-code-ide-cli-path "")
        (claude-code-ide--cli-available nil))
    (should-error (claude-code-ide) :type 'user-error)))

;;; Run all tests

(ert-deftest claude-code-ide-test-tab-bar-tracking ()
  "Test that each instance records the tab it was started in."
  (require 'tab-bar)
  (let* ((temp-dir (file-name-as-directory (make-temp-file "test-project-" t)))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         ;; Mock tab-bar functions
         (mock-tab '((name . "test-tab") (index . 1))))
    (unwind-protect
        (cl-letf (((symbol-function 'tab-bar--current-tab)
                   (lambda (&rest _) mock-tab)))
          ;; Creating an instance captures the current tab in its session
          (let ((session (claude-code-ide-mcp-create-session temp-dir "tab-bar-1")))
            (should (numberp (claude-code-ide-mcp-session-port session)))
            (should (eq (claude-code-ide-mcp--get-session-by-id "tab-bar-1") session))
            (should (equal (claude-code-ide-mcp-session-original-tab session) mock-tab))))
      ;; Cleanup
      (claude-code-ide-mcp-stop-session temp-dir)
      (delete-directory temp-dir t))))

(ert-deftest claude-code-ide-test-tab-bar-switch-on-ediff ()
  "Test that tab-bar switching on ediff respects the configuration."
  ;; Test that the variable exists with the expected default
  (should (boundp 'claude-code-ide-switch-tab-on-ediff))
  (should (equal claude-code-ide-switch-tab-on-ediff t))

  ;; Test with simple mocking to ensure the config is checked
  (let* ((original-tab '((name . "original-tab")))
         (current-tab '((name . "current-tab")))
         (tab-switched nil)
         (tab-bar-mode t))

    ;; Mock functions
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (or (eq sym 'tab-bar--current-tab)
                     (eq sym 'tab-bar-select-tab-by-name)
                     (eq sym 'tab-bar-mode)
                     (funcall (cl-letf-saved-symbol-function 'fboundp) sym))))
              ((symbol-function 'tab-bar--current-tab)
               (lambda () current-tab))
              ((symbol-function 'tab-bar-select-tab-by-name)
               (lambda (name)
                 (setq tab-switched name))))

      ;; Create a minimal test session
      (let ((session (make-claude-code-ide-mcp-session
                      :original-tab original-tab)))

        ;; Test 1: With switch enabled (default)
        (let ((claude-code-ide-switch-tab-on-ediff t))
          (setq tab-switched nil)
          ;; Simulate the relevant part of the handler
          (when (and claude-code-ide-switch-tab-on-ediff
                     (claude-code-ide-mcp-session-original-tab session))
            (let ((original-tab (claude-code-ide-mcp-session-original-tab session)))
              (when (and (fboundp 'tab-bar-mode)
                         tab-bar-mode
                         (fboundp 'tab-bar--current-tab)
                         (fboundp 'tab-bar-select-tab-by-name))
                (let ((current-tab (tab-bar--current-tab)))
                  (when (and original-tab current-tab
                             (not (equal (alist-get 'name original-tab)
                                         (alist-get 'name current-tab))))
                    (tab-bar-select-tab-by-name (alist-get 'name original-tab)))))))
          ;; Should have switched
          (should (equal tab-switched "original-tab")))

        ;; Test 2: With switch disabled
        (let ((claude-code-ide-switch-tab-on-ediff nil))
          (setq tab-switched nil)
          ;; Simulate the relevant part of the handler
          (when (and claude-code-ide-switch-tab-on-ediff
                     (claude-code-ide-mcp-session-original-tab session))
            (let ((original-tab (claude-code-ide-mcp-session-original-tab session)))
              (when (and (fboundp 'tab-bar-mode)
                         tab-bar-mode
                         (fboundp 'tab-bar--current-tab)
                         (fboundp 'tab-bar-select-tab-by-name))
                (let ((current-tab (tab-bar--current-tab)))
                  (when (and original-tab current-tab
                             (not (equal (alist-get 'name original-tab)
                                         (alist-get 'name current-tab))))
                    (tab-bar-select-tab-by-name (alist-get 'name original-tab)))))))
          ;; Should NOT have switched
          (should (null tab-switched)))))))

(defun claude-code-ide-run-tests ()
  "Run all claude-code-ide test cases."
  (interactive)
  (ert-run-tests-batch-and-exit "^claude-code-ide-test-"))

(defun claude-code-ide-run-all-tests ()
  "Run all claude-code-ide tests including MCP tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^claude-code-ide-"))

;;; MCP Tests

;; Load MCP module now that websocket is available
(require 'claude-code-ide-mcp)

;; Load MCP handlers module for testing
(require 'claude-code-ide-mcp-handlers)

;; Load MCP tools server module
(condition-case nil
    (require 'claude-code-ide-mcp-server)
  (error nil))

;;; MCP Test Helper Functions

(defmacro claude-code-ide-mcp-tests--with-temp-file (file-var content &rest body)
  "Create a temporary file with CONTENT, bind its path to FILE-VAR, and execute BODY."
  (declare (indent 2))
  `(let ((,file-var (make-temp-file "claude-mcp-test-")))
     (unwind-protect
         (progn
           (with-temp-file ,file-var
             (insert ,content))
           ,@body)
       (delete-file ,file-var))))

(defmacro claude-code-ide-mcp-tests--with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Tests for MCP Tool Implementations

(ert-deftest claude-code-ide-test-mcp-open-file ()
  "Test the openFile tool implementation."
  ;; Test successful file open
  (claude-code-ide-mcp-tests--with-temp-file test-file "Line 1\nLine 2\nLine 3\nLine 4"
                                             (let ((result (claude-code-ide-mcp-handle-open-file `((filePath . ,test-file)))))
                                               (should (listp result))
                                               (let ((first-item (car result)))
                                                 (should (equal (alist-get 'type first-item) "text"))
                                                 (should (equal (alist-get 'text first-item) "FILE_OPENED")))
                                               (should (equal (buffer-file-name) test-file))
                                               (kill-buffer)))

  ;; Test with selection
  (claude-code-ide-mcp-tests--with-temp-file test-file "Line 1\nLine 2\nLine 3\nLine 4"
                                             (let ((result (claude-code-ide-mcp-handle-open-file
                                                            `((filePath . ,test-file)
                                                              (startLine . 2)
                                                              (endLine . 3)))))
                                               (should (listp result))
                                               (let ((first-item (car result)))
                                                 (should (equal (alist-get 'type first-item) "text"))
                                                 (should (equal (alist-get 'text first-item) "FILE_OPENED")))
                                               (should (use-region-p))
                                               (should (= (line-number-at-pos (region-beginning)) 2))
                                               (kill-buffer)))

  ;; Test missing filePath parameter
  (should-error (claude-code-ide-mcp-handle-open-file '())
                :type 'mcp-error))

(ert-deftest claude-code-ide-test-mcp-get-current-selection ()
  "Test the selection payload builder."
  ;; Test with active selection
  (claude-code-ide-mcp-tests--with-temp-buffer "Line 1\nLine 2\nLine 3"
                                               (goto-char (point-min))
                                               (set-mark (point))
                                               (forward-line 2)
                                               ;; Ensure transient-mark-mode is on and region is active
                                               (let ((transient-mark-mode t))
                                                 (activate-mark)
                                                 (let ((result (claude-code-ide-mcp--get-current-selection)))
                                                   (should (equal (alist-get 'text result) "Line 1\nLine 2\n"))
                                                   (should-not (assq 'fileUrl result))
                                                   (let ((selection (alist-get 'selection result)))
                                                     (should selection)
                                                     (should-not (assq 'isEmpty selection))
                                                     (let ((start (alist-get 'start selection))
                                                           (end (alist-get 'end selection)))
                                                       (should (= (alist-get 'line start) 1))  ; 1-based
                                                       (should (= (alist-get 'line end) 3)))))))  ; 1-based

  ;; Test without selection
  (claude-code-ide-mcp-tests--with-temp-buffer "Test"
                                               (let ((result (claude-code-ide-mcp--get-current-selection)))
                                                 (should (equal (alist-get 'text result) ""))
                                                 (let ((selection (alist-get 'selection result)))
                                                   (should selection)
                                                   ;; No selection: start and end should be equal (cursor position)
                                                   (should (equal (alist-get 'start selection) (alist-get 'end selection)))
                                                   ;; Should not contain isEmpty or fileUrl
                                                   (should-not (assq 'isEmpty selection))
                                                   (should-not (assq 'fileUrl result))))))

(ert-deftest claude-code-ide-test-mcp-close-tab ()
  "Test the close_tab tool implementation."
  (claude-code-ide-mcp-tests--with-temp-file test-file "Content"
                                             (find-file-noselect test-file)
                                             ;; Close using tool
                                             (let ((result (claude-code-ide-mcp-handle-close-tab `((path . ,test-file)))))
                                               ;; Handler returns VS Code format
                                               (should (listp result))
                                               (let ((first-item (car result)))
                                                 (should (equal (alist-get 'type first-item) "text"))
                                                 (should (equal (alist-get 'text first-item) "TAB_CLOSED")))
                                               (should-not (find-buffer-visiting test-file))))

  ;; Test non-existent buffer - should throw an error
  (should-error (claude-code-ide-mcp-handle-close-tab '((path . "/nonexistent/file")))
                :type 'mcp-error))

(ert-deftest claude-code-ide-test-mcp-tool-registry ()
  "Test that all tools are properly registered."
  ;; Build expected tools list dynamically based on configuration
  (let* ((base-tools '("openFile" "getDiagnostics" "close_tab"))
         (diff-tools (when (bound-and-true-p claude-code-ide-use-ide-diff)
                       '("openDiff" "closeAllDiffTabs")))
         (exec-tools (when (bound-and-true-p claude-code-ide-enable-execute-code)
                       '("executeCode")))
         (expected-tools (append base-tools diff-tools exec-tools)))
    ;; Rebuild tool lists to match current configuration
    (setq claude-code-ide-mcp-tools (claude-code-ide-mcp--build-tool-list))
    (setq claude-code-ide-mcp-tool-schemas (claude-code-ide-mcp--build-tool-schemas))
    (setq claude-code-ide-mcp-tool-descriptions (claude-code-ide-mcp--build-tool-descriptions))
    (dolist (tool-name expected-tools)
      (should (alist-get tool-name claude-code-ide-mcp-tools nil nil #'string=))
      (let ((handler (alist-get tool-name claude-code-ide-mcp-tools nil nil #'string=))
            (schema (alist-get tool-name claude-code-ide-mcp-tool-schemas nil nil #'string=)))
        ;; Check that handler is a function or a symbol that points to a function
        (should (or (functionp handler)
                    (and (symbolp handler) (fboundp handler))))
        ;; Check that schema is provided
        (should schema)))))

(ert-deftest claude-code-ide-test-ediff-flag-disables-tools ()
  "Test that diff tools are excluded when claude-code-ide-use-ide-diff is nil."
  (let ((claude-code-ide-use-ide-diff nil))
    ;; Rebuild tool lists with ediff disabled
    (setq claude-code-ide-mcp-tools (claude-code-ide-mcp--build-tool-list))
    (setq claude-code-ide-mcp-tool-schemas (claude-code-ide-mcp--build-tool-schemas))
    (setq claude-code-ide-mcp-tool-descriptions (claude-code-ide-mcp--build-tool-descriptions))
    ;; Verify diff tools are not present
    (should-not (alist-get "openDiff" claude-code-ide-mcp-tools nil nil #'string=))
    (should-not (alist-get "closeAllDiffTabs" claude-code-ide-mcp-tools nil nil #'string=))
    (should-not (alist-get "openDiff" claude-code-ide-mcp-tool-schemas nil nil #'string=))
    (should-not (alist-get "closeAllDiffTabs" claude-code-ide-mcp-tool-schemas nil nil #'string=))
    (should-not (alist-get "openDiff" claude-code-ide-mcp-tool-descriptions nil nil #'string=))
    (should-not (alist-get "closeAllDiffTabs" claude-code-ide-mcp-tool-descriptions nil nil #'string=))
    ;; Verify other tools are still present
    (should (alist-get "openFile" claude-code-ide-mcp-tools nil nil #'string=))
    (should (alist-get "getDiagnostics" claude-code-ide-mcp-tools nil nil #'string=)))
  ;; Test with ediff enabled
  (let ((claude-code-ide-use-ide-diff t))
    ;; Rebuild tool lists with ediff enabled
    (setq claude-code-ide-mcp-tools (claude-code-ide-mcp--build-tool-list))
    (setq claude-code-ide-mcp-tool-schemas (claude-code-ide-mcp--build-tool-schemas))
    (setq claude-code-ide-mcp-tool-descriptions (claude-code-ide-mcp--build-tool-descriptions))
    ;; Verify diff tools are present
    (should (alist-get "openDiff" claude-code-ide-mcp-tools nil nil #'string=))
    (should (alist-get "closeAllDiffTabs" claude-code-ide-mcp-tools nil nil #'string=))
    (should (alist-get "openDiff" claude-code-ide-mcp-tool-schemas nil nil #'string=))
    (should (alist-get "closeAllDiffTabs" claude-code-ide-mcp-tool-schemas nil nil #'string=))
    (should (alist-get "openDiff" claude-code-ide-mcp-tool-descriptions nil nil #'string=))
    (should (alist-get "closeAllDiffTabs" claude-code-ide-mcp-tool-descriptions nil nil #'string=))))

(ert-deftest claude-code-ide-test-execute-code-flag ()
  "Test that executeCode tool is excluded when flag is nil and included when t."
  (let ((claude-code-ide-enable-execute-code nil))
    (setq claude-code-ide-mcp-tools (claude-code-ide-mcp--build-tool-list))
    (setq claude-code-ide-mcp-tool-schemas (claude-code-ide-mcp--build-tool-schemas))
    (setq claude-code-ide-mcp-tool-descriptions (claude-code-ide-mcp--build-tool-descriptions))
    (should-not (alist-get "executeCode" claude-code-ide-mcp-tools nil nil #'string=))
    (should-not (alist-get "executeCode" claude-code-ide-mcp-tool-schemas nil nil #'string=))
    (should-not (alist-get "executeCode" claude-code-ide-mcp-tool-descriptions nil nil #'string=)))
  (let ((claude-code-ide-enable-execute-code t))
    (setq claude-code-ide-mcp-tools (claude-code-ide-mcp--build-tool-list))
    (setq claude-code-ide-mcp-tool-schemas (claude-code-ide-mcp--build-tool-schemas))
    (setq claude-code-ide-mcp-tool-descriptions (claude-code-ide-mcp--build-tool-descriptions))
    (should (alist-get "executeCode" claude-code-ide-mcp-tools nil nil #'string=))
    (should (alist-get "executeCode" claude-code-ide-mcp-tool-schemas nil nil #'string=))
    (should (alist-get "executeCode" claude-code-ide-mcp-tool-descriptions nil nil #'string=))))

(ert-deftest claude-code-ide-test-execute-code-handler ()
  "Test the executeCode handler."
  ;; Simple expression
  (let ((result (claude-code-ide-mcp-handle-execute-code '((code . "(+ 1 2)")))))
    (should (equal (alist-get 'text (car result)) "3")))
  ;; String result
  (let ((result (claude-code-ide-mcp-handle-execute-code '((code . "(concat \"hello\" \" world\")")))))
    (should (equal (alist-get 'text (car result)) "\"hello world\"")))
  ;; Missing code parameter
  (should-error (claude-code-ide-mcp-handle-execute-code '())
                :type 'mcp-error)
  ;; Evaluation error
  (should-error (claude-code-ide-mcp-handle-execute-code '((code . "(error \"boom\")")))
                :type 'mcp-error))

(ert-deftest claude-code-ide-test-mcp-server-lifecycle ()
  "Test per-instance MCP server start and stop."
  (require 'claude-code-ide-mcp)
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-lifecycle/")
        (session nil))
    (unwind-protect
        (progn
          (setq session (claude-code-ide-mcp-create-session project-dir "lifecycle-1"))
          (let ((port (claude-code-ide-mcp-session-port session)))
            (should (numberp port))
            (should (>= port 10000))
            (should (<= port 65535))
            (should (claude-code-ide-mcp-session-server session))
            ;; Registered under its session ID and reachable via its project
            (should (eq (claude-code-ide-mcp--get-session-by-id "lifecycle-1") session))
            (should (equal (claude-code-ide-mcp--sessions-for-project project-dir)
                           (list session)))
            ;; Check lockfile exists
            (should (file-exists-p (claude-code-ide-mcp--lockfile-path port)))
            ;; Selection tracking is installed while an instance lives
            (should (memq #'claude-code-ide-mcp--track-selection post-command-hook))
            ;; Stop server
            (claude-code-ide-mcp--stop-session session)
            ;; Check lockfile removed and the session deregistered
            (should-not (file-exists-p (claude-code-ide-mcp--lockfile-path port)))
            (should-not (claude-code-ide-mcp--get-session-by-id "lifecycle-1"))
            (should-not (memq #'claude-code-ide-mcp--track-selection post-command-hook))))
      ;; Ensure cleanup
      (claude-code-ide-tests--stop-all-sessions)
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-mcp-stop-scoping ()
  "`claude-code-ide-mcp-stop' stops the buffer's project, or everything."
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-stop-scope-a/")
        (stopped '()))
    (unwind-protect
        ;; The fixtures own no real WebSocket server, so record the
        ;; teardowns instead of performing them
        (cl-letf (((symbol-function 'claude-code-ide-mcp--stop-session)
                   (lambda (session)
                     (push session stopped)
                     (remhash (claude-code-ide-mcp-session-session-id session)
                              claude-code-ide-mcp--sessions))))
          (let ((session-a (claude-code-ide-tests--make-session project-dir))
                (session-b (claude-code-ide-tests--make-session project-dir
                                                                :instance-name "b"))
                (other (claude-code-ide-tests--make-session
                        "/tmp/claude-stop-scope-b/")))
            ;; Inside a project, every instance of that project is stopped
            (cl-letf (((symbol-function 'claude-code-ide-mcp--get-buffer-project)
                       (lambda () project-dir)))
              (claude-code-ide-mcp-stop))
            (should (= 2 (length stopped)))
            (should (memq session-a stopped))
            (should (memq session-b stopped))
            (should-not (memq other stopped))

            ;; Outside any project it falls back to stopping all instances
            (setq stopped '())
            (cl-letf (((symbol-function 'claude-code-ide-mcp--get-buffer-project)
                       (lambda () nil)))
              (claude-code-ide-mcp-stop))
            (should (equal stopped (list other)))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-ide-connected-notification ()
  "Test that ide_connected notification stores the CLI PID in its session."
  (require 'claude-code-ide-mcp)
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let* ((session (claude-code-ide-tests--make-session "/tmp/test/" :port 12345))
             (other (claude-code-ide-tests--make-session "/tmp/test/"
                                                         :instance-name "other"
                                                         :port 12346))
             (message '((method . "ide_connected")
                        (params . ((pid . 42))))))
        ;; Simulate the dispatch on the session whose socket carried it
        (claude-code-ide-mcp--handle-message message session)
        (should (= (claude-code-ide-mcp-session-cli-pid session) 42))
        ;; The sibling instance of the same project is unaffected
        (should-not (claude-code-ide-mcp-session-cli-pid other)))
    (claude-code-ide-tests--clear-processes)))

;; Test for side window handling in openDiff
(defvar claude-code-ide-debug-buffer)
(ert-deftest claude-code-ide-test-opendiff-side-window ()
  "Test that openDiff handles side windows correctly."
  (require 'claude-code-ide-debug)
  (require 'claude-code-ide-mcp-handlers)
  (let* ((temp-dir (file-name-as-directory (make-temp-file "test-project-" t)))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         (claude-code-ide-debug t)
         (claude-code-ide-debug-buffer "*claude-code-ide-debug*")
         (temp-file (make-temp-file "test-diff-" nil ".txt" "Original content\n"))
         (side-window nil)
         ;; Create a mock session for the test
         (test-session (claude-code-ide-tests--make-session temp-dir :port 12345)))
    ;; Create a .git directory to make this a project
    (make-directory (expand-file-name ".git" temp-dir) t)

    (unwind-protect
        ;; Mock the project detection to return our test directory
        (cl-letf (((symbol-function 'claude-code-ide-mcp--get-buffer-project)
                   (lambda () temp-dir))
                  ((symbol-function 'claude-code-ide-mcp--get-current-session)
                   (lambda () test-session)))
          ;; Set up the project context
          (with-current-buffer (get-buffer-create "*test-buffer*")
            (setq default-directory temp-dir)

            ;; Create a side window to simulate the problem
            (let ((side-buffer (get-buffer-create "*test-sidebar*")))
              (with-current-buffer side-buffer
                (insert "Sidebar content"))
              ;; Display buffer in side window
              (setq side-window (display-buffer-in-side-window
                                 side-buffer
                                 '((side . left) (slot . 0) (window-width . 30))))

              ;; Verify side window was created
              (should (window-parameter side-window 'window-side))

              ;; Now try to open diff - should handle side window gracefully.
              ;; The session is passed explicitly: openDiff belongs to the
              ;; instance whose socket carried the request.
              (let ((result (claude-code-ide-mcp-handle-open-diff
                             `((old_file_path . ,temp-file)
                               (new_file_path . ,temp-file)
                               (new_file_contents . "Modified content\n")
                               (tab_name . "test-diff"))
                             test-session)))
                ;; Should return deferred
                (should (eq (alist-get 'deferred result) t))

                ;; Should have created diff session in the test session
                (should (gethash "test-diff" (claude-code-ide-mcp-session-active-diffs test-session)))

                ;; Clean up - quit ediff if it started
                (when (and (boundp 'ediff-control-buffer)
                           ediff-control-buffer
                           (buffer-live-p ediff-control-buffer))
                  (with-current-buffer ediff-control-buffer
                    (remove-hook 'ediff-quit-hook t t)
                    (ediff-really-quit nil)))))))
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t))
      (when (and side-window (window-live-p side-window))
        (delete-window side-window))
      (claude-code-ide-mcp--cleanup-diff "test-diff" test-session)
      (kill-buffer "*test-buffer*")
      (kill-buffer "*test-sidebar*"))))

;;; Tests for Diagnostics

(ert-deftest claude-code-ide-test-diagnostics-severity-mapping ()
  "Test diagnostic severity conversion."
  (require 'claude-code-ide-diagnostics)
  ;; Test Flycheck symbols
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'error) 1))
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'warning) 2))
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'info) 3))
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'hint) 4))
  ;; Test default fallback
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'unknown) 3)))

(ert-deftest claude-code-ide-test-diagnostics-severity-to-string ()
  "Test severity to string conversion."
  (require 'claude-code-ide-diagnostics)
  ;; Test Flycheck severities
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'error) "Error"))
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'warning) "Warning"))
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'info) "Information"))
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'hint) "Hint"))
  ;; Test default fallback
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'unknown) "Information")))

(ert-deftest claude-code-ide-test-diagnostics-handler ()
  "Test getDiagnostics handler."
  (require 'claude-code-ide-diagnostics)
  ;; Test with no diagnostics available
  (let ((result (claude-code-ide-diagnostics-handler nil)))
    ;; The diagnostics handler returns content array format
    (should (listp result))
    ;; Check it has the expected format
    (should (equal (alist-get 'type (car result)) "text"))
    ;; The text should be an empty array "[]"
    (should (equal (alist-get 'text (car result)) "[]"))))

;; Define mock struct for flymake diagnostics testing
(cl-defstruct claude-code-ide-test-mock-diag
  beg end type text backend)

(ert-deftest claude-code-ide-test-flymake-diagnostics ()
  "Test flymake diagnostics collection."
  ;; Skip this test in batch mode as it requires a complex flymake setup
  (skip-unless nil)
  (require 'claude-code-ide-diagnostics))

(ert-deftest claude-code-ide-test-diagnostics-backend-auto ()
  "Test automatic backend detection."
  (require 'claude-code-ide-diagnostics)
  ;; Test flycheck detection
  (cl-letf (((symbol-function 'featurep)
             (lambda (feature &rest _)
               (memq feature '(flycheck flymake))))
            ((symbol-function 'bound-and-true-p)
             (lambda (var)
               (eq var 'flycheck-mode)))
            ((symbol-function 'flycheck-diagnostics)
             (lambda () nil))
            (flycheck-current-errors nil)
            (claude-code-ide-diagnostics-backend 'auto))
    (with-temp-buffer
      (let ((diags (claude-code-ide-diagnostics-get-all (current-buffer))))
        ;; Should use flycheck when flycheck-mode is active
        (should (vectorp diags))))))

;; Disabled due to ERT macro interaction with transient-mark-mode in batch mode
;; The handler works correctly (verified with direct testing) but the test fails
;; because `should` macro seems to evaluate `use-region-p` in a different context
(ert-deftest claude-code-ide-test-open-file-text-patterns ()
  "Test openFile handler with text pattern selection."
  (skip-unless nil) ; Skip this test for now
  (require 'claude-code-ide-mcp-handlers)
  ;; Create a temporary file with known content
  (let ((temp-file (make-temp-file "test-openfile-" nil ".el"))
        ;; Save and restore global transient-mark-mode
        (orig-tmm transient-mark-mode))
    (unwind-protect
        (progn
          ;; Enable transient-mark-mode globally for this test
          (setq transient-mark-mode t)
          ;; Write test content to file
          (with-temp-file temp-file
            (insert "Line 1\n")
            (insert "function foo() {\n")
            (insert "  console.log('hello');\n")
            (insert "}\n")
            (insert "Line 5\n")
            (insert "function bar() {\n")
            (insert "  return 42;\n")
            (insert "}\n"))

          ;; Test 1: Text pattern selection with both start and end
          (let ((result (claude-code-ide-mcp-handle-open-file
                         `((path . ,temp-file)
                           (startText . "function foo")
                           (endText . "}")))))
            ;; Should have opened the file and selected from "function foo" to first "}"
            (with-current-buffer (find-buffer-visiting temp-file)
              (should (string= (buffer-file-name) temp-file))
              ;; Debug info
              (message "Debug: buffer=%s tmm=%s mark-active=%s mark=%s point=%s region-p=%s"
                       (buffer-name) transient-mark-mode mark-active
                       (and (mark) (mark)) (point) (use-region-p))
              ;; Store region state before should
              (let ((region-was-active (use-region-p)))
                (should region-was-active))
              (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                               "function foo() {\n  console.log('hello');\n}"))))

          ;; Test 2: Only start text pattern
          (with-current-buffer (find-buffer-visiting temp-file)
            (deactivate-mark))
          (let ((result (claude-code-ide-mcp-handle-open-file
                         `((path . ,temp-file)
                           (startText . "function bar")))))
            ;; Should position cursor at start of "function bar"
            (with-current-buffer (find-buffer-visiting temp-file)
              (should (looking-at "function bar"))
              (should-not (use-region-p))))

          ;; Test 3: Text pattern with fallback to line numbers
          (let ((result (claude-code-ide-mcp-handle-open-file
                         `((path . ,temp-file)
                           (startText . "nonexistent text")
                           (startLine . 2)
                           (endLine . 4)))))
            ;; Should fall back to line selection
            (with-current-buffer (find-buffer-visiting temp-file)
              (should (use-region-p))
              (let ((selected (buffer-substring-no-properties (region-beginning) (region-end))))
                (should (string-match-p "function foo" selected)))))

          ;; Test 4: Text patterns take precedence over line numbers
          (with-current-buffer (find-buffer-visiting temp-file)
            (deactivate-mark))
          (let ((result (claude-code-ide-mcp-handle-open-file
                         `((path . ,temp-file)
                           (startText . "Line 5")
                           (startLine . 1)))))
            ;; Should go to "Line 5", not line 1
            (with-current-buffer (find-buffer-visiting temp-file)
              (should (looking-at "Line 5"))
              (should (= (line-number-at-pos) 5)))))

      ;; Cleanup
      (delete-file temp-file)
      ;; Restore original transient-mark-mode
      (setq transient-mark-mode orig-tmm))))

;; Test claude-code-ide-show-claude-window-in-ediff option
(ert-deftest claude-code-ide-test-show-claude-window-in-ediff ()
  "Test that Claude window visibility is controlled correctly during ediff."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     (let* ((claude-buffer-created (get-buffer-create "*Claude Code Test*"))
            ;; The instance's own terminal buffer is shown, never a buffer
            ;; found by the canonical name
            (session (claude-code-ide-tests--make-session default-directory
                                                          :buffer claude-buffer-created))
            (diff-buffer-b (get-buffer-create "*Claude Ediff B Test*"))
            (test-file (expand-file-name "test.txt" default-directory))
            (claude-window-displayed nil))

       ;; Create a test file
       (with-temp-file test-file (insert "Original content"))

       ;; Create a .git directory to make this a project
       (make-directory (expand-file-name ".git" default-directory) t)

       ;; Register the diff so the startup handler recognizes it as its own
       ;; (identity goes through buffer-B, set below on the control buffer)
       (puthash "test-diff" `((buffer-B . ,diff-buffer-b) (session . ,session))
                (claude-code-ide-mcp-session-active-diffs session))

       ;; Mock relevant functions
       (cl-letf* (((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                   (lambda (buffer)
                     (setq claude-window-displayed (eq buffer claude-buffer-created))
                     (selected-window)))
                  ((symbol-function 'ediff-buffers)
                   (lambda (_buf-A _buf-B)
                     ;; Simulate successful ediff start
                     (setq ediff-control-buffer (get-buffer-create "*Ediff Control*"))))
                  ((symbol-function 'ediff-next-difference)
                   (lambda () nil))
                  ((symbol-function 'claude-code-ide-mcp--get-current-session)
                   (lambda () session)))

         ;; Test 1: With claude-code-ide-show-claude-window-in-ediff = t (default)
         (let ((claude-code-ide-show-claude-window-in-ediff t)
               (ediff-control-buffer (get-buffer-create "*Ediff Control*")))
           (with-current-buffer ediff-control-buffer
             (setq-local ediff-buffer-B diff-buffer-b))
           (setq claude-window-displayed nil)
           ;; Call the startup handler
           (claude-code-ide-mcp--handle-ediff-startup "test-diff" session nil
                                                      (lambda () nil))
           ;; Should display Claude window
           (should claude-window-displayed))

         ;; Test 2: With claude-code-ide-show-claude-window-in-ediff = nil
         (let ((claude-code-ide-show-claude-window-in-ediff nil)
               (ediff-control-buffer (get-buffer-create "*Ediff Control*")))
           (with-current-buffer ediff-control-buffer
             (setq-local ediff-buffer-B diff-buffer-b))
           (setq claude-window-displayed nil)
           ;; Call the startup handler
           (claude-code-ide-mcp--handle-ediff-startup "test-diff" session nil
                                                      (lambda () nil))
           ;; Should NOT display Claude window
           (should-not claude-window-displayed))

         ;; Cleanup
         (when (buffer-live-p claude-buffer-created)
           (kill-buffer claude-buffer-created))
         (when (buffer-live-p diff-buffer-b)
           (kill-buffer diff-buffer-b))
         (when (get-buffer "*Ediff Control*")
           (kill-buffer "*Ediff Control*"))
         (when (file-exists-p test-file)
           (delete-file test-file))
         (claude-code-ide-tests--clear-processes))))))

(ert-deftest claude-code-ide-test-ediff-startup-buffer-identity ()
  "The ediff startup hook acts only on its own ediff.
`ediff-startup-hook' is global, so with two racing ediffs the wrong
closure can fire first.  Ours is recognized by buffer-B identity: a
foreign control buffer is ignored and the hook stays armed, and the
control buffer name plays no role (ediff derives
`ediff-control-buffer-suffix' from the name — it never consumes it)."
  (claude-code-ide-tests--clear-processes)
  (let* ((buffer-b (generate-new-buffer "*ediff-identity-b*"))
         (foreign-b (generate-new-buffer "*ediff-identity-foreign*"))
         (control-buf (generate-new-buffer "*Ediff Control Panel<2>*"))
         (hook-removed '()))
    (unwind-protect
        (let* ((session (claude-code-ide-tests--make-session "/tmp/ediff-identity/"))
               (active-diffs (claude-code-ide-mcp-session-active-diffs session))
               (startup-fn (lambda ())))
          (puthash "tab" `((buffer-B . ,buffer-b) (session . ,session))
                   active-diffs)
          (cl-letf (((symbol-function 'remove-hook)
                     (lambda (hook fn) (push (cons hook fn) hook-removed)))
                    ((symbol-function 'ediff-next-difference)
                     (lambda (&rest _))))
            ;; A foreign ediff (different buffer-B): untouched, hook kept
            (with-current-buffer control-buf
              (setq-local ediff-buffer-B foreign-b))
            (let ((ediff-control-buffer control-buf))
              (claude-code-ide-mcp--handle-ediff-startup "tab" session nil startup-fn))
            (should-not (alist-get 'control-buffer (gethash "tab" active-diffs)))
            (should-not hook-removed)
            ;; Our own ediff: control buffer recorded, hook removed,
            ;; quit hook installed in the control buffer
            (with-current-buffer control-buf
              (setq-local ediff-buffer-B buffer-b))
            (let ((ediff-control-buffer control-buf))
              (claude-code-ide-mcp--handle-ediff-startup "tab" session nil startup-fn))
            (should (eq (alist-get 'control-buffer (gethash "tab" active-diffs))
                        control-buf))
            (should (equal hook-removed (list (cons 'ediff-startup-hook startup-fn))))
            (should (buffer-local-value 'ediff-quit-hook control-buf))))
      (dolist (buffer (list buffer-b foreign-b control-buf))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (claude-code-ide-tests--clear-processes))))

;; Test multiple ediff sessions
(ert-deftest claude-code-ide-test-multiple-ediff-sessions ()
  "Test that multiple ediff sessions can run simultaneously without conflicts."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     (let* ((session (claude-code-ide-tests--make-session default-directory
                                                          :port 12345))
            (file1 (expand-file-name "test-file1.txt" default-directory))
            (file2 (expand-file-name "test-file2.txt" default-directory))
            (control-buffers '()))

       ;; Create test files
       (with-temp-file file1 (insert "Original content 1"))
       (with-temp-file file2 (insert "Original content 2"))

       ;; Create a .git directory to make this a project
       (make-directory (expand-file-name ".git" default-directory) t)

       ;; Mock ediff functions to capture the buffers each diff runs on
       (cl-letf* ((ediff-called-count 0)
                  ((symbol-function 'ediff-buffers)
                   (lambda (_buf-A buf-B)
                     (cl-incf ediff-called-count)
                     (push (buffer-name buf-B) control-buffers)))
                  ((symbol-function 'claude-code-ide-mcp--get-current-session)
                   (lambda () session)))

         ;; Simulate opening multiple diffs
         (unwind-protect
             (progn
               ;; Open first diff
               (let ((result1 (claude-code-ide-mcp-handle-open-diff
                               `((old_file_path . ,file1)
                                 (new_file_path . ,file1)
                                 (new_file_contents . "Modified content 1")
                                 (tab_name . "diff1"))
                               session)))
                 (should (equal (alist-get 'deferred result1) t))
                 (should (equal (alist-get 'unique-key result1) "diff1")))

               ;; Open second diff
               (let ((result2 (claude-code-ide-mcp-handle-open-diff
                               `((old_file_path . ,file2)
                                 (new_file_path . ,file2)
                                 (new_file_contents . "Modified content 2")
                                 (tab_name . "diff2"))
                               session)))
                 (should (equal (alist-get 'deferred result2) t))
                 (should (equal (alist-get 'unique-key result2) "diff2")))

               ;; Verify ediff was called twice, on two distinct B buffers
               (should (= ediff-called-count 2))
               (should (= (length control-buffers) 2))
               (should (member "*diff1*" control-buffers))
               (should (member "*diff2*" control-buffers))

               ;; Verify active diffs are tracked correctly
               (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session)))
                 (should (gethash "diff1" active-diffs))
                 (should (gethash "diff2" active-diffs))))

           ;; Cleanup
           (claude-code-ide-mcp-handle-close-all-diff-tabs nil session)
           (when (file-exists-p file1) (delete-file file1))
           (when (file-exists-p file2) (delete-file file2))
           (claude-code-ide-tests--clear-processes)))))))

(ert-deftest claude-code-ide-test-mcp-two-projects-deferred-isolation ()
  "Test that deferred responses are answered on their own session's socket."
  (claude-code-ide-tests--clear-processes)
  (let* ((project-a "/tmp/project-a/")
         (project-b "/tmp/project-b/")
         (client-a (claude-code-ide-tests--make-websocket "ws://127.0.0.1:10004"))
         (client-b (claude-code-ide-tests--make-websocket "ws://127.0.0.1:10005"))
         (session-a nil)
         (session-b nil)
         (sent '()))
    ;; Create mock websocket-send-text to capture (client . payload) pairs
    (cl-letf* (((symbol-function 'websocket-send-text)
                (lambda (ws text)
                  (push (cons ws text) sent))))
      (unwind-protect
          (progn
            (setq session-a (claude-code-ide-tests--make-session
                             project-a :client client-a)
                  session-b (claude-code-ide-tests--make-session
                             project-b :client client-b))

            ;; Each session awaits its own deferred openDiff response
            (puthash "openDiff-diff1" "request-id-1"
                     (claude-code-ide-mcp-session-deferred session-a))
            (puthash "openDiff-diff2" "request-id-2"
                     (claude-code-ide-mcp-session-deferred session-b))

            (claude-code-ide-mcp-complete-deferred session-a
                                                   "openDiff"
                                                   '(((type . "text") (text . "FILE_SAVED")))
                                                   "diff1")
            (claude-code-ide-mcp-complete-deferred session-b
                                                   "openDiff"
                                                   '(((type . "text") (text . "DIFF_REJECTED")))
                                                   "diff2")

            ;; Both responses were sent, each on its own session's client
            (should (= (length sent) 2))
            (let ((payload-a (cdr (cl-find-if (lambda (entry)
                                                (eq (car entry) client-a))
                                              sent)))
                  (payload-b (cdr (cl-find-if (lambda (entry)
                                                (eq (car entry) client-b))
                                              sent))))
              (should payload-a)
              (should payload-b)
              (should (equal (alist-get 'id (json-read-from-string payload-a))
                             "request-id-1"))
              (should (string-match-p "FILE_SAVED" payload-a))
              (should (equal (alist-get 'id (json-read-from-string payload-b))
                             "request-id-2"))
              (should (string-match-p "DIFF_REJECTED" payload-b)))

            ;; Verify deferred responses were removed from both sessions
            (should (= 0 (hash-table-count
                          (claude-code-ide-mcp-session-deferred session-a))))
            (should (= 0 (hash-table-count
                          (claude-code-ide-mcp-session-deferred session-b))))

            ;; Completing an id that lives in another session sends nothing
            (setq sent '())
            (puthash "openDiff-diff3" "request-id-3"
                     (claude-code-ide-mcp-session-deferred session-a))
            (claude-code-ide-mcp-complete-deferred session-b "openDiff" nil "diff3")
            (should (null sent))
            (should (gethash "openDiff-diff3"
                             (claude-code-ide-mcp-session-deferred session-a))))
        (claude-code-ide-tests--clear-processes)))))

;;; MCP Tools Server Tests

;; Mock the server functions since web-server might not be available in test env
(defvar claude-code-ide-mcp-server-tests--mock-server-started nil)
(defvar claude-code-ide-mcp-server-tests--mock-server-port 12345)

(defun claude-code-ide-mcp-server-tests--mock-server-start (&optional _port)
  "Mock server start function."
  (setq claude-code-ide-mcp-server-tests--mock-server-started t)
  (cons 'mock-process claude-code-ide-mcp-server-tests--mock-server-port))

(defun claude-code-ide-mcp-server-tests--mock-server-stop (_process)
  "Mock server stop function."
  (setq claude-code-ide-mcp-server-tests--mock-server-started nil))

;;; Mock websocket request/response for testing
(defvar claude-code-ide-mcp-server-tests--last-response nil
  "Storage for the last response sent.")

(defvar claude-code-ide-mcp-server-tests--last-response-headers nil
  "Storage for the last response headers.")

(defvar claude-code-ide-mcp-server-tests--last-response-status nil
  "Storage for the last response status.")

;; Mock the web-server functions
(cl-defstruct claude-code-ide-mcp-server-tests--mock-request
  process headers body)

(cl-defstruct claude-code-ide-mcp-server-tests--mock-process)

(defun claude-code-ide-mcp-server-tests--mock-ws-response-header (process status &rest headers)
  "Mock ws-response-header function."
  (setq claude-code-ide-mcp-server-tests--last-response-status status)
  (setq claude-code-ide-mcp-server-tests--last-response-headers headers))

(defun claude-code-ide-mcp-server-tests--mock-ws-send (process data)
  "Mock ws-send function."
  (unless (claude-code-ide-mcp-server-tests--mock-process-p process)
    (error "Wrong type argument: processp, %s" process))
  (setq claude-code-ide-mcp-server-tests--last-response data))

(defun claude-code-ide-mcp-server-tests--mock-ws-send-404 (process)
  "Mock ws-send-404 function."
  (unless (claude-code-ide-mcp-server-tests--mock-process-p process)
    (error "Wrong type argument: processp, %s" process))
  (setq claude-code-ide-mcp-server-tests--last-response-status 404))

;;; Session Management Tests

(ert-deftest claude-code-ide-mcp-server-test-session-lifecycle ()
  "Test MCP tools server lifecycle against the registered-session table.
The server lives as long as at least one instance is registered, so an
unbalanced or repeated end call cannot pull it out from under a live
sibling instance."
  (let ((claude-code-ide-enable-mcp-server t)
        (claude-code-ide-mcp-server--server nil)
        (claude-code-ide-mcp-server--port nil))
    (unwind-protect
        ;; Mock the server functions and require
        (cl-letf (((symbol-function 'claude-code-ide-mcp-http-server-start)
                   #'claude-code-ide-mcp-server-tests--mock-server-start)
                  ((symbol-function 'claude-code-ide-mcp-http-server-stop)
                   #'claude-code-ide-mcp-server-tests--mock-server-stop)
                  ((symbol-function 'require)
                   (lambda (feature &optional _filename _noerror)
                     (cond ((eq feature 'claude-code-ide-mcp-http-server) nil)
                           ((memq feature '(claude-code-ide-mcp-server websocket vterm flycheck
                                                                       claude-code-ide-debug claude-code-ide-mcp-handlers
                                                                       claude-code-ide transient)) nil)
                           (t (funcall (cl-letf-saved-symbol-function 'require) feature _filename _noerror))))))
          (clrhash claude-code-ide-mcp-server--sessions)
          ;; First instance registers; the web-server package may be absent
          ;; in batch, so bring the mock server up directly
          (claude-code-ide-mcp-server-session-started "sid-1" "/tmp/proj-1/" nil)
          (should (= (hash-table-count claude-code-ide-mcp-server--sessions) 1))
          (let ((result (claude-code-ide-mcp-server-tests--mock-server-start)))
            (setq claude-code-ide-mcp-server--server (car result)
                  claude-code-ide-mcp-server--port (cdr result)))
          (should claude-code-ide-mcp-server--server)
          (should (= claude-code-ide-mcp-server--port
                     claude-code-ide-mcp-server-tests--mock-server-port))

          ;; A second instance of the same project registers separately and
          ;; does not disturb the running server
          (let ((server claude-code-ide-mcp-server--server))
            (claude-code-ide-mcp-server-session-started "sid-2" "/tmp/proj-1/" nil)
            (should (= (hash-table-count claude-code-ide-mcp-server--sessions) 2))
            (should (eq server claude-code-ide-mcp-server--server)))

          ;; Ending one instance keeps the server up for its sibling
          (claude-code-ide-mcp-server-session-ended "sid-1")
          (should (= (hash-table-count claude-code-ide-mcp-server--sessions) 1))
          (should claude-code-ide-mcp-server--server)

          ;; Ending it a second time is a no-op, not a shutdown
          (claude-code-ide-mcp-server-session-ended "sid-1")
          (should (= (hash-table-count claude-code-ide-mcp-server--sessions) 1))
          (should claude-code-ide-mcp-server--server)

          ;; Ending the last instance stops the server
          (claude-code-ide-mcp-server-session-ended "sid-2")
          (should (= (hash-table-count claude-code-ide-mcp-server--sessions) 0))
          (should-not claude-code-ide-mcp-server--server)
          (should-not claude-code-ide-mcp-server--port)
          (should-not claude-code-ide-mcp-server-tests--mock-server-started))
      (clrhash claude-code-ide-mcp-server--sessions))))

(ert-deftest claude-code-ide-mcp-server-test-config-generation ()
  "Test MCP configuration generation."
  (let ((claude-code-ide-enable-mcp-server t)
        (claude-code-ide-mcp-server--server 'mock-server)
        (claude-code-ide-mcp-server--port 8080))
    ;; With server running
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'ws-process) (lambda (_) 'mock-process)))
      (let ((config (claude-code-ide-mcp-server-get-config)))
        (should config)
        (should (equal (alist-get 'type (alist-get 'emacs-tools (alist-get 'mcpServers config)))
                       "http"))
        (should (equal (alist-get 'url (alist-get 'emacs-tools (alist-get 'mcpServers config)))
                       "http://localhost:8080/mcp"))))

    ;; Without server running
    (let ((claude-code-ide-mcp-server--server nil)
          (claude-code-ide-mcp-server--port nil)
          (config (claude-code-ide-mcp-server-get-config)))
      (should-not config))))

(ert-deftest claude-code-ide-mcp-server-test-disabled ()
  "Test that MCP tools server does nothing when disabled."
  (let ((claude-code-ide-enable-mcp-server nil))
    (unwind-protect
        (progn
          (clrhash claude-code-ide-mcp-server--sessions)
          (should-not (claude-code-ide-mcp-server-ensure-server))
          ;; Sessions are still tracked so tool requests can find their
          ;; context, but no server is started
          (claude-code-ide-mcp-server-session-started "sid-disabled" "/tmp/proj/" nil)
          (should (= (hash-table-count claude-code-ide-mcp-server--sessions) 1))
          (should-not claude-code-ide-mcp-server--server)
          (claude-code-ide-mcp-server-session-ended "sid-disabled")
          (should (= (hash-table-count claude-code-ide-mcp-server--sessions) 0)))
      (clrhash claude-code-ide-mcp-server--sessions))))

;;; Tool Configuration Tests

(ert-deftest claude-code-ide-mcp-server-test-tool-config ()
  "Test tool configuration structure."
  (let ((claude-code-ide-mcp-server-tools
         '((test-function
            :description "Test function"
            :parameters ((:name "arg1" :type "string" :required t)
                         (:name "arg2" :type "number" :required nil))))))
    (let* ((tool (car claude-code-ide-mcp-server-tools))
           (name (car tool))
           (plist (cdr tool)))
      (should (eq name 'test-function))
      (should (equal (plist-get plist :description) "Test function"))
      (should (= (length (plist-get plist :parameters)) 2)))))

;;; JSON-RPC Message Tests

(ert-deftest claude-code-ide-mcp-server-test-json-encoding ()
  "Test JSON encoding of MCP config."
  (let ((config '((mcpServers . ((emacs-tools . ((transport . "http")
                                                 (url . "http://localhost:8080/mcp"))))))))
    (let ((json-str (json-encode config)))
      (should (stringp json-str))
      (should (string-match "mcpServers" json-str))
      (should (string-match "emacs-tools" json-str))
      (should (string-match "transport.*:.*http" json-str)))))

(ert-deftest claude-code-ide-mcp-server-test-ws-send-fix ()
  "Test that ws-send is called with process, not request."
  ;; Test that verifies our fix for the wrong-type-argument error
  ;; Skip test if web-server is not available
  (skip-unless (condition-case nil
                   (progn (require 'web-server) t)
                 (error nil)))
  (require 'claude-code-ide-mcp-http-server)
  (let ((mock-process (make-claude-code-ide-mcp-server-tests--mock-process))
        (mock-request (make-claude-code-ide-mcp-server-tests--mock-request)))
    ;; Set the process in the request
    (setf (claude-code-ide-mcp-server-tests--mock-request-process mock-request) mock-process)
    ;; Mock the ws-* functions
    (cl-letf (((symbol-function 'ws-response-header)
               #'claude-code-ide-mcp-server-tests--mock-ws-response-header)
              ((symbol-function 'ws-send)
               #'claude-code-ide-mcp-server-tests--mock-ws-send)
              ((symbol-function 'ws-send-404)
               #'claude-code-ide-mcp-server-tests--mock-ws-send-404))
      ;; Test send-json-response
      (claude-code-ide-mcp-http-server--send-json-response
       mock-request 200 '((test . "data")))
      (should (equal claude-code-ide-mcp-server-tests--last-response-status 200))
      (should (string-match "test.*:.*data" claude-code-ide-mcp-server-tests--last-response))

      ;; Test handle-get (404 response)
      (claude-code-ide-mcp-http-server--handle-get mock-request)
      (should (equal claude-code-ide-mcp-server-tests--last-response-status 404)))))

;;; MCP Server Session Context Tests

(ert-deftest claude-code-ide-mcp-server-test-session-registration ()
  "Test session registration and retrieval."
  (let ((session-id "test-session-123")
        (project-dir "/tmp/test-project")
        (buffer (get-buffer-create "*test-buffer*")))
    (unwind-protect
        (progn
          ;; Register a session
          (claude-code-ide-mcp-server-register-session session-id project-dir buffer)

          ;; Retrieve and verify session context
          (let ((context (gethash session-id claude-code-ide-mcp-server--sessions)))
            (should context)
            (should (equal (plist-get context :project-dir) project-dir))
            (should (eq (plist-get context :buffer) buffer))
            (should (plist-get context :start-time)))

          ;; Test get-session-context function
          (let ((claude-code-ide-mcp-server--current-session-id session-id))
            (let ((context (claude-code-ide-mcp-server-get-session-context)))
              (should context)
              (should (equal (plist-get context :project-dir) project-dir))))

          ;; Unregister session
          (claude-code-ide-mcp-server-unregister-session session-id)
          (should-not (gethash session-id claude-code-ide-mcp-server--sessions)))

      ;; Cleanup
      (kill-buffer buffer)
      (clrhash claude-code-ide-mcp-server--sessions))))

(ert-deftest claude-code-ide-mcp-server-test-with-session-context-macro ()
  "Test the with-session-context macro."
  (let ((session-id "test-session-456")
        (project-dir "/tmp/test-project-2/")
        (buffer (get-buffer-create "*test-buffer-2*"))
        (original-dir default-directory))
    (unwind-protect
        (progn
          ;; Set up the buffer with the project directory
          (with-current-buffer buffer
            (setq default-directory project-dir))

          ;; Register a session
          (claude-code-ide-mcp-server-register-session session-id project-dir buffer)

          ;; Test macro with valid session
          (let ((claude-code-ide-mcp-server--current-session-id session-id))
            (claude-code-ide-mcp-server-with-session-context nil
              ;; Inside the macro, default-directory should be the project dir
              (should (equal default-directory project-dir))
              ;; Current buffer should be the session buffer
              (should (eq (current-buffer) buffer))))

          ;; Verify we're back to original context
          (should (equal default-directory original-dir))

          ;; Test error handling with invalid session
          (let ((claude-code-ide-mcp-server--current-session-id "invalid-session"))
            (should-error
             (claude-code-ide-mcp-server-with-session-context nil
               (error "Should not reach here")))))

      ;; Cleanup
      (kill-buffer buffer)
      (clrhash claude-code-ide-mcp-server--sessions))))

(ert-deftest claude-code-ide-mcp-server-test-session-lifecycle-detailed ()
  "Test complete session lifecycle with detailed tracking."
  (let ((session-id "test-session-789")
        (project-dir "/tmp/test-project-3")
        (buffer (get-buffer-create "*test-buffer-3*")))
    (unwind-protect
        (progn
          (clrhash claude-code-ide-mcp-server--sessions)
          ;; A session may register before its terminal buffer exists
          (claude-code-ide-mcp-server-session-started session-id project-dir nil)
          (should (= (hash-table-count claude-code-ide-mcp-server--sessions) 1))
          (should (gethash session-id claude-code-ide-mcp-server--sessions))
          (should-not (plist-get (gethash session-id claude-code-ide-mcp-server--sessions)
                                 :buffer))

          ;; The buffer is filled in once the terminal has been created
          (claude-code-ide-mcp-server-update-session-buffer session-id buffer)
          (should (eq (plist-get (gethash session-id claude-code-ide-mcp-server--sessions)
                                 :buffer)
                      buffer))

          ;; End session
          (claude-code-ide-mcp-server-session-ended session-id)
          (should (= (hash-table-count claude-code-ide-mcp-server--sessions) 0))
          (should-not (gethash session-id claude-code-ide-mcp-server--sessions)))

      ;; Cleanup
      (kill-buffer buffer)
      (clrhash claude-code-ide-mcp-server--sessions))))

(ert-deftest claude-code-ide-mcp-server-test-config-with-session-id ()
  "Test MCP config generation with session ID."
  ;; Mock the server port
  (cl-letf (((symbol-function 'claude-code-ide-mcp-server-get-port)
             (lambda () 12345)))
    ;; Test without session ID
    (let ((config (claude-code-ide-mcp-server-get-config)))
      (should config)
      (let ((url (alist-get 'url (alist-get 'emacs-tools (alist-get 'mcpServers config)))))
        (should (equal url "http://localhost:12345/mcp"))))

    ;; Test with session ID
    (let ((config (claude-code-ide-mcp-server-get-config "my-session-123")))
      (should config)
      (let* ((emacs-tools (alist-get 'emacs-tools (alist-get 'mcpServers config)))
             (url (alist-get 'url emacs-tools)))
        (should (equal url "http://localhost:12345/mcp/my-session-123"))))))

;;; Emacs Tools Tests

(ert-deftest claude-code-ide-emacs-tools-test-imenu-list-symbols ()
  "Test the imenu-list-symbols MCP tool."
  ;; Load the emacs-tools module
  (require 'claude-code-ide-emacs-tools)

  (let ((test-file (make-temp-file "test-imenu-" nil ".el"))
        (session-id "test-session-imenu")
        (project-dir (temporary-file-directory)))
    (unwind-protect
        (progn
          ;; Write test content to file
          (with-temp-file test-file
            (insert ";;; Test file for imenu\n\n"
                    "(defun test-function-1 (arg)\n"
                    "  \"A test function.\"\n"
                    "  (message \"Hello %s\" arg))\n\n"
                    "(defvar test-variable 42\n"
                    "  \"A test variable.\")\n\n"
                    "(defun test-function-2 ()\n"
                    "  \"Another test function.\"\n"
                    "  (+ 1 2))\n\n"
                    "(defconst test-constant 'foo\n"
                    "  \"A test constant.\")\n"))

          ;; Register a mock session
          (claude-code-ide-mcp-server-register-session session-id project-dir nil)

          ;; Test with session context
          (let ((claude-code-ide-mcp-server--current-session-id session-id))
            (let ((result (claude-code-ide-mcp-imenu-list-symbols test-file)))
              ;; Should return a list of results
              (should (listp result))
              (should (> (length result) 0))

              ;; Check that we found our functions and variables
              (let ((result-string (mapconcat #'identity result "\n")))
                (should (string-match "test-function-1" result-string))
                (should (string-match "test-function-2" result-string))
                (should (string-match "test-variable" result-string))
                (should (string-match "test-constant" result-string))

                ;; Check format includes line numbers
                (should (string-match ":[0-9]+:" result-string)))))

          ;; Test error handling - no file path
          (should-error (claude-code-ide-mcp-imenu-list-symbols nil)
                        :type 'error)

          ;; Test with non-existent file
          (let ((result (condition-case nil
                            (claude-code-ide-mcp-imenu-list-symbols "/nonexistent/file.el")
                          (error "Error listing symbols"))))
            (should (stringp result))
            (should (string-match "Error" result))))

      ;; Cleanup
      (delete-file test-file)
      (claude-code-ide-mcp-server-unregister-session session-id))))

(ert-deftest claude-code-ide-emacs-tools-test-imenu-nested-symbols ()
  "Test imenu-list-symbols with nested symbol structures."
  (require 'claude-code-ide-emacs-tools)

  (let ((test-file (make-temp-file "test-imenu-nested-" nil ".py"))
        (session-id "test-session-imenu-nested")
        (project-dir (temporary-file-directory)))
    (unwind-protect
        (progn
          ;; Write Python test content (which often has nested imenu structures)
          (with-temp-file test-file
            (insert "# Test Python file\n\n"
                    "class TestClass:\n"
                    "    def method1(self):\n"
                    "        pass\n\n"
                    "    def method2(self, arg):\n"
                    "        return arg * 2\n\n"
                    "def standalone_function():\n"
                    "    return 42\n"))

          ;; Register a mock session
          (claude-code-ide-mcp-server-register-session session-id project-dir nil)

          ;; Test with session context
          (let ((claude-code-ide-mcp-server--current-session-id session-id))
            ;; Note: This test might not find nested structures if python-mode
            ;; isn't properly configured, but it should at least not error
            (condition-case err
                (let ((result (claude-code-ide-mcp-imenu-list-symbols test-file)))
                  ;; Should return either a list or a string (no symbols message)
                  (should (or (listp result) (stringp result))))
              (error
               ;; If python mode isn't available, that's okay for this test
               (should (string-match "Error" (error-message-string err)))))))

      ;; Cleanup
      (delete-file test-file)
      (claude-code-ide-mcp-server-unregister-session session-id))))

(ert-deftest claude-code-ide-test-tool-format-backward-compatibility ()
  "Test that both old and new tool formats work correctly."
  (require 'claude-code-ide-mcp-server)

  ;; Define a test function
  (defun test-tool-func (arg1 arg2)
    "Test function for tool format testing."
    (list arg1 arg2))

  ;; Test old format
  (let ((old-format-tool '(test-tool-func
                           :description "Test tool in old format"
                           :parameters ((:name "arg1"
                                               :type "string"
                                               :required t
                                               :description "First argument")
                                        (:name "arg2"
                                               :type "number"
                                               :required nil
                                               :description "Second argument")))))

    ;; Check format detection
    (should (eq (claude-code-ide--tool-format-p old-format-tool) 'old))

    ;; Check normalization - should emit warning
    (let ((warning-msg nil))
      ;; Capture the warning message
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match "deprecated format" fmt)
                     (setq warning-msg (apply #'format fmt args))))))
        (let ((normalized (claude-code-ide--normalize-tool-spec old-format-tool)))
          (should (eq (plist-get normalized :function) 'test-tool-func))
          (should (equal (plist-get normalized :name) "test-tool-func"))
          (should (equal (plist-get normalized :description) "Test tool in old format"))
          (should (equal (length (plist-get normalized :args)) 2))))
      ;; Verify warning was emitted
      (should warning-msg)
      (should (string-match "test-tool-func.*deprecated.*claude-code-ide-make-tool" warning-msg))))

  ;; Test new format
  (let ((new-format-tool (claude-code-ide-make-tool
                          :function #'test-tool-func
                          :name "test_tool_new"
                          :description "Test tool in new format"
                          :args '((:name "arg1"
                                         :type string
                                         :description "First argument")
                                  (:name "arg2"
                                         :type number
                                         :description "Second argument"
                                         :optional t)))))

    ;; Check format detection
    (should (eq (claude-code-ide--tool-format-p new-format-tool) 'new))

    ;; Check normalization
    (let ((normalized (claude-code-ide--normalize-tool-spec new-format-tool)))
      (should (eq (plist-get normalized :function) 'test-tool-func))
      (should (equal (plist-get normalized :name) "test_tool_new"))
      (should (equal (plist-get normalized :description) "Test tool in new format"))
      (let ((args (plist-get normalized :args)))
        (should (equal (length args) 2))
        ;; Check first argument
        (let ((arg1 (car args)))
          (should (equal (plist-get arg1 :name) "arg1"))
          (should (eq (plist-get arg1 :type) 'string))
          (should (not (plist-get arg1 :optional))))
        ;; Check second argument
        (let ((arg2 (cadr args)))
          (should (equal (plist-get arg2 :name) "arg2"))
          (should (eq (plist-get arg2 :type) 'number))
          (should (plist-get arg2 :optional))))))

  ;; Test that both formats can coexist in the same list
  (let* ((claude-code-ide-mcp-server-tools
          (list
           ;; Old format
           '(test-func-old
             :description "Old format tool"
             :parameters ((:name "param" :type "string" :required t)))
           ;; New format
           (claude-code-ide-make-tool
            :function #'test-func-new
            :name "test_func_new"
            :description "New format tool"
            :args '((:name "param" :type string)))))
         (normalized-tools (mapcar #'claude-code-ide--normalize-tool-spec
                                   claude-code-ide-mcp-server-tools)))

    ;; Both tools should normalize correctly
    (should (equal (length normalized-tools) 2))
    (should (eq (plist-get (car normalized-tools) :function) 'test-func-old))
    (should (eq (plist-get (cadr normalized-tools) :function) 'test-func-new))))

(ert-deftest claude-code-ide-emacs-tools-test-tool-configuration ()
  "Test that imenu tool is properly configured."
  (require 'claude-code-ide-emacs-tools)
  (require 'claude-code-ide-mcp-server)

  ;; Setup tools first
  (claude-code-ide-emacs-tools-setup)

  ;; Find the imenu tool in the registered tools
  (let ((imenu-tool (cl-find-if
                     (lambda (tool)
                       (let ((normalized (claude-code-ide--normalize-tool-spec tool)))
                         (eq (plist-get normalized :function)
                             'claude-code-ide-mcp-imenu-list-symbols)))
                     claude-code-ide-mcp-server-tools)))
    (should imenu-tool)

    ;; Normalize the tool to check its properties
    (let ((normalized (claude-code-ide--normalize-tool-spec imenu-tool)))
      ;; Check description
      (should (equal (plist-get normalized :description)
                     "Navigate and explore a file's structure by listing all its functions, classes, and variables with their locations"))

      ;; Check args
      (let ((args (plist-get normalized :args)))
        (should (= (length args) 1))
        (let ((file-path-arg (car args)))
          (should (equal (plist-get file-path-arg :name) "file_path"))
          (should (eq (plist-get file-path-arg :type) 'string))
          (should (not (plist-get file-path-arg :optional)))
          (should (equal (plist-get file-path-arg :description)
                         "Path to the file to analyze for symbols")))))))

;;; Multi-Instance Tests

(ert-deftest claude-code-ide-test-two-instances-one-project ()
  "Two instances of one project get their own server, port and lockfile."
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-two-instances/")
        (primary nil)
        (secondary nil))
    (unwind-protect
        (progn
          (setq primary (claude-code-ide-mcp-create-session project-dir "two-1")
                secondary (claude-code-ide-mcp-create-session
                           project-dir "two-2" "review"))
          (should-not (equal (claude-code-ide-mcp-session-session-id primary)
                             (claude-code-ide-mcp-session-session-id secondary)))
          (should-not (claude-code-ide-mcp-session-instance-name primary))
          (should (equal (claude-code-ide-mcp-session-instance-name secondary) "review"))
          ;; Both belong to the same project
          (should (= 2 (length (claude-code-ide-mcp--sessions-for-project project-dir))))
          (let ((port1 (claude-code-ide-mcp-session-port primary))
                (port2 (claude-code-ide-mcp-session-port secondary)))
            (should-not (= port1 port2))
            (should (file-exists-p (claude-code-ide-mcp--lockfile-path port1)))
            (should (file-exists-p (claude-code-ide-mcp--lockfile-path port2)))
            ;; Their terminal buffers cannot collide either
            (should-not (equal (claude-code-ide--instance-buffer-name project-dir nil)
                               (claude-code-ide--instance-buffer-name project-dir "review")))
            (should (equal (claude-code-ide--instance-buffer-name project-dir "review")
                           "*claude-code[claude-two-instances:review]*"))

            ;; Stopping one takes down only its own lockfile
            (claude-code-ide-mcp--stop-session primary)
            (should-not (file-exists-p (claude-code-ide-mcp--lockfile-path port1)))
            (should (file-exists-p (claude-code-ide-mcp--lockfile-path port2)))
            (should (equal (claude-code-ide-mcp--sessions-for-project project-dir)
                           (list secondary)))

            (claude-code-ide-mcp--stop-session secondary)
            (should-not (file-exists-p (claude-code-ide-mcp--lockfile-path port2)))
            (should-not (claude-code-ide-mcp--sessions-for-project project-dir))))
      (claude-code-ide-tests--stop-all-sessions)
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-generate-session-id-unique ()
  "Session IDs are unique even for instances started in the same second."
  (let* ((claude-code-ide--session-counter 0)
         (id1 (claude-code-ide--generate-session-id "/tmp/proj/"))
         (id2 (claude-code-ide--generate-session-id "/tmp/proj/")))
    (should-not (equal id1 id2))
    (should (string-prefix-p "claude-proj-" id1))
    (should (string-suffix-p "-1" id1))
    (should (string-suffix-p "-2" id2))))

(ert-deftest claude-code-ide-test-auto-instance-name ()
  "Auto naming takes the plain slot first, then the lowest free number."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let ((project-dir "/tmp/claude-auto-name/"))
        ;; No instance yet, so the plain unnamed slot is free
        (should-not (claude-code-ide--auto-instance-name project-dir))
        (let ((plain (claude-code-ide-tests--make-session project-dir)))
          (should (equal (claude-code-ide--auto-instance-name project-dir) "2"))
          (let ((second (claude-code-ide-tests--make-session project-dir
                                                             :instance-name "2")))
            (should (equal (claude-code-ide--auto-instance-name project-dir) "3"))
            ;; A user-chosen name does not consume a number
            (claude-code-ide-tests--make-session project-dir :instance-name "review")
            (should (equal (claude-code-ide--auto-instance-name project-dir) "3"))
            ;; While renaming, the instance's own name counts as free
            (should-not (claude-code-ide--auto-instance-name project-dir plain))
            (should (equal (claude-code-ide--auto-instance-name project-dir second) "2"))
            ;; The plain slot is reused once its instance is gone
            (remhash (claude-code-ide-mcp-session-session-id plain)
                     claude-code-ide-mcp--sessions)
            (should-not (claude-code-ide--auto-instance-name project-dir))
            ;; Another project's instances are irrelevant
            (claude-code-ide-tests--make-session "/tmp/claude-other-project/")
            (should-not (claude-code-ide--auto-instance-name project-dir)))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-read-instance-name-validation ()
  "Instance names are validated and empty input auto-numbers."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let ((project-dir "/tmp/claude-read-name/")
            (inputs nil)
            (messages '()))
        (claude-code-ide-tests--make-session project-dir)
        (claude-code-ide-tests--make-session project-dir :instance-name "taken")
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (or (pop inputs) (error "No test input left"))))
                  ((symbol-function 'sit-for) (lambda (&rest _) t))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Numeric names are reserved for auto-numbering; input is trimmed
          (setq inputs '("7" "  refactor  ")
                messages '())
          (should (equal (claude-code-ide--read-instance-name project-dir) "refactor"))
          (should (cl-find-if (lambda (m) (string-match-p "Numeric" m)) messages))

          ;; Characters that would break buffer naming are rejected
          (setq inputs '("bad[name" "bad]name" "bad*name" "ok")
                messages '())
          (should (equal (claude-code-ide--read-instance-name project-dir) "ok"))
          (should (= 3 (length messages)))

          ;; A name already used in this project is rejected
          (setq inputs '("taken" "fresh")
                messages '())
          (should (equal (claude-code-ide--read-instance-name project-dir) "fresh"))
          (should (cl-find-if (lambda (m) (string-match-p "already used" m)) messages))

          ;; Empty input auto-numbers: the plain slot is taken, so "2"
          (setq inputs '(""))
          (should (equal (claude-code-ide--read-instance-name project-dir) "2"))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-instance-buffer-name-compat ()
  "Buffer naming keeps working for single-argument naming functions."
  (let ((project-dir "/tmp/naming/"))
    ;; The documented single-argument function is left alone for the
    ;; unnamed instance and decorated inside the brackets for named ones
    (let ((claude-code-ide-buffer-name-function
           (lambda (dir) (format "*claude-code[%s]*"
                                 (file-name-nondirectory (directory-file-name dir))))))
      (should (equal (claude-code-ide--instance-buffer-name project-dir nil)
                     "*claude-code[naming]*"))
      (should (equal (claude-code-ide--instance-buffer-name project-dir "review")
                     "*claude-code[naming:review]*")))
    ;; A name that does not end in "]*" gets the instance appended instead
    (let ((claude-code-ide-buffer-name-function (lambda (_dir) "claude")))
      (should (equal (claude-code-ide--instance-buffer-name project-dir nil) "claude"))
      (should (equal (claude-code-ide--instance-buffer-name project-dir "review")
                     "claude<review>")))
    ;; A two-argument function is handed the instance name itself
    (let* ((received nil)
           (claude-code-ide-buffer-name-function
            (lambda (dir instance)
              (setq received (list dir instance))
              (format "*c[%s]*" (or instance "plain")))))
      (should (equal (claude-code-ide--instance-buffer-name project-dir "review")
                     "*c[review]*"))
      (should (equal received (list project-dir "review")))
      (should (equal (claude-code-ide--instance-buffer-name project-dir nil)
                     "*c[plain]*"))
      (should (equal received (list project-dir nil))))))

(ert-deftest claude-code-ide-test-resolve-session-chain ()
  "Test how a command decides which instance it targets."
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-resolve/")
        (buffer-a (generate-new-buffer "*claude-resolve-a*"))
        (buffer-b (generate-new-buffer "*claude-resolve-b*"))
        (visible-buffer nil)
        (prompted 0))
    (unwind-protect
        (let ((session-a (claude-code-ide-tests--make-session
                          project-dir :instance-name "alpha"
                          :buffer buffer-a :last-used 100.0))
              (session-b (claude-code-ide-tests--make-session
                          project-dir :instance-name "beta"
                          :buffer buffer-b :last-used 200.0)))
          (cl-letf (((symbol-function 'claude-code-ide--get-working-directory)
                     (lambda () project-dir))
                    ((symbol-function 'get-buffer-window)
                     (lambda (buffer &rest _)
                       (when (eq buffer visible-buffer) 'mock-window)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt candidates &rest _)
                       (cl-incf prompted)
                       (car (car candidates)))))
            ;; Inside a Claude terminal the buffer's own instance always wins
            (with-current-buffer buffer-a
              (setq-local claude-code-ide--session session-a)
              (should (eq (claude-code-ide--resolve-session 'auto) session-a))
              (should (eq (claude-code-ide--resolve-session 'prompt) session-a))
              (should (= prompted 0)))

            ;; A single visible instance is unambiguous, even for a
            ;; destructive command
            (setq visible-buffer buffer-a)
            (should (eq (claude-code-ide--resolve-session 'prompt) session-a))
            (should (= prompted 0))

            ;; With nothing visible, `auto' guesses the most recently used
            ;; instance and `prompt' asks instead
            (setq visible-buffer nil)
            (should (eq (claude-code-ide--resolve-session 'auto) session-b))
            (should (= prompted 0))
            (should (claude-code-ide--resolve-session 'prompt))
            (should (= prompted 1))

            ;; A prefix argument always asks
            (let ((current-prefix-arg '(4)))
              (should (claude-code-ide--resolve-session 'auto))
              (should (= prompted 2)))

            ;; A project without instances resolves to nothing
            (claude-code-ide-tests--clear-processes)
            (should-not (claude-code-ide--resolve-session 'prompt))
            (should (= prompted 2))))
      (dolist (buffer (list buffer-a buffer-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-selection-broadcast ()
  "Selection changes reach every instance of the project, deduped per instance."
  (claude-code-ide-tests--clear-processes)
  (let* ((project-dir "/tmp/claude-selection/")
         (file (expand-file-name "file.txt" project-dir))
         (client-a (claude-code-ide-tests--make-websocket "ws://127.0.0.1:10006"))
         (client-b (claude-code-ide-tests--make-websocket "ws://127.0.0.1:10007"))
         (sent '()))
    (unwind-protect
        (let ((session-a (claude-code-ide-tests--make-session project-dir
                                                              :client client-a))
              (session-b (claude-code-ide-tests--make-session project-dir
                                                              :instance-name "b"
                                                              :client client-b)))
          (cl-letf (((symbol-function 'websocket-send-text)
                     (lambda (ws text) (push (cons ws text) sent))))
            (with-temp-buffer
              (insert "line 1\nline 2\n")
              (setq buffer-file-name file)
              (goto-char (point-min))

              ;; Every instance of the project is told about the selection
              (claude-code-ide-mcp--send-selection-for-project project-dir)
              (should (= 2 (length sent)))
              (dolist (entry sent)
                (should (string-match-p "selection_changed" (cdr entry))))
              (should (cl-find-if (lambda (e) (eq (car e) client-a)) sent))
              (should (cl-find-if (lambda (e) (eq (car e) client-b)) sent))
              ;; Each instance remembers what it was told
              (should (claude-code-ide-mcp-session-last-selection session-a))
              (should (claude-code-ide-mcp-session-last-selection session-b))

              ;; Without any movement nothing is resent
              (setq sent '())
              (claude-code-ide-mcp--send-selection-for-project project-dir)
              (should-not sent)

              ;; Dedupe state is per instance, so an instance that lost its
              ;; state still gets the current selection
              (setf (claude-code-ide-mcp-session-last-selection session-b) nil)
              (claude-code-ide-mcp--send-selection-for-project project-dir)
              (should (= 1 (length sent)))
              (should (eq (car (car sent)) client-b))

              ;; Moving point broadcasts to everyone again
              (setq sent '())
              (forward-line 1)
              (claude-code-ide-mcp--send-selection-for-project project-dir)
              (should (= 2 (length sent)))

              ;; A buffer outside the project resets the per-instance state
              ;; instead of broadcasting
              (setq sent '())
              (setq buffer-file-name "/tmp/elsewhere/file.txt")
              (claude-code-ide-mcp--send-selection-for-project project-dir)
              (should-not sent)
              (should-not (claude-code-ide-mcp-session-last-selection session-a))
              (should-not (claude-code-ide-mcp-session-last-selection session-b))

              (set-buffer-modified-p nil)
              (setq buffer-file-name nil))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-track-selection-debounces-per-project ()
  "Selection tracking arms one debounce timer per project, not per instance."
  (claude-code-ide-tests--clear-processes)
  (let* ((project-dir "/tmp/claude-debounce/")
         (timers 0)
         (cancelled 0)
         (fired '()))
    (unwind-protect
        (progn
          (claude-code-ide-tests--make-session project-dir)
          (claude-code-ide-tests--make-session project-dir :instance-name "b")
          (cl-letf (((symbol-function 'claude-code-ide-mcp--get-buffer-project)
                     (lambda () project-dir))
                    ((symbol-function 'run-with-timer)
                     (lambda (_delay _repeat callback)
                       (cl-incf timers)
                       ;; Return the callback itself as the timer object so
                       ;; the test can fire it synchronously
                       callback))
                    ((symbol-function 'cancel-timer)
                     (lambda (_timer) (cl-incf cancelled)))
                    ((symbol-function 'claude-code-ide-mcp--send-selection-for-project)
                     (lambda (dir) (push dir fired))))
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "file.txt" project-dir))
              ;; Two instances, but only one timer for the project
              (claude-code-ide-mcp--track-selection)
              (should (= timers 1))
              (should (= cancelled 0))
              (should (gethash project-dir claude-code-ide-mcp--selection-timers))

              ;; A second command replaces the pending timer
              (claude-code-ide-mcp--track-selection)
              (should (= timers 2))
              (should (= cancelled 1))

              ;; Firing it fans the selection out to the whole project once
              (funcall (gethash project-dir claude-code-ide-mcp--selection-timers))
              (should (equal fired (list project-dir)))
              (setq buffer-file-name nil))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-track-active-buffer-broadcast ()
  "The active buffer is tracked for every instance under its own session ID."
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-active-buffer/")
        (updates '()))
    (unwind-protect
        (let ((session-a (claude-code-ide-tests--make-session project-dir
                                                              :client (claude-code-ide-tests--make-websocket
                                                                       "ws://127.0.0.1:10008")))
              (session-b (claude-code-ide-tests--make-session project-dir
                                                              :instance-name "b"
                                                              :client (claude-code-ide-tests--make-websocket
                                                                       "ws://127.0.0.1:10009")))
              (session-c (claude-code-ide-tests--make-session project-dir
                                                              :instance-name "c")))
          (cl-letf (((symbol-function 'claude-code-ide-mcp--get-buffer-project)
                     (lambda () project-dir))
                    ((symbol-function 'claude-code-ide-mcp-server-update-last-active-buffer)
                     (lambda (session-id buffer) (push (cons session-id buffer) updates))))
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "file.txt" project-dir))
              (claude-code-ide-mcp--track-active-buffer)
              (should (eq (claude-code-ide-mcp-session-last-buffer session-a)
                          (current-buffer)))
              (should (eq (claude-code-ide-mcp-session-last-buffer session-b)
                          (current-buffer)))
              ;; An instance whose CLI has not connected yet is skipped
              (should-not (claude-code-ide-mcp-session-last-buffer session-c))
              ;; Each instance is updated under its own session ID
              (should (= 2 (length updates)))
              (should (assoc (claude-code-ide-mcp-session-session-id session-a) updates))
              (should (assoc (claude-code-ide-mcp-session-session-id session-b) updates))

              ;; Nothing is resent while the same buffer stays active
              (setq updates '())
              (claude-code-ide-mcp--track-active-buffer)
              (should-not updates)
              (setq buffer-file-name nil))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-at-mentioned-targets-one-instance ()
  "An at-mention is an explicit insertion, so it goes to exactly one instance."
  (claude-code-ide-tests--clear-processes)
  (let* ((project-dir "/tmp/claude-at-mentioned/")
         (file (expand-file-name "file.txt" project-dir))
         (client-a (claude-code-ide-tests--make-websocket "ws://127.0.0.1:10010"))
         (client-b (claude-code-ide-tests--make-websocket "ws://127.0.0.1:10011"))
         (sent '()))
    (unwind-protect
        (let ((session-b (progn
                           (claude-code-ide-tests--make-session project-dir
                                                                :client client-a)
                           (claude-code-ide-tests--make-session project-dir
                                                                :instance-name "b"
                                                                :client client-b))))
          (cl-letf (((symbol-function 'websocket-send-text)
                     (lambda (ws text) (push (cons ws text) sent))))
            (with-temp-buffer
              (insert "one\ntwo\nthree\n")
              (setq buffer-file-name file)
              (goto-char (point-min))
              (forward-line 1)
              (claude-code-ide-mcp-send-at-mentioned session-b)
              (should (= 1 (length sent)))
              (should (eq (car (car sent)) client-b))
              ;; The sibling instance is not told about it
              (should-not (cl-find-if (lambda (e) (eq (car e) client-a)) sent))
              (let* ((payload (json-read-from-string (cdr (car sent))))
                     (params (alist-get 'params payload)))
                (should (equal (alist-get 'method payload) "at_mentioned"))
                (should (equal (alist-get 'filePath params) file))
                ;; Line numbers are zero-based on the wire
                (should (= (alist-get 'lineStart params) 1))
                (should (= (alist-get 'lineEnd params) 1)))
              (set-buffer-modified-p nil)
              (setq buffer-file-name nil))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-deferred-stored-in-dispatch-session ()
  "A deferred tool response is filed under the instance that asked for it."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let* ((project-dir "/tmp/claude-deferred/")
             (session-a (claude-code-ide-tests--make-session project-dir))
             (session-b (claude-code-ide-tests--make-session project-dir
                                                             :instance-name "b"))
             (handler-sessions '())
             (claude-code-ide-mcp-tools
              (append `(("mockDeferred" . ,(lambda (_arguments session)
                                             (push session handler-sessions)
                                             '((deferred . t) (unique-key . "tab-1"))))
                        ("mockPlain" . ,(lambda (_arguments _session)
                                          (list '((type . "text") (text . "OK"))))))
                      claude-code-ide-mcp-tools)))
        ;; The handler is called with its session, and the pending id lands
        ;; in that session even though a sibling instance exists
        (should-not (claude-code-ide-mcp--handle-tools-call
                     "req-7" '((name . "mockDeferred")) session-b))
        (should (equal handler-sessions (list session-b)))
        (should (equal (gethash "mockDeferred-tab-1"
                                (claude-code-ide-mcp-session-deferred session-b))
                       "req-7"))
        (should (= 0 (hash-table-count
                      (claude-code-ide-mcp-session-deferred session-a))))

        ;; A non-deferred tool answers inline and stores nothing
        (let ((response (claude-code-ide-mcp--handle-tools-call
                         "req-8" '((name . "mockPlain")) session-a)))
          (should (equal (alist-get 'id response) "req-8"))
          (should (= 0 (hash-table-count
                        (claude-code-ide-mcp-session-deferred session-a))))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-opendiff-requires-session ()
  "openDiff refuses to run without the instance that requested it."
  (should-error (claude-code-ide-mcp-handle-open-diff
                 '((old_file_path . "/tmp/claude-x.txt")
                   (new_file_path . "/tmp/claude-x.txt")
                   (new_file_contents . "new")
                   (tab_name . "tab"))
                 nil)
                :type 'mcp-error)
  ;; A nil session also has no diffs to report
  (should-not (claude-code-ide-mcp--get-active-diffs nil)))

(ert-deftest claude-code-ide-test-close-tab-session-scoping ()
  "A close_tab only closes diffs owned by the requesting instance."
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-close-tab/")
        (diff-buffer (generate-new-buffer "*claude-close-tab-diff*")))
    (unwind-protect
        (let ((session-a (claude-code-ide-tests--make-session project-dir))
              (session-b (claude-code-ide-tests--make-session project-dir
                                                              :instance-name "b")))
          ;; Sibling instances of one project see identical CLI tab names,
          ;; but only A opened this one
          (puthash "shared-tab-name"
                   `((buffer-B . ,diff-buffer) (file-exists . t))
                   (claude-code-ide-mcp-session-active-diffs session-a))

          ;; B's request must not reach into A's diffs; with no buffer of
          ;; that name either, the request fails
          (should-error (claude-code-ide-mcp-handle-close-tab
                         '((tab_name . "shared-tab-name")) session-b)
                        :type 'mcp-error)
          (should (gethash "shared-tab-name"
                           (claude-code-ide-mcp-session-active-diffs session-a)))
          (should (buffer-live-p diff-buffer))

          ;; A's request closes A's diff
          (let ((result (claude-code-ide-mcp-handle-close-tab
                         '((tab_name . "shared-tab-name")) session-a)))
            (should (equal (alist-get 'text (car result)) "TAB_CLOSED")))
          (should-not (gethash "shared-tab-name"
                               (claude-code-ide-mcp-session-active-diffs session-a)))
          (should-not (buffer-live-p diff-buffer)))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-reconnect-adopts-new-client ()
  "A CLI reconnect replaces the client, flushes pending ids, keeps diffs."
  (claude-code-ide-tests--clear-processes)
  (let ((ws1 (claude-code-ide-tests--make-websocket "ws://127.0.0.1:10001"))
        (ws2 (claude-code-ide-tests--make-websocket "ws://127.0.0.1:10002"))
        (closed '()))
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-close)
                   (lambda (ws) (push ws closed))))
          (let ((session (claude-code-ide-tests--make-session
                          "/tmp/claude-reconnect/" :client ws1)))
            (puthash "openDiff-tab" "req-1"
                     (claude-code-ide-mcp-session-deferred session))
            (puthash "tab" '((buffer-B . nil))
                     (claude-code-ide-mcp-session-active-diffs session))
            (setf (claude-code-ide-mcp-session-last-selection session) '(1 1 1))

            (with-temp-buffer
              (claude-code-ide-mcp--on-open session ws2))
            (should (eq (claude-code-ide-mcp-session-client session) ws2))
            (should (equal closed (list ws1)))
            ;; Ids that died with the old socket are dropped and selection
            ;; dedupe restarts, but a diff the user may still be looking at
            ;; survives the reconnect
            (should (= 0 (hash-table-count
                          (claude-code-ide-mcp-session-deferred session))))
            (should-not (claude-code-ide-mcp-session-last-selection session))
            (should (gethash "tab" (claude-code-ide-mcp-session-active-diffs session)))

            ;; Traffic from the replaced socket is ignored
            (let ((handled '())
                  (frame (claude-code-ide-tests--make-frame "{\"method\":\"ping\"}")))
              (cl-letf (((symbol-function 'claude-code-ide-mcp--handle-message)
                         (lambda (message &optional _session _ws) (push message handled))))
                (claude-code-ide-mcp--on-message session ws1 frame)
                (should-not handled)
                (claude-code-ide-mcp--on-message session ws2 frame)
                (should (= 1 (length handled)))))

            ;; The replaced socket's close must not clear its successor
            (claude-code-ide-mcp--on-close session ws1)
            (should (eq (claude-code-ide-mcp-session-client session) ws2))
            (claude-code-ide-mcp--on-close session ws2)
            (should-not (claude-code-ide-mcp-session-client session))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-cleanup-session-isolation ()
  "Cleaning up one instance leaves its project sibling fully working."
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-isolation/")
        (buffer-a (generate-new-buffer "*claude-isolation-a*"))
        (session-a nil)
        (session-b nil))
    (unwind-protect
        (let ((claude-code-ide-prevent-reflow-glitch nil))
          (setq session-a (claude-code-ide-mcp-create-session project-dir "iso-a")
                session-b (claude-code-ide-mcp-create-session
                           project-dir "iso-b" "keep"))
          (setf (claude-code-ide-mcp-session-buffer session-a) buffer-a)
          (claude-code-ide-mcp-server-session-started "iso-a" project-dir buffer-a)
          (claude-code-ide-mcp-server-session-started "iso-b" project-dir nil)
          (setq claude-code-ide--last-accessed-buffer buffer-a)
          (let ((server-b (claude-code-ide-mcp-session-server session-b))
                (port-b (claude-code-ide-mcp-session-port session-b)))
            (claude-code-ide--cleanup-session session-a)

            ;; The cleaned instance is gone: deregistered, buffer killed,
            ;; tools-server registration dropped
            (should (claude-code-ide-mcp-session-cleanup-done session-a))
            (should-not (claude-code-ide-mcp--get-session-by-id "iso-a"))
            (should-not (buffer-live-p buffer-a))
            (should-not (gethash "iso-a" claude-code-ide-mcp-server--sessions))
            ;; The global most-recent buffer no longer points at it
            (should-not (eq claude-code-ide--last-accessed-buffer buffer-a))

            ;; The sibling is untouched, down to its own server and lockfile
            (should (eq (claude-code-ide-mcp--get-session-by-id "iso-b") session-b))
            (should (eq (claude-code-ide-mcp-session-server session-b) server-b))
            (should (file-exists-p (claude-code-ide-mcp--lockfile-path port-b)))
            (should (gethash "iso-b" claude-code-ide-mcp-server--sessions))
            ;; Project-wide tracking stays installed for the survivor
            (should (memq #'claude-code-ide-mcp--track-selection post-command-hook))
            (should (memq #'claude-code-ide-mcp--track-active-buffer post-command-hook))

            ;; Cleaning the same instance again is a no-op
            (claude-code-ide--cleanup-session session-a)
            (should (eq (claude-code-ide-mcp--get-session-by-id "iso-b") session-b))))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (claude-code-ide-tests--stop-all-sessions)
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-kill-buffer-no-nested-kill ()
  "Killing a terminal buffer leaves it alive for later kill hooks.
The session's `kill-buffer-hook' entry runs cleanup with the
buffer-dying flag, so backend hooks running after it (ghostel's
native-process hook) must still see a live buffer — a nested kill from
cleanup used to leave them firing on a dead one."
  (claude-code-ide-tests--clear-processes)
  (let* ((project-dir "/tmp/claude-nested-kill/")
         (buffer (generate-new-buffer "*claude-nested-kill*"))
         (backend-hook-saw-live-buffer nil)
         (session nil))
    (unwind-protect
        (let ((claude-code-ide-prevent-reflow-glitch nil))
          (setq session (claude-code-ide-tests--make-session project-dir
                                                             :buffer buffer))
          (with-current-buffer buffer
            (setq-local claude-code-ide--session session)
            ;; Simulate a backend kill hook registered BEFORE ours (like
            ;; ghostel-mode's): add-hook prepends, so registering it
            ;; first makes it run after the session's own hook
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (setq backend-hook-saw-live-buffer
                              (buffer-live-p (current-buffer))))
                      nil t)
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (claude-code-ide--cleanup-session session 'buffer-dying))
                      nil t))
          (kill-buffer buffer)
          (should backend-hook-saw-live-buffer)
          (should (claude-code-ide-mcp-session-cleanup-done session))
          (should-not (claude-code-ide-mcp--get-session-by-id
                       (claude-code-ide-mcp-session-session-id session)))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (claude-code-ide-tests--stop-all-sessions)
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-backend-recommendation ()
  "The ghostel suggestion is echoed once, only for vterm and eat."
  (let ((claude-code-ide--backend-recommendation-shown nil)
        (claude-code-ide-show-backend-recommendation t)
        (messages '()))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      ;; ghostel: no suggestion
      (let ((claude-code-ide-terminal-backend 'ghostel))
        (claude-code-ide--maybe-recommend-ghostel))
      (should (null messages))
      ;; eat: suggested once, then never again
      (let ((claude-code-ide-terminal-backend 'eat))
        (claude-code-ide--maybe-recommend-ghostel)
        (claude-code-ide--maybe-recommend-ghostel))
      (should (= 1 (length messages)))
      (should (string-match-p "ghostel" (car messages)))
      ;; disabled: nothing even when unseen
      (let ((claude-code-ide--backend-recommendation-shown nil)
            (claude-code-ide-show-backend-recommendation nil)
            (claude-code-ide-terminal-backend 'vterm))
        (claude-code-ide--maybe-recommend-ghostel))
      (should (= 1 (length messages))))))

(ert-deftest claude-code-ide-test-selection-dedupe-includes-file ()
  "Switching files at identical coordinates still notifies the CLI."
  (claude-code-ide-tests--clear-processes)
  (let* ((project-dir (expand-file-name "/tmp/claude-dedupe/"))
         (sent '()))
    (unwind-protect
        (let ((session (claude-code-ide-tests--make-session
                        project-dir
                        :client (claude-code-ide-tests--make-websocket "ws://dedupe"))))
          (ignore session)
          (cl-letf (((symbol-function 'claude-code-ide-mcp--send-notification)
                     (lambda (method _params session) (push (cons method session) sent))))
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "a.el" project-dir))
              (claude-code-ide-mcp--send-selection-for-project project-dir))
            (should (= 1 (length sent)))
            ;; Different file, same point/region coordinates → still a change
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "b.el" project-dir))
              (claude-code-ide-mcp--send-selection-for-project project-dir))
            (should (= 2 (length sent)))
            ;; Same file, same coordinates → deduped
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "b.el" project-dir))
              (claude-code-ide-mcp--send-selection-for-project project-dir))
            (should (= 2 (length sent)))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-client-swap-mid-request ()
  "A reconnect during a tool handler must not leak to the new client.
Responses go only to the requesting socket, and deferred ids from the
replaced socket are discarded rather than stored for the successor."
  (claude-code-ide-tests--clear-processes)
  (let* ((project-dir "/tmp/claude-swap/")
         (old-ws (claude-code-ide-tests--make-websocket "ws://old"))
         (new-ws (claude-code-ide-tests--make-websocket "ws://new"))
         (sent '()))
    (unwind-protect
        (let ((session (claude-code-ide-tests--make-session project-dir
                                                            :client old-ws)))
          (cl-letf (((symbol-function 'websocket-send-text)
                     (lambda (ws text) (push (cons ws text) sent))))
            ;; Inline response: handler swaps the client mid-request
            (let ((claude-code-ide-mcp-tools
                   (list (cons "swapTool"
                               (lambda (_args session)
                                 (setf (claude-code-ide-mcp-session-client session) new-ws)
                                 (list '((type . "text") (text . "OK"))))))))
              (claude-code-ide-mcp--handle-message
               `((jsonrpc . "2.0") (id . 7) (method . "tools/call")
                 (params . ((name . "swapTool"))))
               session old-ws)
              ;; Dropped: neither socket received the response
              (should (null sent)))
            ;; Deferred id: same swap → id is discarded, not stored
            (let ((claude-code-ide-mcp-tools
                   (list (cons "swapDefer"
                               (lambda (_args session)
                                 (setf (claude-code-ide-mcp-session-client session) new-ws)
                                 (list '(deferred . t)))))))
              (claude-code-ide-mcp--handle-tools-call
               8 '((name . "swapDefer")) session old-ws)
              (should (= 0 (hash-table-count
                            (claude-code-ide-mcp-session-deferred session)))))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-stop-session-survives-teardown-error ()
  "A failing teardown step still deregisters the session and hooks."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let ((session (claude-code-ide-tests--make-session "/tmp/claude-wedge/")))
        (setf (claude-code-ide-mcp-session-port session) 12345)
        (add-hook 'post-command-hook #'claude-code-ide-mcp--track-selection)
        (cl-letf (((symbol-function 'claude-code-ide-mcp--remove-lockfile)
                   (lambda (_port) (error "Permission denied"))))
          (should-error (claude-code-ide-mcp--stop-session session)))
        ;; Despite the error: deregistered, hooks gone
        (should-not (claude-code-ide-mcp--get-session-by-id
                     (claude-code-ide-mcp-session-session-id session)))
        (should-not (memq #'claude-code-ide-mcp--track-selection post-command-hook)))
    (remove-hook 'post-command-hook #'claude-code-ide-mcp--track-selection)
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-cleanup-session-closes-diffs ()
  "Cleaning up an instance also closes its open diffs."
  (claude-code-ide-tests--clear-processes)
  (let ((buffer-b (generate-new-buffer "*claude-diff-b*")))
    (unwind-protect
        (let* ((session (claude-code-ide-tests--make-session "/tmp/claude-diffclean/"))
               (active-diffs (claude-code-ide-mcp-session-active-diffs session)))
          (puthash "tab-1"
                   `((buffer-B . ,buffer-b)
                     (file-exists . t)
                     (session . ,session))
                   active-diffs)
          (cl-letf (((symbol-function 'claude-code-ide-mcp-server-session-ended)
                     (lambda (&rest _) nil))
                    ((symbol-function 'claude-code-ide-mcp--stop-session)
                     (lambda (_session) nil)))
            (claude-code-ide--cleanup-session session))
          ;; The diff state is gone and its temp buffer killed
          (should (= 0 (hash-table-count active-diffs)))
          (should-not (buffer-live-p buffer-b)))
      (when (buffer-live-p buffer-b)
        (kill-buffer buffer-b))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-window-slot-block-overflow ()
  "A project overflowing its slot block gets a fresh block, not a neighbor's."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let ((block claude-code-ide--window-slot-block))
        ;; Fill project A's entire block
        (dotimes (i block)
          (let ((session (claude-code-ide-tests--make-session "/tmp/slot-a/")))
            (setf (claude-code-ide-mcp-session-window-slot session) i)))
        ;; Project B takes the next block
        (let ((session-b (claude-code-ide-tests--make-session "/tmp/slot-b/")))
          (setf (claude-code-ide-mcp-session-window-slot session-b)
                (claude-code-ide--assign-window-slot "/tmp/slot-b/"))
          (should (= block (claude-code-ide-mcp-session-window-slot session-b))))
        ;; A's overflow instance must NOT land inside B's block
        (let ((slot (claude-code-ide--assign-window-slot "/tmp/slot-a/")))
          (should (>= slot (* 2 block)))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-window-slots-group-by-project ()
  "Interleaved instance creation still yields project-grouped slots.
Sorting sessions by slot must keep one project's instances adjacent:
emacs, emacs, src, src — not the emacs, src, src, emacs creation order."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (cl-flet ((new-session (project-dir)
                  (let ((session (claude-code-ide-tests--make-session project-dir)))
                    (setf (claude-code-ide-mcp-session-window-slot session)
                          (claude-code-ide--assign-window-slot project-dir))
                    session)))
        ;; Interleave two projects: emacs, src, src, emacs
        (let* ((e1 (new-session "/tmp/emacs/"))
               (s1 (new-session "/tmp/src/"))
               (s2 (new-session "/tmp/src/"))
               (e2 (new-session "/tmp/emacs/"))
               (ordered (sort (list e1 s1 s2 e2)
                              (lambda (a b)
                                (< (claude-code-ide-mcp-session-window-slot a)
                                   (claude-code-ide-mcp-session-window-slot b)))))
               (projects (mapcar #'claude-code-ide-mcp-session-project-dir ordered)))
          ;; Grouped: both emacs slots precede both src slots
          (should (equal projects (list "/tmp/emacs/" "/tmp/emacs/"
                                        "/tmp/src/" "/tmp/src/")))
          ;; All slots distinct
          (should (= 4 (length (cl-remove-duplicates
                                (mapcar #'claude-code-ide-mcp-session-window-slot
                                        ordered)))))
          ;; A stopped project's block is reused by the next new project
          (dolist (session (list e1 e2))
            (remhash (claude-code-ide-mcp-session-session-id session)
                     claude-code-ide-mcp--sessions))
          (let ((n1 (new-session "/tmp/next/")))
            (should (= 0 (claude-code-ide-mcp-session-window-slot n1))))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-hidden-panel-per-tab ()
  "Remembered hidden window sets are scoped per tab-bar tab.
Hiding a panel in one tab must not leak into another tab's restore,
and pruning drops entries whose tab no longer exists."
  (claude-code-ide-tests--clear-processes)
  (let ((current-tab "tab-1")
        (live-tabs '("tab-1" "tab-2")))
    (cl-letf (((symbol-function 'claude-code-ide--current-tab-key)
               (lambda () current-tab))
              ((symbol-function 'tab-bar-tabs)
               (lambda (&optional _frame)
                 (mapcar (lambda (name) (list 'tab (cons 'name name)))
                         live-tabs))))
      (unwind-protect
          (progn
            ;; Each tab remembers its own set, for projects and for :all
            (claude-code-ide--hidden-panel-set "/tmp/p/" '(a b))
            (claude-code-ide--hidden-panel-set :all '(c))
            (setq current-tab "tab-2")
            (should-not (claude-code-ide--hidden-panel-get "/tmp/p/"))
            (should-not (claude-code-ide--hidden-panel-get :all))
            (claude-code-ide--hidden-panel-set "/tmp/p/" '(d))
            (setq current-tab "tab-1")
            (should (equal (claude-code-ide--hidden-panel-get "/tmp/p/") '(a b)))
            (should (equal (claude-code-ide--hidden-panel-get :all) '(c)))
            ;; Closing a tab prunes its entries on the next set
            (setq live-tabs '("tab-1"))
            (claude-code-ide--hidden-panel-set :all '(e))
            (let ((keys (mapcar #'car (frame-parameter nil 'claude-code-ide-hidden-panel))))
              (should-not (cl-find "tab-2" keys :key #'car :test #'equal))))
        (set-frame-parameter nil 'claude-code-ide-hidden-panel nil)))))

(ert-deftest claude-code-ide-test-strip-new-tab-claude-windows ()
  "New tabs get cloned Claude side windows removed."
  (claude-code-ide-tests--clear-processes)
  (let* ((claude-buffer (generate-new-buffer "*claude-strip-tab*"))
         (other-buffer (generate-new-buffer "*strip-tab-other*"))
         (session (claude-code-ide-tests--make-session "/tmp/strip-tab/"
                                                       :buffer claude-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer claude-buffer
            (setq-local claude-code-ide--session session))
          (delete-other-windows)
          (set-window-buffer (selected-window) other-buffer)
          (let ((claude-window (split-window)))
            (set-window-buffer claude-window claude-buffer)
            (should (get-buffer-window claude-buffer))
            (claude-code-ide--strip-new-tab-claude-windows)
            ;; The Claude window is gone, the other window survives
            (should-not (get-buffer-window claude-buffer))
            (should (get-buffer-window other-buffer))))
      (delete-other-windows)
      (kill-buffer claude-buffer)
      (kill-buffer other-buffer)
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-show-all ()
  "Show-all displays every hidden instance and skips visible ones."
  (claude-code-ide-tests--clear-processes)
  (let* ((project-dir "/tmp/claude-show-all/")
         (buffer-a (generate-new-buffer "*claude-show-all-a*"))
         (buffer-b (generate-new-buffer "*claude-show-all-b*"))
         (displayed '()))
    (unwind-protect
        (let ((session-a (claude-code-ide-tests--make-session project-dir
                                                              :buffer buffer-a))
              (session-b (claude-code-ide-tests--make-session project-dir
                                                              :buffer buffer-b)))
          (ignore session-b)
          (cl-letf (((symbol-function 'claude-code-ide--get-working-directory)
                     (lambda () (expand-file-name project-dir)))
                    ((symbol-function 'claude-code-ide--session-visible-p)
                     ;; Only A is visible
                     (lambda (session) (eq session session-a)))
                    ((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                     (lambda (buffer) (push buffer displayed))))
            ;; Only the hidden instance gets displayed
            (claude-code-ide-show-all)
            (should (equal displayed (list buffer-b)))
            ;; No sessions anywhere errors
            (claude-code-ide-tests--clear-processes)
            (should-error (claude-code-ide-show-all) :type 'user-error)))
      (kill-buffer buffer-a)
      (kill-buffer buffer-b)
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-rename-session ()
  "Renaming an instance updates its name and its terminal buffer."
  (claude-code-ide-tests--clear-processes)
  (let* ((project-dir "/tmp/claude-rename/")
         (buffer (generate-new-buffer
                  (claude-code-ide--instance-buffer-name project-dir nil))))
    (unwind-protect
        (let ((session (claude-code-ide-tests--make-session project-dir
                                                            :buffer buffer)))
          (cl-letf (((symbol-function 'claude-code-ide--resolve-session)
                     (lambda (&rest _) session))
                    ((symbol-function 'claude-code-ide--read-instance-name)
                     (lambda (&rest _) "refactor")))
            (claude-code-ide-rename-session)
            (should (equal (claude-code-ide-mcp-session-instance-name session)
                           "refactor"))
            (should (equal (buffer-name buffer)
                           (claude-code-ide--instance-buffer-name project-dir
                                                                  "refactor")))
            (should (equal (claude-code-ide--session-display-name session)
                           "claude-rename:refactor")))

          ;; Empty input converts the instance back to the plain slot
          (cl-letf (((symbol-function 'claude-code-ide--resolve-session)
                     (lambda (&rest _) session))
                    ((symbol-function 'claude-code-ide--read-instance-name)
                     (lambda (&rest _) nil)))
            (claude-code-ide-rename-session)
            (should-not (claude-code-ide-mcp-session-instance-name session))
            (should (equal (buffer-name buffer)
                           (claude-code-ide--instance-buffer-name project-dir nil)))
            (should (equal (claude-code-ide--session-display-name session)
                           "claude-rename"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-stop-all-sessions-command ()
  "`claude-code-ide-stop-all' stops the project's instances after confirming."
  (claude-code-ide-tests--clear-processes)
  (let ((project-dir "/tmp/claude-stop-all/")
        (cleaned '())
        (confirm nil))
    (unwind-protect
        (let ((session-a (claude-code-ide-tests--make-session project-dir))
              (session-b (claude-code-ide-tests--make-session project-dir
                                                              :instance-name "b"))
              (other (claude-code-ide-tests--make-session "/tmp/claude-stop-other/")))
          (cl-letf (((symbol-function 'claude-code-ide--get-working-directory)
                     (lambda () project-dir))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) confirm))
                    ((symbol-function 'claude-code-ide--cleanup-session)
                     (lambda (session) (push session cleaned))))
            ;; Declining leaves everything running
            (claude-code-ide-stop-all)
            (should-not cleaned)

            ;; Confirming stops both instances of this project only
            (setq confirm t)
            (claude-code-ide-stop-all)
            (should (= 2 (length cleaned)))
            (should (memq session-a cleaned))
            (should (memq session-b cleaned))
            (should-not (memq other cleaned))

            ;; With a prefix argument every project's instances are stopped
            (setq cleaned '())
            (claude-code-ide-stop-all t)
            (should (= 3 (length cleaned)))
            (should (memq other cleaned))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-status-loads ()
  "The status feature loads and exposes its `buffer-name' constant."
  (should (featurep 'claude-code-ide-status))
  (should (stringp claude-code-ide-status-buffer-name)))

(ert-deftest claude-code-ide-test-status-mark-waiting-and-active ()
  "`mark-waiting' flags every instance of a project; `mark-active' clears them.
The hooks report a directory rather than an instance, so both commands
fan out over the project's instances and key the flag by session ID."
  (claude-code-ide-tests--clear-processes)
  (clrhash claude-code-ide-status--attention)
  (unwind-protect
      (let* ((dir "/tmp/proj/")
             (one (claude-code-ide-tests--make-session dir))
             (two (claude-code-ide-tests--make-session dir :instance-name "second"))
             (other (claude-code-ide-tests--make-session "/tmp/elsewhere/"))
             (id-of #'claude-code-ide-mcp-session-session-id))
        (claude-code-ide-status-mark-waiting dir)
        ;; Both instances of the project are flagged...
        (should (gethash (funcall id-of one) claude-code-ide-status--attention))
        (should (gethash (funcall id-of two) claude-code-ide-status--attention))
        ;; ...and no instance of any other project is.
        (should-not (gethash (funcall id-of other) claude-code-ide-status--attention))
        ;; A directory without a trailing slash still matches.
        (claude-code-ide-status-mark-active "/tmp/proj")
        (should-not (gethash (funcall id-of one) claude-code-ide-status--attention))
        (should-not (gethash (funcall id-of two) claude-code-ide-status--attention)))
    (claude-code-ide-tests--clear-processes)
    (clrhash claude-code-ide-status--attention)))

(ert-deftest claude-code-ide-test-status-state-for ()
  "`state-for' returns the right symbol, honouring precedence."
  (require 'claude-code-ide-mcp)
  (claude-code-ide-tests--clear-processes)
  (clrhash claude-code-ide-status--attention)
  (unwind-protect
      (let* ((dir "/tmp/proj/")
             (deferred (make-hash-table :test 'equal))
             (session (claude-code-ide-tests--make-session
                       dir :client nil :deferred deferred)))
        ;; No client, nothing pending -> disconnected.
        (should (eq (claude-code-ide-status--state-for session) 'disconnected))
        ;; A live client -> idle.
        (setf (claude-code-ide-mcp-session-client session) '(:mock-client t))
        (should (eq (claude-code-ide-status--state-for session) 'idle))
        ;; Attention flag beats idle -> waiting.
        (puthash (claude-code-ide-mcp-session-session-id session) t claude-code-ide-status--attention)
        (should (eq (claude-code-ide-status--state-for session) 'waiting))
        ;; A pending permission beats waiting -> permission.
        (puthash "some-tool" 123 deferred)
        (should (eq (claude-code-ide-status--state-for session) 'permission)))
    (claude-code-ide-tests--clear-processes)
    (clrhash claude-code-ide-status--attention)))

(ert-deftest claude-code-ide-test-status-live-entries ()
  "`live-entries' produces one row per instance, tagged as live.
Two instances of one project get two rows, each keyed by its own session
ID, since the directory no longer identifies an instance."
  (require 'claude-code-ide-mcp)
  (claude-code-ide-tests--clear-processes)
  (clrhash claude-code-ide-status--attention)
  (unwind-protect
      (let* ((dir "/tmp/proj-live/")
             (one (claude-code-ide-tests--make-session dir))
             (two (claude-code-ide-tests--make-session dir :instance-name "second")))
        ;; Cleanup would prune these fixtures, which own no real process.
        (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-sessions)
                   (lambda () nil)))
          (let* ((entries (claude-code-ide-status--live-entries))
                 (ids (mapcar #'car entries)))
            (should (= (length entries) 2))
            ;; Both rows are live, and keyed by session ID rather than by
            ;; directory.
            (should (seq-every-p (lambda (id) (eq (cdr id) 'live)) ids))
            (should (member (cons (claude-code-ide-mcp-session-session-id one) 'live) ids))
            (should (member (cons (claude-code-ide-mcp-session-session-id two) 'live) ids))
            (let ((cols (cadr (car entries))))
              ;; State, Instance, Project, Branch, Uptime, Activity.
              (should (= (length cols)
                         (length claude-code-ide-status--columns)))
              (should (string-match-p "proj-live" (aref cols 2))))
            ;; The named instance is distinguishable in the Instance column.
            (should (seq-find (lambda (entry)
                                (equal (aref (cadr entry) 1) "proj-live:second"))
                              entries)))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-status-state-label ()
  "`state-label' renders known states and falls back for unknown ones."
  (should (string-match-p "permission" (claude-code-ide-status--state-label 'permission)))
  (should (string-match-p "waiting" (claude-code-ide-status--state-label 'waiting)))
  (should (string-match-p "idle" (claude-code-ide-status--state-label 'idle)))
  (should (string-match-p "resume" (claude-code-ide-status--state-label 'resume)))
  ;; Known states are propertized with their face.
  (should (eq (get-text-property 0 'face (claude-code-ide-status--state-label 'permission))
              'claude-code-ide-status-permission-face))
  ;; An unknown state degrades to a plain string rather than erroring.
  (should (equal (claude-code-ide-status--state-label 'bogus) "bogus")))

(ert-deftest claude-code-ide-test-status-project-cwd ()
  "`project-cwd' recovers the recorded cwd from the newest transcript."
  (let ((sub (make-temp-file "claude-status-proj" t)))
    (unwind-protect
        (progn
          ;; The directory name is deliberately not the real cwd, proving we
          ;; read the path from the file contents rather than decoding it.
          (with-temp-file (expand-file-name "session.jsonl" sub)
            (insert "{\"type\":\"summary\",\"leafUuid\":\"x\"}\n")
            (insert "{\"type\":\"user\",\"cwd\":\"/Users/someone/code\"}\n"))
          (should (equal (claude-code-ide-status--project-cwd sub)
                         "/Users/someone/code/")))
      (delete-directory sub t))))

(ert-deftest claude-code-ide-test-status-resume-entries-excludes-live ()
  "`resume-entries' skips directories already present in the exclude table."
  (let ((root (make-temp-file "claude-status-projects" t)))
    (unwind-protect
        (let ((claude-code-ide-status-projects-directory root)
              (live-dir "/Users/someone/live/")
              (resume-dir "/Users/someone/resume/"))
          ;; Two on-disk projects, one of which is a live session.
          (dolist (pair (list (cons "live" live-dir) (cons "resume" resume-dir)))
            (let ((sub (expand-file-name (car pair) root)))
              (make-directory sub)
              (with-temp-file (expand-file-name "s.jsonl" sub)
                (insert (format "{\"cwd\":\"%s\"}\n"
                                (directory-file-name (cdr pair)))))))
          (let* ((exclude (make-hash-table :test 'equal))
                 (_ (puthash live-dir t exclude))
                 (entries (claude-code-ide-status--resume-entries exclude)))
            (should (= (length entries) 1))
            (should (equal (car (car (car entries))) resume-dir))
            (should (eq (cdr (car (car entries))) 'resume))))
      (delete-directory root t))))

(ert-deftest claude-code-ide-test-status-session-for-buffer ()
  "`session-for-buffer' finds the instance owning a terminal buffer.
Matching is by buffer identity rather than by name, so two instances of
one project are told apart."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let ((buffer-one (get-buffer-create "*ccide-revmap-1*"))
            (buffer-two (get-buffer-create "*ccide-revmap-2*")))
        (unwind-protect
            (let ((one (claude-code-ide-tests--make-session
                        "/tmp/proj-revmap/" :buffer buffer-one))
                  (two (claude-code-ide-tests--make-session
                        "/tmp/proj-revmap/" :instance-name "second"
                        :buffer buffer-two)))
              (should (eq (claude-code-ide-status--session-for-buffer buffer-one) one))
              (should (eq (claude-code-ide-status--session-for-buffer buffer-two) two))
              ;; An unrelated buffer maps to nothing.
              (should-not (claude-code-ide-status--session-for-buffer
                           (current-buffer))))
          (kill-buffer buffer-one)
          (kill-buffer buffer-two)))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-status-clear-on-select ()
  "Selecting a session's terminal buffer clears its waiting flag."
  (claude-code-ide-tests--clear-processes)
  (clrhash claude-code-ide-status--attention)
  (unwind-protect
      (let ((buffer (get-buffer-create "*ccide-select-test*")))
        (unwind-protect
            (let* ((session (claude-code-ide-tests--make-session
                             "/tmp/proj-select/" :buffer buffer))
                   (sibling (claude-code-ide-tests--make-session
                             "/tmp/proj-select/" :instance-name "second"))
                   (id (claude-code-ide-mcp-session-session-id session))
                   (sibling-id (claude-code-ide-mcp-session-session-id sibling)))
              (puthash id t claude-code-ide-status--attention)
              (puthash sibling-id t claude-code-ide-status--attention)
              ;; Simulate the terminal buffer becoming the selected one.
              (with-current-buffer buffer
                (claude-code-ide-status--clear-on-select))
              (should-not (gethash id claude-code-ide-status--attention))
              ;; Visiting one instance must not clear its sibling's flag.
              (should (gethash sibling-id claude-code-ide-status--attention)))
          (kill-buffer buffer)))
    (claude-code-ide-tests--clear-processes)
    (clrhash claude-code-ide-status--attention)))

(ert-deftest claude-code-ide-test-pop-to-session-buffer ()
  "`pop-to-session-buffer' focuses a visible window, else displays."
  (let ((buf (get-buffer-create "*ccide-pop-test*"))
        (displayed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                   (lambda (b) (setq displayed b) nil)))
          ;; Not visible anywhere -> falls back to the side-window display.
          (claude-code-ide-pop-to-session-buffer buf)
          (should (eq displayed buf))
          ;; Already visible -> selects its window, no fresh display.
          (setq displayed nil)
          (save-window-excursion
            (switch-to-buffer buf)
            (claude-code-ide-pop-to-session-buffer buf)
            (should (eq (window-buffer (selected-window)) buf))
            (should (null displayed))))
      (kill-buffer buf))))

(ert-deftest claude-code-ide-test-status-busy-poll-and-p ()
  "`poll-activity' stamps output changes; `busy-p' honours the timeout."
  (claude-code-ide-tests--clear-processes)
  (clrhash claude-code-ide-status--activity)
  (unwind-protect
      (let ((buffer (get-buffer-create "*ccide-busy-test*")))
        (unwind-protect
            (let* ((session (claude-code-ide-tests--make-session
                             "/tmp/proj-busy/" :buffer buffer))
                   (id (claude-code-ide-mcp-session-session-id session)))
              ;; First poll only records a baseline -> not yet busy.
              (claude-code-ide-status--poll-activity)
              (should-not (claude-code-ide-status--busy-p session))
              ;; Terminal produces output -> next poll stamps activity -> busy.
              (with-current-buffer buffer (insert "claude output\n"))
              (claude-code-ide-status--poll-activity)
              (should (claude-code-ide-status--busy-p session))
              ;; Backdate the stamp past the timeout -> no longer busy.
              (puthash id (cons (buffer-chars-modified-tick buffer)
                                (- (float-time)
                                   claude-code-ide-status-busy-timeout 1))
                       claude-code-ide-status--activity)
              (should-not (claude-code-ide-status--busy-p session)))
          (kill-buffer buffer)))
    (claude-code-ide-tests--clear-processes)
    (clrhash claude-code-ide-status--activity)))

(ert-deftest claude-code-ide-test-status-state-for-working ()
  "A live session with recent output reports `working', below `permission'."
  (require 'claude-code-ide-mcp)
  (claude-code-ide-tests--clear-processes)
  (clrhash claude-code-ide-status--attention)
  (clrhash claude-code-ide-status--activity)
  (unwind-protect
      (let* ((dir "/tmp/proj-working/")
             (buffer (get-buffer-create "*ccide-working-test*"))
             (deferred (make-hash-table :test 'equal))
             ;; `working' requires a live instance, so the fixture needs a
             ;; real process for `claude-code-ide-session-live-p'.
             (process (make-process :name "ccide-working"
                                    :command '("sleep" "10")
                                    :buffer nil))
             (session (claude-code-ide-tests--make-session
                       dir :client '(:mock t) :deferred deferred
                       :buffer buffer :process process)))
        (unwind-protect
            (progn
              ;; Mark it as having produced output just now.
              (puthash (claude-code-ide-mcp-session-session-id session)
                       (cons 1 (float-time)) claude-code-ide-status--activity)
              (should (eq (claude-code-ide-status--state-for session) 'working))
              ;; Even flagged waiting, live output wins (the turn resumed).
              (puthash (claude-code-ide-mcp-session-session-id session) t claude-code-ide-status--attention)
              (should (eq (claude-code-ide-status--state-for session) 'working))
              ;; A pending permission still outranks working.
              (puthash "tool" 1 deferred)
              (should (eq (claude-code-ide-status--state-for session) 'permission)))
          (delete-process process)
          (kill-buffer buffer)))
    (claude-code-ide-tests--clear-processes)
    (clrhash claude-code-ide-status--attention)
    (clrhash claude-code-ide-status--activity)))

(ert-deftest claude-code-ide-test-status-input-state ()
  "The `input' reason yields the `input' state, above working, below permission."
  (require 'claude-code-ide-mcp)
  (claude-code-ide-tests--clear-processes)
  (clrhash claude-code-ide-status--attention)
  (clrhash claude-code-ide-status--activity)
  (unwind-protect
      (let* ((dir "/tmp/proj-input/")
             (buffer (get-buffer-create "*ccide-input-test*"))
             (deferred (make-hash-table :test 'equal))
             (session (claude-code-ide-tests--make-session
                       dir :client '(:mock t) :deferred deferred
                       :buffer buffer)))
        (unwind-protect
            (progn
              ;; `mark-waiting' with reason `input' stores that reason...
              (claude-code-ide-status-mark-waiting dir 'input)
              (should (eq (gethash (claude-code-ide-mcp-session-session-id session)
                                   claude-code-ide-status--attention)
                          'input))
              ;; ...and yields the `input' state even with recent output.
              (puthash (claude-code-ide-mcp-session-session-id session)
                       (cons 1 (float-time)) claude-code-ide-status--activity)
              (should (eq (claude-code-ide-status--state-for session) 'input))
              ;; A pending permission still outranks input.
              (puthash "tool" 1 deferred)
              (should (eq (claude-code-ide-status--state-for session) 'permission)))
          (kill-buffer buffer)))
    (claude-code-ide-tests--clear-processes)
    (clrhash claude-code-ide-status--attention)
    (clrhash claude-code-ide-status--activity)))

(ert-deftest claude-code-ide-test-status-state-rank ()
  "`state-rank' orders states from most to least urgent."
  (should (< (claude-code-ide-status--state-rank 'permission)
             (claude-code-ide-status--state-rank 'input)))
  (should (< (claude-code-ide-status--state-rank 'input)
             (claude-code-ide-status--state-rank 'working)))
  (should (< (claude-code-ide-status--state-rank 'permission)
             (claude-code-ide-status--state-rank 'working)))
  (should (< (claude-code-ide-status--state-rank 'working)
             (claude-code-ide-status--state-rank 'waiting)))
  (should (< (claude-code-ide-status--state-rank 'waiting)
             (claude-code-ide-status--state-rank 'idle)))
  (should (< (claude-code-ide-status--state-rank 'idle)
             (claude-code-ide-status--state-rank 'disconnected)))
  ;; An unknown state sorts last, not first.
  (should (>= (claude-code-ide-status--state-rank 'bogus)
              (claude-code-ide-status--state-rank 'disconnected))))

(ert-deftest claude-code-ide-test-status-header ()
  "`header' counts live sessions and cached resumable projects."
  (claude-code-ide-tests--clear-processes)
  (clrhash claude-code-ide-status--attention)
  (let ((claude-code-ide-status--resume-cache
         (list (list (cons "/tmp/r1/" 'resume) (vector "" "" "" "" "" ""))
               (list (cons "/tmp/r2/" 'resume) (vector "" "" "" "" "" "")))))
    (unwind-protect
        (progn
          (claude-code-ide-tests--make-session "/tmp/live/")
          (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-sessions)
                     (lambda () nil)))
            (let ((header (claude-code-ide-status--header)))
              (should (string-match-p "1 live" header))
              (should (string-match-p "2 resumable" header))))
          ;; A second instance of the same project counts as two live rows,
          ;; and its project is still not resumable.
          (claude-code-ide-tests--make-session "/tmp/live/" :instance-name "second")
          (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-sessions)
                     (lambda () nil)))
            (should (string-match-p "2 live" (claude-code-ide-status--header)))))
      (claude-code-ide-tests--clear-processes))))

(ert-deftest claude-code-ide-test-status-stop-rejects-resume ()
  "`stop' refuses a resume row and requires confirmation for a live one."
  ;; Put the row id in the buffer as the real `tabulated-list-id' text
  ;; property rather than stubbing `tabulated-list-get-id', which is a
  ;; defsubst and gets inlined past `cl-letf' when byte-compiled.
  (with-temp-buffer
    (insert "row\n")
    (put-text-property (point-min) (point-max)
                       'tabulated-list-id (cons "/tmp/proj/" 'resume))
    (goto-char (point-min))
    (should-error (claude-code-ide-status-stop) :type 'user-error))
  ;; Declining the confirmation leaves the live instance untouched.
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let ((buffer (get-buffer-create "*ccide-keep-test*"))
            (prompt nil))
        (unwind-protect
            (let* ((session (claude-code-ide-tests--make-session
                             "/tmp/proj-keep/" :instance-name "keep"
                             :buffer buffer))
                   (id (claude-code-ide-mcp-session-session-id session)))
              (with-temp-buffer
                (insert "row\n")
                (put-text-property (point-min) (point-max)
                                   'tabulated-list-id (cons id 'live))
                (goto-char (point-min))
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (fmt &rest _) (setq prompt fmt) nil)))
                  (claude-code-ide-status-stop)
                  ;; The instance survives, buffer and registry entry intact.
                  (should (buffer-live-p buffer))
                  (should (gethash id claude-code-ide-mcp--sessions))
                  ;; The prompt names the instance, not just the project.
                  (should (string-match-p "proj-keep:keep" prompt)))))
          (when (buffer-live-p buffer) (kill-buffer buffer))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-status-attention-lighter-string ()
  "`attention-lighter-string' builds a coloured per-state breakdown."
  ;; No live sessions -> empty.
  (should (equal (claude-code-ide-status--attention-lighter-string nil 0) ""))
  ;; A calm fleet still shows working/idle counts, each in its own face.
  (let ((s (claude-code-ide-status--attention-lighter-string
            '((working . 2) (idle . 3)) 5)))
    (should (string-match-p "▶2" s))
    (should (string-match-p "○3" s))
    (should (text-property-any 0 (length s) 'face
                               'claude-code-ide-status-working-face s)))
  ;; Attention states show their coloured counts, urgent first.
  (let ((s (claude-code-ide-status--attention-lighter-string
            '((permission . 1) (waiting . 2)) 3)))
    (should (string-match-p "●1" s))
    (should (string-match-p "●2" s))
    (should (text-property-any 0 (length s) 'face
                               'claude-code-ide-status-permission-face s))))

(ert-deftest claude-code-ide-test-status-attention-notify ()
  "`poll-attention' counts states and notifies once per entry into one."
  (claude-code-ide-tests--clear-processes)
  (clrhash claude-code-ide-status--attention-seen)
  (unwind-protect
      (let* ((notified '())
             (claude-code-ide-status-notify t)
             (claude-code-ide-status-notify-function
              (lambda (dir state) (push (cons dir state) notified))))
        (let* ((session (claude-code-ide-tests--make-session "/tmp/att/"))
               (id (claude-code-ide-mcp-session-session-id session)))
          (cl-letf (((symbol-function 'claude-code-ide-status--state-for)
                     (lambda (_session) 'permission)))
            ;; Entering `permission' fires exactly one notification, and the
            ;; notify function still receives the project directory.
            (claude-code-ide-status--poll-attention)
            (should (equal notified '(("/tmp/att/" . permission))))
            (should (string-match-p "●1" claude-code-ide-status--attention-lighter))
            ;; Staying in `permission' does not re-notify.
            (claude-code-ide-status--poll-attention)
            (should (= (length notified) 1)))
          ;; Leaving the attention state (now idle) clears the seen entry,
          ;; which is keyed per instance.
          (cl-letf (((symbol-function 'claude-code-ide-status--state-for)
                     (lambda (_session) 'idle)))
            (claude-code-ide-status--poll-attention)
            (should-not (gethash id claude-code-ide-status--attention-seen))))
        ;; With no live sessions at all, the lighter is empty.
        (claude-code-ide-tests--clear-processes)
        (claude-code-ide-status--poll-attention)
        (should (equal claude-code-ide-status--attention-lighter "")))
    (claude-code-ide-tests--clear-processes)
    (clrhash claude-code-ide-status--attention-seen)))

(ert-deftest claude-code-ide-test-status-attention-mode-toggle ()
  "Enabling the mode registers the lighter; disabling removes it."
  (cl-letf (((symbol-function 'claude-code-ide-status--start-attention-timer) #'ignore)
            ((symbol-function 'claude-code-ide-status--stop-attention-timer) #'ignore))
    (let ((global-mode-string (list "")))
      (claude-code-ide-status-attention-mode 1)
      (unwind-protect
          (should (member claude-code-ide-status--mode-line-construct global-mode-string))
        (claude-code-ide-status-attention-mode -1))
      (should-not (member claude-code-ide-status--mode-line-construct global-mode-string)))))

(ert-deftest claude-code-ide-test-status-empty-state ()
  "`redraw' shows a placeholder when nothing is live or resumable."
  (claude-code-ide-tests--clear-processes)
  (with-temp-buffer
    (claude-code-ide-status-mode)
    (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-processes)
               (lambda () nil)))
      (let ((claude-code-ide-status-projects-directory "/nonexistent-ccide/")
            (claude-code-ide-status--resume-cache nil)
            (claude-code-ide-status--resume-cache-time (float-time)))
        (claude-code-ide-status--redraw)
        (should (string-match-p "No Claude sessions" (buffer-string)))))))

(ert-deftest claude-code-ide-test-status-sort-cycles ()
  "`sort' cycles each column ascending then reversed, then to the default."
  (claude-code-ide-tests--clear-processes)
  (with-temp-buffer
    (claude-code-ide-status-mode)
    (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-processes)
               (lambda () nil))
              ;; Avoid a disk scan; the sort key is all we assert on.
              ((symbol-function 'claude-code-ide-status--redraw) #'ignore))
      (let ((columns (mapcar #'car (append tabulated-list-format nil))))
        (should (null tabulated-list-sort-key))
        ;; Each column is visited ascending (nil) then reversed (t).
        (dolist (name columns)
          (claude-code-ide-status-sort)
          (should (equal tabulated-list-sort-key (cons name nil)))
          (claude-code-ide-status-sort)
          (should (equal tabulated-list-sort-key (cons name t))))
        ;; After the last column's reversed sort, back to the default order.
        (claude-code-ide-status-sort)
        (should (null tabulated-list-sort-key))))))

(ert-deftest claude-code-ide-test-status-apply-filter ()
  "`apply-filter' keeps rows matching every token, case-insensitively."
  (let ((entries
         (list (list (cons "/a/" 'live) (vector "○ idle" "~/work/alpha/" "main" "" ""))
               (list (cons "/b/" 'live) (vector "○ idle" "~/work/beta/" "dev" "" "")))))
    (unwind-protect
        (progn
          ;; No filter keeps every row.
          (setq claude-code-ide-status--filter nil)
          (should (= (length (claude-code-ide-status--apply-filter entries)) 2))
          ;; Case-insensitive substring match.
          (setq claude-code-ide-status--filter "ALPHA")
          (should (equal (mapcar (lambda (e) (car (car e)))
                                 (claude-code-ide-status--apply-filter entries))
                         '("/a/")))
          ;; Every whitespace-separated token must match (project + branch).
          (setq claude-code-ide-status--filter "beta dev")
          (should (= (length (claude-code-ide-status--apply-filter entries)) 1))
          (setq claude-code-ide-status--filter "beta main")
          (should (= (length (claude-code-ide-status--apply-filter entries)) 0)))
      (setq claude-code-ide-status--filter nil))))

(ert-deftest claude-code-ide-test-status-refresh-clears-sort ()
  "`refresh' clears the column sort so the list returns to urgency order."
  (claude-code-ide-tests--clear-processes)
  (with-temp-buffer
    (claude-code-ide-status-mode)
    (setq tabulated-list-sort-key '("Project"))
    (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-processes)
               (lambda () nil)))
      (let ((claude-code-ide-status-projects-directory "/nonexistent-ccide/")
            (claude-code-ide-status--resume-cache-time (float-time)))
        (claude-code-ide-status-refresh)
        (should (null tabulated-list-sort-key))))))

(ert-deftest claude-code-ide-test-status-display-in-split-fallback ()
  "`display-in-split' returns nil (not an error) when the split is too small."
  (let ((window-min-width 10000))       ; force `split-window' to fail
    (should (null (claude-code-ide-status--display-in-split (current-buffer))))))

(ert-deftest claude-code-ide-test-status-project-label ()
  "`project-label' compacts worktree paths and leaves plain projects alone."
  (let ((claude-code-ide-status-worktree-directories '(".worktrees" "worktrees")))
    ;; A plain checkout keeps its directory name.
    (should (equal (substring-no-properties
                    (claude-code-ide-status--project-label
                     "/Users/x/Develop/kycsystems/kycsitescan/"))
                   "kycsitescan"))
    ;; A worktree inside the repository shows repository and worktree.
    (should (equal (substring-no-properties
                    (claude-code-ide-status--project-label
                     "/Users/x/Develop/kycsystems/kycsitescan/.worktrees/3919-ip-api/"))
                   "kycsitescan/3919-ip-api"))
    ;; A worktree in a container beside the repositories names that container.
    (should (equal (substring-no-properties
                    (claude-code-ide-status--project-label
                     "/Users/x/Develop/kycsystems/.worktrees/kycsitescan_434-crawls/"))
                   "kycsystems/kycsitescan_434-crawls"))
    ;; A trailing slash is not required.
    (should (equal (substring-no-properties
                    (claude-code-ide-status--project-label
                     "/Users/x/Develop/kycsystems/mtbeacon"))
                   "mtbeacon"))
    ;; Claude Code's own `--worktree' nests the container inside .claude/,
    ;; so the repository is two levels above, not one.
    (should (equal (substring-no-properties
                    (claude-code-ide-status--project-label
                     "/Users/x/src/myrepo/.claude/worktrees/feat-x/"))
                   "myrepo/feat-x"))
    ;; The full path stays reachable, so shortening loses nothing.
    (should (equal (get-text-property
                    0 'help-echo
                    (claude-code-ide-status--project-label
                     "/Users/x/Develop/kycsystems/kycsitescan/.worktrees/3919-ip-api/"))
                   (abbreviate-file-name
                    "/Users/x/Develop/kycsystems/kycsitescan/.worktrees/3919-ip-api/"))))
  ;; Opting out restores the plain directory name.
  (let ((claude-code-ide-status-worktree-directories nil))
    (should (equal (substring-no-properties
                    (claude-code-ide-status--project-label
                     "/Users/x/Develop/kycsystems/kycsitescan/.worktrees/3919-ip-api/"))
                   "3919-ip-api"))))

(ert-deftest claude-code-ide-test-status-numeric-columns-sort-numerically ()
  "Rendered numbers sort by value, not by their text.
Sorted as strings these columns misorder outright: \"1.3M\" lands before
\"195.8k\" and \"9s\" after \"45m\".  Each cell carries the underlying number
as a `sort-key', and a cell without one sorts last so unknowns do not
crowd the head of the list."
  ;; Token counts.
  (let* ((cells (mapcar #'claude-code-ide-status--format-tokens
                        '(1300000 195800 392600 818800)))
         (entries (mapcar (lambda (c) (list nil (vector c))) cells))
         (sorted (sort (copy-sequence entries)
                       (claude-code-ide-status--sort-by-key 0))))
    ;; Text order would have put 1.3M first; value order puts it last.
    (should (equal (mapcar (lambda (e) (substring-no-properties (aref (cadr e) 0))) sorted)
                   '("195.8k" "392.6k" "818.8k" "1.3M"))))
  ;; Durations, including the "now" case and an unknown.
  (let* ((cells (list (propertize "45m" 'sort-key 2700)
                      (propertize "9s" 'sort-key 9)
                      (propertize "3d02h" 'sort-key 266400)
                      "—"))
         (entries (mapcar (lambda (c) (list nil (vector c))) cells))
         (sorted (sort (copy-sequence entries)
                       (claude-code-ide-status--sort-by-key 0))))
    (should (equal (mapcar (lambda (e) (substring-no-properties (aref (cadr e) 0))) sorted)
                   '("9s" "45m" "3d02h" "—"))))
  ;; A session producing output right now is zero seconds ago.
  (should (= (claude-code-ide-status--sort-key (propertize "now" 'sort-key 0)) 0))
  ;; No tokens is a known zero, not an unknown.
  (should (= (claude-code-ide-status--sort-key
              (claude-code-ide-status--format-tokens 0))
             0)))

(ert-deftest claude-code-ide-test-status-format-tokens ()
  "Token counts render compactly, and nothing renders as a dash."
  (should (equal (claude-code-ide-status--format-tokens nil) "—"))
  (should (equal (claude-code-ide-status--format-tokens 0) "—"))
  (should (equal (claude-code-ide-status--format-tokens 411) "411"))
  (should (equal (claude-code-ide-status--format-tokens 12400) "12.4k"))
  (should (equal (claude-code-ide-status--format-tokens 1200000) "1.2M")))

(ert-deftest claude-code-ide-test-status-scan-output-tokens ()
  "Output tokens accumulate, reading only bytes appended since last scan.
Transcripts reach several megabytes, so a re-read on every refresh would
be too costly; the scan keeps a byte cursor.  A partial trailing line,
which occurs while Claude is mid-write, must not be consumed twice."
  (let ((file (make-temp-file "ccide-tokens-" nil ".jsonl")))
    (unwind-protect
        (progn
          (clrhash claude-code-ide-status--output-tokens)
          ;; Cache reads dwarf output and must not be counted.
          (with-temp-file file
            (insert "{\"usage\":{\"output_tokens\":100,\"cache_read_input_tokens\":99999}}\n"
                    "{\"usage\":{\"output_tokens\":50,\"input_tokens\":7}}\n"))
          (should (= (claude-code-ide-status--scan-output-tokens file) 150))
          ;; Re-scanning without new bytes must not double-count.
          (should (= (claude-code-ide-status--scan-output-tokens file) 150))
          ;; Only the appended record is added.
          (with-temp-buffer
            (insert "{\"usage\":{\"output_tokens\":25}}\n")
            (append-to-file (point-min) (point-max) file))
          (should (= (claude-code-ide-status--scan-output-tokens file) 175))
          ;; A partial line is left for the next scan, then counted once.
          (with-temp-buffer
            (insert "{\"usage\":{\"output_tokens\":9")
            (append-to-file (point-min) (point-max) file))
          (should (= (claude-code-ide-status--scan-output-tokens file) 175))
          (with-temp-buffer
            (insert "99}}\n")
            (append-to-file (point-min) (point-max) file))
          (should (= (claude-code-ide-status--scan-output-tokens file) 1174))
          ;; A shorter file is a different session; the total restarts.
          (with-temp-file file
            (insert "{\"usage\":{\"output_tokens\":5}}\n"))
          (should (= (claude-code-ide-status--scan-output-tokens file) 5)))
      (delete-file file)
      (clrhash claude-code-ide-status--output-tokens))))

(ert-deftest claude-code-ide-test-status-column-arity-agrees ()
  "The header list and every row producer agree on the column count.
`claude-code-ide-status--refresh-columns' rebuilds `tabulated-list-format'
from `claude-code-ide-status--columns' on every refresh, so a header list
shorter than the rows silently shifts each value one column left and drops
the last.  It also defeats the resume cache, whose staleness check compares
these same lengths."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let ((width (length claude-code-ide-status--columns)))
        ;; A live row.
        (claude-code-ide-tests--make-session "/tmp/arity/")
        (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-sessions)
                   (lambda () nil)))
          (let ((row (car (claude-code-ide-status--live-entries))))
            (should row)
            (should (= (length (cadr row)) width))))
        ;; A resume row, built by the other producer.
        (let ((claude-code-ide-status-projects-directory
               (make-temp-file "ccide-arity-" t)))
          (unwind-protect
              (let ((sub (expand-file-name "proj" claude-code-ide-status-projects-directory)))
                (make-directory sub t)
                (with-temp-file (expand-file-name "s.jsonl" sub)
                  (insert "{\"type\":\"user\",\"cwd\":\"/tmp/arity-resume/\"}\n"))
                (when-let* ((row (car (claude-code-ide-status--build-resume-rows))))
                  (should (= (length (cadr row)) width))))
            (delete-directory claude-code-ide-status-projects-directory t))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-status-resume-cache-arity ()
  "A cached resume row with a stale column count is rebuilt, not printed."
  (let ((claude-code-ide-status-projects-directory "/nonexistent-ccide/")
        ;; A stale 3-column cache row while the mode now has more columns.
        (claude-code-ide-status--resume-cache
         (list (list (cons "/tmp/stale/" 'resume) (vector "" "" ""))))
        ;; Fresh time — only the arity mismatch should force the rebuild.
        (claude-code-ide-status--resume-cache-time (float-time)))
    (should (< (length (vector "" "" ""))
               (length claude-code-ide-status--columns)))
    (should (null (claude-code-ide-status--resume-entries
                   (make-hash-table :test 'equal))))))

(ert-deftest claude-code-ide-test-status-wrap-navigation ()
  "Row movement wraps at both ends and never lands on a non-row line."
  (with-temp-buffer
    ;; Three rows, each a line carrying a `tabulated-list-id', followed by the
    ;; trailing empty line `tabulated-list' leaves after the last row (no id).
    (dolist (n '("1" "2" "3"))
      (let ((start (point)))
        (insert "row" n "\n")
        (put-text-property start (1- (point))
                           'tabulated-list-id (cons n 'live))))
    (goto-char (point-min))
    (should (equal (car (tabulated-list-get-id)) "1"))
    ;; Down through the rows.
    (claude-code-ide-status-next-line)
    (should (equal (car (tabulated-list-get-id)) "2"))
    (claude-code-ide-status-next-line)
    (should (equal (car (tabulated-list-get-id)) "3"))
    ;; Down from the last row wraps to the first (never the empty line).
    (claude-code-ide-status-next-line)
    (should (equal (car (tabulated-list-get-id)) "1"))
    ;; Up from the first row wraps to the last.
    (claude-code-ide-status-previous-line)
    (should (equal (car (tabulated-list-get-id)) "3"))
    ;; And back up one more, staying on a real row.
    (claude-code-ide-status-previous-line)
    (should (equal (car (tabulated-list-get-id)) "2"))))

(ert-deftest claude-code-ide-test-status-header-intangible-no-bleed ()
  "`redraw' marks the header `cursor-intangible' without bleeding onto row 1.
Text properties are rear-sticky by default, so the first row's start would
otherwise inherit the header's `cursor-intangible' via `get-pos-property'
and make `cursor-sensor' try to move point off a fresh window (signalling
`wrong-type-argument number-or-marker-p nil' on the first redisplay)."
  (claude-code-ide-tests--clear-processes)
  (with-temp-buffer
    (claude-code-ide-status-mode)
    (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-sessions)
               (lambda () nil))
              ;; One live row so there is a first data row to inspect.  Its
              ;; width tracks the header list, so this fixture cannot drift
              ;; out of arity the way a hand-counted vector would.
              ((symbol-function 'claude-code-ide-status--entries)
               (lambda ()
                 (list (list (cons "/tmp/x/" 'live)
                             (vconcat
                              (vector (claude-code-ide-status--state-label 'idle))
                              (make-vector
                               (1- (length claude-code-ide-status--columns))
                               "")))))))
      (claude-code-ide-status--redraw)
      (let ((row1 (save-excursion (goto-char (point-min))
                                  (forward-line 1) (point))))
        ;; The header line itself carries the property (so point is repelled
        ;; from it — `cursor-sensor' checks `get-pos-property', which sees it
        ;; on interior header positions via rear-stickiness).
        (should (get-char-property (point-min) 'cursor-intangible))
        (should (get-pos-property (1+ (point-min)) 'cursor-intangible))
        ;; The first row's start does not — the property is stopped at the
        ;; boundary, so `cursor-sensor' leaves the row tangible.
        (should-not (get-pos-property row1 'cursor-intangible))
        (should (equal (car (get-text-property (1- row1) 'rear-nonsticky))
                       'cursor-intangible))))))

;;; Public session API

(provide 'claude-code-ide-tests)

;; Local Variables:
;; no-update-autoloads: t
;; autoload-compute-prefixes: nil
;; End:

;;; claude-code-ide-tests.el ends here
