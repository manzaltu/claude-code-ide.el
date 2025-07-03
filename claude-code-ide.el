;;; claude-code-ide.el --- Claude Code integration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "0.0.1") (websocket "1.12"))
;; Keywords: ai, claude, code, assistant, mcp, websocket
;; URL: https://github.com/manzaltu/claude-code-ide.el

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

;; Claude Code IDE integration for Emacs provides seamless integration
;; with Claude Code CLI through the Model Context Protocol (MCP).
;;
;; This package starts a WebSocket server that Claude Code CLI connects to,
;; enabling real-time communication between Emacs and Claude.  It supports
;; multiple concurrent sessions per project.
;;
;; Features:
;; - Automatic IDE mode activation when starting Claude
;; - MCP WebSocket server for bidirectional communication
;; - Project-aware sessions with automatic working directory detection
;; - Clean session management with automatic cleanup on exit
;; - Selection and buffer state tracking
;; - Tool support for file operations, diagnostics, and more
;;
;; Usage:
;; M-x claude-code-ide - Start Claude Code for current project
;; M-x claude-code-ide-resume - Resume Claude Code with previous conversation
;; M-x claude-code-ide-stop - Stop Claude Code for current project
;; M-x claude-code-ide-switch-to-buffer - Switch to project's Claude buffer
;; M-x claude-code-ide-list-sessions - List and switch between all sessions
;; M-x claude-code-ide-check-status - Check CLI availability and version
;; M-x claude-code-ide-insert-at-mentioned - Send selected text to Claude

;;; Code:

(require 'vterm)
(require 'cl-lib)
(require 'project)
(require 'claude-code-ide-mcp)
(require 'claude-code-ide-debug)

(declare-function claude-code-ide-mcp-stop-session "claude-code-ide-mcp" (project-dir))
(declare-function claude-code-ide-mcp--get-session-for-project "claude-code-ide-mcp" (project-dir))
(declare-function claude-code-ide-mcp-session-original-tab "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp--get-buffer-project "claude-code-ide-mcp" ())
(declare-function claude-code-ide-mcp-session-client "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-send-at-mentioned "claude-code-ide-mcp" ())

;;; Customization

(defgroup claude-code-ide nil
  "Claude Code integration for Emacs."
  :group 'tools
  :prefix "claude-code-ide-")

(defcustom claude-code-ide-cli-path "claude"
  "Path to the Claude Code CLI executable."
  :type 'string
  :group 'claude-code-ide)

(defcustom claude-code-ide-buffer-name-function #'claude-code-ide--default-buffer-name
  "Function to generate buffer names for Claude Code sessions.
The function is called with one argument, the working directory,
and should return a string to use as the buffer name."
  :type 'function
  :group 'claude-code-ide)

(defcustom claude-code-ide-cli-debug nil
  "When non-nil, launch Claude Code with the -d debug flag."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-side 'right
  "Side of the frame where the Claude Code window should appear.
Can be `'left', `'right', `'top', or `'bottom'."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-width 90
  "Width of the Claude Code side window when opened on left or right."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-height 20
  "Height of the Claude Code side window when opened on top or bottom."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-focus-on-open t
  "Whether to focus the Claude Code window when it opens."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-focus-claude-after-ediff t
  "Whether to focus the Claude Code window after opening ediff.
When non-nil (default), focus returns to the Claude Code window
after opening ediff.  When nil, focus remains on the ediff control
window, allowing direct interaction with the diff controls."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-use-side-window t
  "Whether to display Claude Code in a side window.
When non-nil (default), Claude Code opens in a dedicated side window
controlled by `claude-code-ide-window-side' and related settings.
When nil, Claude Code opens in a regular buffer that follows standard
display-buffer behavior."
  :type 'boolean
  :group 'claude-code-ide)

;;; Constants

(defconst claude-code-ide--active-editor-notification-delay 0.1
  "Delay in seconds before sending active editor notification after connection.")

;;; Variables

(defvar claude-code-ide--cli-available nil
  "Whether Claude Code CLI is available and detected.")

(defvar claude-code-ide--processes (make-hash-table :test 'equal)
  "Hash table mapping project/directory roots to their Claude Code processes.")

;;; Helper Functions

(defun claude-code-ide--default-buffer-name (directory)
  "Generate default buffer name for DIRECTORY."
  (format "*claude-code[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

(defun claude-code-ide--get-working-directory ()
  "Get the current working directory (project root or current directory)."
  (if-let ((project (project-current)))
      (expand-file-name (project-root project))
    (expand-file-name default-directory)))

(defun claude-code-ide--get-buffer-name (&optional directory)
  "Get the buffer name for the Claude Code session in DIRECTORY.
If DIRECTORY is not provided, use the current working directory."
  (funcall claude-code-ide-buffer-name-function
           (or directory (claude-code-ide--get-working-directory))))

(defun claude-code-ide--get-process (&optional directory)
  "Get the Claude Code process for DIRECTORY or current working directory."
  (gethash (or directory (claude-code-ide--get-working-directory))
           claude-code-ide--processes))

(defun claude-code-ide--set-process (process &optional directory)
  "Set the Claude Code PROCESS for DIRECTORY or current working directory."
  (puthash (or directory (claude-code-ide--get-working-directory))
           process
           claude-code-ide--processes))

(defun claude-code-ide--cleanup-dead-processes ()
  "Remove entries for dead processes from the process table."
  (maphash (lambda (directory process)
             (unless (process-live-p process)
               (remhash directory claude-code-ide--processes)))
           claude-code-ide--processes))

(defun claude-code-ide--cleanup-all-sessions ()
  "Clean up all active Claude Code sessions."
  (maphash (lambda (directory process)
             (when (process-live-p process)
               (claude-code-ide--cleanup-on-exit directory)))
           claude-code-ide--processes))

;; Ensure cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-code-ide--cleanup-all-sessions)

(defun claude-code-ide--display-buffer-in-side-window (buffer)
  "Display BUFFER in a side window according to customization.
The window is displayed on the side specified by
`claude-code-ide-window-side' with dimensions from
`claude-code-ide-window-width' or `claude-code-ide-window-height'.
If `claude-code-ide-focus-on-open' is non-nil, the window is selected."
  (let ((window
         (if claude-code-ide-use-side-window
             ;; Use side window
             (let* ((side claude-code-ide-window-side)
                    (slot 0)
                    (window-parameters '((no-delete-other-windows . t)))
                    (display-buffer-alist
                     `((,(regexp-quote (buffer-name buffer))
                        (display-buffer-in-side-window)
                        (side . ,side)
                        (slot . ,slot)
                        (window-width . ,(if (memq side '(left right))
                                             claude-code-ide-window-width
                                           'fit-window-to-buffer))
                        (window-height . ,(if (memq side '(top bottom))
                                              claude-code-ide-window-height
                                            'fit-window-to-buffer))
                        (window-parameters . ,window-parameters)))))
               (display-buffer buffer))
           ;; Use regular buffer
           (display-buffer buffer))))
    ;; Select the window to give it focus if configured to do so
    (when (and window claude-code-ide-focus-on-open)
      (select-window window))
    window))

(defvar claude-code-ide--cleanup-in-progress nil
  "Flag to prevent recursive cleanup calls.")

(defun claude-code-ide--cleanup-on-exit (directory)
  "Clean up MCP server and process tracking when Claude exits for DIRECTORY."
  (unless claude-code-ide--cleanup-in-progress
    (setq claude-code-ide--cleanup-in-progress t)
    (unwind-protect
        (progn
          ;; Remove from process table
          (remhash directory claude-code-ide--processes)
          ;; Stop MCP server for this project directory
          (claude-code-ide-mcp-stop-session directory)
          ;; Kill the vterm buffer if it exists
          (let ((buffer-name (claude-code-ide--get-buffer-name directory)))
            (when-let ((buffer (get-buffer buffer-name)))
              (when (buffer-live-p buffer)
                (let ((kill-buffer-hook nil) ; Disable hooks to prevent recursion
                      (kill-buffer-query-functions nil)) ; Don't ask for confirmation
                  (kill-buffer buffer)))))
          (claude-code-ide-debug "Cleaned up Claude Code session for %s"
                                 (file-name-nondirectory (directory-file-name directory))))
      (setq claude-code-ide--cleanup-in-progress nil))))

;;; CLI Detection

(defun claude-code-ide--detect-cli ()
  "Detect if Claude Code CLI is available."
  (let ((available (condition-case nil
                       (eq (call-process claude-code-ide-cli-path nil nil nil "--version") 0)
                     (error nil))))
    (setq claude-code-ide--cli-available available)))

(defun claude-code-ide--ensure-cli ()
  "Ensure Claude Code CLI is available, detect if needed."
  (unless claude-code-ide--cli-available
    (claude-code-ide--detect-cli))
  claude-code-ide--cli-available)

;;; Commands

(defun claude-code-ide--toggle-existing-window (existing-buffer working-dir)
  "Toggle visibility of EXISTING-BUFFER window for WORKING-DIR.
If the window is visible, it will be hidden.
If the window is not visible, it will be shown in a side window."
  (let ((window (get-buffer-window existing-buffer)))
    (if window
        ;; Window is visible, hide it
        (progn
          (delete-window window)
          (claude-code-ide-debug "Claude Code window hidden"))
      ;; Window is not visible, show it
      (progn
        (claude-code-ide--display-buffer-in-side-window existing-buffer)
        ;; Update the original tab when showing the window
        (when-let ((session (claude-code-ide-mcp--get-session-for-project working-dir)))
          (when (fboundp 'tab-bar--current-tab)
            (setf (claude-code-ide-mcp-session-original-tab session) (tab-bar--current-tab))))
        (claude-code-ide-debug "Claude Code window shown")))))

(defun claude-code-ide--build-claude-command (&optional resume)
  "Build the Claude command with optional flags.
If RESUME is non-nil, add the -r flag.
If `claude-code-ide-cli-debug' is non-nil, add the -d flag."
  (let ((claude-cmd claude-code-ide-cli-path))
    ;; Add debug flag if enabled
    (when claude-code-ide-cli-debug
      (setq claude-cmd (concat claude-cmd " -d")))
    ;; Add resume flag if requested
    (when resume
      (setq claude-cmd (concat claude-cmd " -r")))
    claude-cmd))

(defun claude-code-ide--create-vterm-session (buffer-name working-dir port resume)
  "Create a new vterm session for Claude Code.
BUFFER-NAME is the name for the vterm buffer.
WORKING-DIR is the working directory.
PORT is the MCP server port.
RESUME is whether to resume a previous conversation.

Returns a cons cell of (buffer . process) on success.
Signals an error if vterm fails to initialize."
  (let* ((claude-cmd (claude-code-ide--build-claude-command resume))
         (vterm-buffer-name buffer-name)
         (default-directory working-dir)
         ;; Set vterm-shell to run Claude directly instead of a shell
         (vterm-shell claude-cmd)
         ;; vterm uses vterm-environment for passing env vars
         (vterm-environment (append
                             (list (format "CLAUDE_CODE_SSE_PORT=%d" port)
                                   "ENABLE_IDE_INTEGRATION=true"
                                   "TERM_PROGRAM=emacs"
                                   "FORCE_CODE_TERMINAL=true")
                             vterm-environment)))
    ;; Create vterm buffer without switching to it
    (let ((buffer (save-window-excursion
                    (vterm vterm-buffer-name))))
      ;; Check if vterm successfully created a buffer
      (unless buffer
        (error "Failed to create vterm buffer.  Please ensure vterm is properly installed"))
      ;; Get the process that vterm created
      (let ((process (get-buffer-process buffer)))
        (unless process
          (error "Failed to get vterm process.  The vterm buffer may not have initialized properly"))
        ;; Check if buffer is still alive
        (unless (buffer-live-p buffer)
          (error "Vterm buffer was killed during initialization"))
        (cons buffer process)))))

(defun claude-code-ide--start-session (&optional resume)
  "Start a Claude Code session for the current project.
If RESUME is non-nil, start Claude with the -r (resume) flag.

This function handles:
- CLI availability checking
- Dead process cleanup
- Existing session detection and window toggling
- New session creation with MCP server setup
- Process and buffer lifecycle management"
  (unless (claude-code-ide--ensure-cli)
    (user-error "Claude Code CLI not available.  Please install it and ensure it's in PATH"))

  (unless (fboundp 'vterm)
    (user-error "The package vterm is not installed.  Please install the vterm package"))

  ;; Try to require vterm to ensure it's loaded
  (require 'vterm nil t)

  ;; Clean up any dead processes first
  (claude-code-ide--cleanup-dead-processes)

  (let* ((working-dir (claude-code-ide--get-working-directory))
         (buffer-name (claude-code-ide--get-buffer-name))
         (existing-buffer (get-buffer buffer-name))
         (existing-process (claude-code-ide--get-process working-dir)))

    ;; If buffer exists and process is alive, toggle the window
    (if (and existing-buffer
             (buffer-live-p existing-buffer)
             existing-process)
        (claude-code-ide--toggle-existing-window existing-buffer working-dir)
      ;; Start MCP server with project directory
      (let ((port (claude-code-ide-mcp-start working-dir)))
        ;; Create new vterm session
        (let* ((buffer-and-process (claude-code-ide--create-vterm-session
                                    buffer-name working-dir port resume))
               (buffer (car buffer-and-process))
               (process (cdr buffer-and-process)))
          (claude-code-ide--set-process process working-dir)
          ;; Set up process sentinel to clean up when Claude exits
          (set-process-sentinel process
                                (lambda (_proc event)
                                  (when (or (string-match "finished" event)
                                            (string-match "exited" event)
                                            (string-match "killed" event)
                                            (string-match "terminated" event))
                                    (claude-code-ide--cleanup-on-exit working-dir))))
          ;; Also add buffer kill hook as a backup
          (with-current-buffer buffer
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (claude-code-ide--cleanup-on-exit working-dir))
                      nil t)
            ;; Add vterm exit hook to ensure buffer is killed when process exits
            ;; vterm runs Claude directly, no shell involved
            (add-hook 'vterm-exit-functions
                      (lambda (&rest _)
                        (when (buffer-live-p buffer)
                          (kill-buffer buffer)))
                      nil t))
          ;; Display the buffer in a side window
          (claude-code-ide--display-buffer-in-side-window buffer)
          (claude-code-ide-log "Claude Code %sstarted in %s with MCP on port %d%s"
                               (if resume "resumed and " "")
                               (file-name-nondirectory (directory-file-name working-dir))
                               port
                               (if claude-code-ide-cli-debug " (debug mode enabled)" "")))))))

;;;###autoload
(defun claude-code-ide ()
  "Run Claude Code in a terminal for the current project or directory."
  (interactive)
  (claude-code-ide--start-session))

;;;###autoload
(defun claude-code-ide-resume ()
  "Resume Claude Code in a terminal for the current project or directory.
This starts Claude with the -r (resume) flag to continue the previous
conversation."
  (interactive)
  (claude-code-ide--start-session t))

;;;###autoload
(defun claude-code-ide-check-status ()
  "Check Claude Code CLI status."
  (interactive)
  (claude-code-ide--detect-cli)
  (if claude-code-ide--cli-available
      (let ((version-output
             (with-temp-buffer
               (call-process claude-code-ide-cli-path nil t nil "--version")
               (buffer-string))))
        (claude-code-ide-log "Claude Code CLI version: %s" (string-trim version-output)))
    (claude-code-ide-log "Claude Code is not installed.")))

;;;###autoload
(defun claude-code-ide-stop ()
  "Stop the Claude Code session for the current project or directory."
  (interactive)
  (let* ((working-dir (claude-code-ide--get-working-directory))
         (buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (progn
          ;; Kill the buffer (cleanup will be handled by hooks)
          ;; The process sentinel will handle cleanup when the process dies
          (kill-buffer buffer)
          (claude-code-ide-log "Stopping Claude Code in %s..."
                               (file-name-nondirectory (directory-file-name working-dir))))
      (claude-code-ide-log "No Claude Code session is running in this directory"))))


;;;###autoload
(defun claude-code-ide-switch-to-buffer ()
  "Switch to the Claude Code buffer for the current project.
If the buffer is not visible, display it in the configured side window.
If the buffer is already visible, switch focus to it."
  (interactive)
  (let ((buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (if-let ((window (get-buffer-window buffer)))
            ;; Buffer is visible, just focus it
            (select-window window)
          ;; Buffer exists but not visible, display it
          (claude-code-ide--display-buffer-in-side-window buffer))
      (user-error "No Claude Code session for this project.  Use M-x claude-code-ide to start one"))))

;;;###autoload
(defun claude-code-ide-list-sessions ()
  "List all active Claude Code sessions and switch to selected one."
  (interactive)
  (claude-code-ide--cleanup-dead-processes)
  (let ((sessions '()))
    (maphash (lambda (directory _)
               (push (cons (abbreviate-file-name directory)
                           directory)
                     sessions))
             claude-code-ide--processes)
    (if sessions
        (let ((choice (completing-read "Switch to Claude Code session: "
                                       sessions nil t)))
          (when choice
            (let* ((directory (alist-get choice sessions nil nil #'string=))
                   (buffer-name (funcall claude-code-ide-buffer-name-function directory)))
              (if-let ((buffer (get-buffer buffer-name)))
                  (claude-code-ide--display-buffer-in-side-window buffer)
                (user-error "Buffer for session %s no longer exists" choice)))))
      (claude-code-ide-log "No active Claude Code sessions"))))

;;;###autoload
(defun claude-code-ide-insert-at-mentioned ()
  "Insert selected text into Claude prompt."
  (interactive)
  (if-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
            (session (claude-code-ide-mcp--get-session-for-project project-dir))
            (client (claude-code-ide-mcp-session-client session)))
      (progn
        (claude-code-ide-mcp-send-at-mentioned)
        (claude-code-ide-debug "Sent selection to Claude Code"))
    (user-error "Claude Code is not connected.  Please start Claude Code first")))

;;;###autoload
(defun claude-code-ide-send-escape ()
  "Send escape key to the Claude Code vterm buffer for the current project."
  (interactive)
  (let ((buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (vterm-send-escape))
      (user-error "No Claude Code session for this project"))))

;;;###autoload
(defun claude-code-ide-insert-newline ()
  "Send newline (backslash + return) to the Claude Code vterm buffer for the current project.
This simulates typing backslash followed by Enter, which Claude Code interprets as a newline."
  (interactive)
  (let ((buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (vterm-send-string "\\")
          (vterm-send-return))
      (user-error "No Claude Code session for this project"))))

(provide 'claude-code-ide)

;;; claude-code-ide.el ends here
