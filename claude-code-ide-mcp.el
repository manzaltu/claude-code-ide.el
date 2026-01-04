;;; claude-code-ide-mcp.el --- MCP server for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Keywords: ai, claude, mcp

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

;; This file implements an MCP (Model Context Protocol) server for Claude Code IDE.
;; It provides a WebSocket server that Claude CLI can connect to, handling JSON-RPC
;; messages and exposing Emacs functionality through MCP tools.

;;; Code:

;; Declare external functions for byte-compilation
(declare-function websocket-server "websocket" (port &rest plist))
(declare-function websocket-server-close "websocket" (server))
(declare-function websocket-server-filter "websocket" (proc string))
(declare-function websocket-send-text "websocket" (ws text))
(declare-function websocket-send "websocket" (ws frame))
(declare-function websocket-ready-state "websocket" (websocket))
(declare-function websocket-url "websocket" (websocket))
(declare-function websocket-frame-text "websocket" (frame))
(declare-function websocket-frame-opcode "websocket" (frame))
(declare-function make-websocket-frame "websocket" (&rest args))

;; Require websocket at runtime to avoid batch mode issues
(unless (featurep 'websocket)
  (condition-case err
      (require 'websocket)
    (error
     (claude-code-ide-debug "Failed to load websocket package: %s" (error-message-string err)))))
(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-parse)
(require 'claude-code-ide-debug)
(require 'claude-code-ide-mcp-handlers)
(require 'claude-code-ide-mcp-server)

;; External declarations
(defvar claude-code-ide--session-ids)
(declare-function claude-code-ide-mcp--build-tool-list "claude-code-ide-mcp-handlers" ())
(declare-function claude-code-ide-mcp--build-tool-schemas "claude-code-ide-mcp-handlers" ())
(declare-function claude-code-ide-mcp--build-tool-descriptions "claude-code-ide-mcp-handlers" ())

;;; Constants

(defconst claude-code-ide-mcp-version "2024-11-05"
  "MCP protocol version.")

(defconst claude-code-ide-mcp-port-range '(10000 . 65535)
  "Port range for WebSocket server.")

(defconst claude-code-ide-mcp-max-port-attempts 100
  "Maximum number of attempts to find a free port.")

(defconst claude-code-ide-mcp-ping-interval 30
  "Interval in seconds between ping messages to keep connection alive.")

(defconst claude-code-ide-mcp-selection-delay 0.05
  "Delay in seconds before sending selection changes to avoid flooding.")

(defconst claude-code-ide-mcp-initial-notification-delay 0.1
  "Delay in seconds before sending initial notifications after connection.")

;;; Variables

;; Only keep the global sessions table
(defvar claude-code-ide-mcp--sessions (make-hash-table :test 'equal)
  "Hash table mapping project directories to MCP sessions.")

;; Buffer-local cache variables for performance optimization
(defvar-local claude-code-ide-mcp--buffer-project-cache nil
  "Cached project directory for the current buffer.
This avoids repeated project lookups on every cursor movement.")

(defvar-local claude-code-ide-mcp--buffer-session-cache nil
  "Cached MCP session for the current buffer.
This avoids repeated session lookups on every cursor movement.")

(defvar-local claude-code-ide-mcp--buffer-cache-valid nil
  "Whether the buffer-local cache is valid.
Set to nil when cache needs to be invalidated.")

;;; Error Definition

(define-error 'mcp-error "MCP Error" 'error)

;;; Session Management

(cl-defstruct claude-code-ide-mcp-session
  "Structure to hold all state for a single MCP session."
  server           ; WebSocket server instance
  client           ; Connected WebSocket client
  port             ; Server port
  project-dir      ; Project directory
  deferred         ; Hash table of deferred responses
  ping-timer       ; Ping timer
  selection-timer  ; Selection tracking timer
  last-selection   ; Last selection state
  last-buffer      ; Last active buffer
  active-diffs     ; Hash table of active diffs
  original-tab)    ; Original tab-bar tab where Claude was opened

(defun claude-code-ide-mcp--get-buffer-project ()
  "Get the project directory for the current buffer.
Returns the expanded project root path if a project is found,
otherwise returns nil.
Uses buffer-local cache to avoid repeated project lookups."
  ;; Check if we have a valid cache
  (if claude-code-ide-mcp--buffer-cache-valid
      ;; Cache is valid, return cached value (even if nil)
      claude-code-ide-mcp--buffer-project-cache
    ;; Cache is invalid or doesn't exist, recalculate
    (let ((project-dir (when-let* ((project (project-current)))
                         (expand-file-name (project-root project)))))
      ;; Update cache
      (setq claude-code-ide-mcp--buffer-project-cache project-dir
            claude-code-ide-mcp--buffer-cache-valid t)
      project-dir)))

(defun claude-code-ide-mcp--get-session-for-project (project-dir)
  "Get the MCP session for PROJECT-DIR.
Returns the session structure if found, nil otherwise."
  (when project-dir
    (gethash project-dir claude-code-ide-mcp--sessions)))

(defun claude-code-ide-mcp--get-current-session ()
  "Get the MCP session for the current buffer's project.
This is a convenience function that combines
`claude-code-ide-mcp--get-buffer-project' and
`claude-code-ide-mcp--get-session-for-project'."
  (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project)))
    (claude-code-ide-mcp--get-session-for-project project-dir)))

(defun claude-code-ide-mcp--find-session-by-websocket (ws)
  "Find the MCP session that owns the WebSocket WS.
Searches through all active sessions to find the one with matching client.
Returns the session if found, nil otherwise."
  (let ((found-session nil))
    (maphash (lambda (_project-dir session)
               (when (eq (claude-code-ide-mcp-session-client session) ws)
                 (setq found-session session)))
             claude-code-ide-mcp--sessions)
    found-session))

(defun claude-code-ide-mcp--active-sessions ()
  "Return a list of all active MCP sessions."
  (let ((sessions '()))
    (maphash (lambda (_project-dir session)
               (push session sessions))
             claude-code-ide-mcp--sessions)
    sessions))

;;; Backward Compatibility Layer

;;; Lockfile Management

(defun claude-code-ide-mcp--lockfile-directory ()
  "Return the directory for MCP lockfiles."
  (expand-file-name "~/.claude/ide/"))

(defun claude-code-ide-mcp--lockfile-path (port)
  "Return the lockfile path for PORT."
  (format "%s%d.lock" (claude-code-ide-mcp--lockfile-directory) port))

(defun claude-code-ide-mcp--create-lockfile (port project-dir)
  "Create a lockfile for PORT with server information for PROJECT-DIR."
  (let* ((lockfile-dir (claude-code-ide-mcp--lockfile-directory))
         (lockfile-path (claude-code-ide-mcp--lockfile-path port))
         (workspace-folders (vector project-dir))
         (lockfile-content `((pid . ,(emacs-pid))
                             (workspaceFolders . ,workspace-folders)
                             (ideName . "Emacs")
                             (transport . "ws"))))
    ;; Ensure directory exists
    (make-directory lockfile-dir t)
    ;; Write lockfile directly without temp file
    (condition-case err
        (with-temp-file lockfile-path
          (insert (json-encode lockfile-content)))
      (error
       (claude-code-ide-debug "Failed to create lockfile: %s" err)
       (signal 'mcp-error (list (format "Failed to create lockfile: %s" (error-message-string err))))))))

(defun claude-code-ide-mcp--remove-lockfile (port)
  "Remove the lockfile for PORT."
  (when port
    (let ((lockfile-path (claude-code-ide-mcp--lockfile-path port)))
      (claude-code-ide-debug "Attempting to remove lockfile: %s" lockfile-path)
      (if (file-exists-p lockfile-path)
          (progn
            (delete-file lockfile-path)
            (claude-code-ide-debug "Lockfile deleted: %s" lockfile-path))
        (claude-code-ide-debug "Lockfile not found: %s" lockfile-path)))))


;;; JSON-RPC Message Handling

(defun claude-code-ide-mcp--make-response (id result)
  "Create a JSON-RPC response with ID and RESULT."
  `((jsonrpc . "2.0")
    (id . ,id)
    (result . ,result)))

(defun claude-code-ide-mcp--make-error-response (id code message &optional data)
  "Create a JSON-RPC error response with ID, CODE, MESSAGE and optional DATA."
  `((jsonrpc . "2.0")
    (id . ,id)
    (error . ((code . ,code)
              (message . ,message)
              ,@(when data `((data . ,data)))))))

(defun claude-code-ide-mcp--send-notification (method params)
  "Send a JSON-RPC notification with METHOD and PARAMS to the current session."
  ;; Try to use cached session first
  (when-let* ((session (or (when (and claude-code-ide-mcp--buffer-cache-valid
                                      claude-code-ide-mcp--buffer-session-cache)
                             claude-code-ide-mcp--buffer-session-cache)
                           (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
                                       (found-session (claude-code-ide-mcp--get-session-for-project project-dir)))
                             ;; Cache the session (only cache non-nil values)
                             (when found-session
                               (setq claude-code-ide-mcp--buffer-session-cache found-session
                                     claude-code-ide-mcp--buffer-cache-valid t))
                             found-session)))
              (client (claude-code-ide-mcp-session-client session)))
    (let ((message `((jsonrpc . "2.0")
                     (method . ,method)
                     (params . ,params))))
      (claude-code-ide-debug "Sending notification: %s" (json-encode message))
      (condition-case err
          (progn
            (websocket-send-text client (json-encode message))
            (claude-code-ide-debug "Sent %s notification" method))
        (error
         (claude-code-ide-debug "Failed to send notification %s: %s" method err))))))

(defun claude-code-ide-mcp--handle-initialize (id _params)
  "Handle the initialize request with ID."
  (claude-code-ide-debug "Handling initialize request with id: %s" id)
  ;; Start ping timer after successful initialization
  ;; DISABLED: Ping causing connection issues - needs investigation
  ;; (claude-code-ide-mcp--start-ping-timer)

  ;; Send tools/list_changed notification after initialization
  (claude-code-ide-debug "Scheduling tools/list_changed notification")
  (run-with-timer claude-code-ide-mcp-initial-notification-delay nil
                  (lambda ()
                    (claude-code-ide-debug "Sending tools/list_changed notification")
                    (claude-code-ide-mcp--send-notification
                     "notifications/tools/list_changed"
                     (make-hash-table :test 'equal))))

  (let ((response `((protocolVersion . ,claude-code-ide-mcp-version)
                    (capabilities . ((tools . ((listChanged . t)))
                                     (resources . ((subscribe . :json-false)
                                                   (listChanged . :json-false)))
                                     (prompts . ((listChanged . t)))
                                     (logging . ,(make-hash-table :test 'equal))))
                    (serverInfo . ((name . "claude-code-ide-mcp")
                                   (version . "0.1.0"))))))
    (claude-code-ide-debug "Initialize response capabilities: tools.listChanged=%s, resources.subscribe=%s, resources.listChanged=%s, prompts.listChanged=%s"
                           t :json-false :json-false t)
    (claude-code-ide-mcp--make-response id response)))

(defun claude-code-ide-mcp--prepare-schema-for-json (schema)
  "Prepare SCHEMA for JSON encoding.
Converts :json-empty to empty hash tables which json-encode will
turn into {}. Recursively processes nested structures."
  (cond
   ;; If it's :json-empty, return an empty alist which json-encode will convert to {}
   ((eq schema :json-empty)
    (make-hash-table :test 'equal))
   ;; If it's a list, recursively process each element
   ((listp schema)
    (mapcar (lambda (item)
              (if (consp item)
                  (cons (car item) (claude-code-ide-mcp--prepare-schema-for-json (cdr item)))
                item))
            schema))
   ;; Otherwise return as-is
   (t schema)))

(defun claude-code-ide-mcp--handle-tools-list (id _params)
  "Handle the tools/list request with ID."
  (claude-code-ide-debug "Handling tools/list request with id: %s" id)
  ;; Rebuild tool lists to respect current settings
  (setq claude-code-ide-mcp-tools (claude-code-ide-mcp--build-tool-list))
  (setq claude-code-ide-mcp-tool-schemas (claude-code-ide-mcp--build-tool-schemas))
  (setq claude-code-ide-mcp-tool-descriptions (claude-code-ide-mcp--build-tool-descriptions))
  ;; Ensure handlers are loaded
  (claude-code-ide-debug "Building tools list from %d registered tools"
                         (length claude-code-ide-mcp-tools))
  (let ((tools '()))
    (dolist (tool-entry claude-code-ide-mcp-tools)
      (let* ((name (car tool-entry))
             (schema (alist-get name claude-code-ide-mcp-tool-schemas nil nil #'string=))
             (prepared-schema (claude-code-ide-mcp--prepare-schema-for-json schema))
             (description (alist-get name claude-code-ide-mcp-tool-descriptions nil nil #'string=)))
        (claude-code-ide-debug "  Tool: %s (has schema: %s, has description: %s)"
                               name
                               (if schema "yes" "no")
                               (if description "yes" "no"))
        (push `((name . ,name)
                (description . ,description)
                (inputSchema . ,prepared-schema))
              tools)))
    (let* ((tools-array (vconcat (nreverse tools)))
           (response `((tools . ,tools-array))))
      (claude-code-ide-debug "Returning %d tools in response" (length tools-array))
      (claude-code-ide-mcp--make-response id response))))

(defun claude-code-ide-mcp--handle-prompts-list (id _params)
  "Handle the prompts/list request with ID."
  ;; Return empty prompts list for now - Claude Code doesn't require any prompts
  (claude-code-ide-mcp--make-response id '((prompts . []))))

(defun claude-code-ide-mcp--handle-tools-call (id params &optional session)
  "Handle the tools/call request with ID and PARAMS.
Optional SESSION contains the MCP session context."
  (claude-code-ide-debug "Handling tools/call request with id: %s" id)
  ;; Ensure handlers are loaded
  (let* ((tool-name (alist-get 'name params))
         (arguments (alist-get 'arguments params))
         (handler (alist-get tool-name claude-code-ide-mcp-tools nil nil #'string=)))
    (claude-code-ide-debug "Tool call: %s with arguments: %S" tool-name arguments)
    (if handler
        (condition-case err
            (progn
              (claude-code-ide-debug "Found handler for tool: %s" tool-name)
              (let ((result (if (member tool-name '("getDiagnostics"))
                                ;; Pass session to handlers that need it
                                (funcall handler arguments session)
                              (funcall handler arguments))))
                ;; Check if this is a deferred response
                (if (alist-get 'deferred result)
                    (progn
                      (claude-code-ide-debug "Tool %s returned deferred, storing id %s" tool-name id)
                      ;; Store the request ID for later in the current session
                      (let* ((unique-key (alist-get 'unique-key result))
                             (storage-key (if unique-key
                                              (format "%s-%s" tool-name unique-key)
                                            tool-name))
                             ;; Get session from result or try to find current session
                             (result-session (alist-get 'session result))
                             (session (or result-session
                                          (claude-code-ide-mcp--get-current-session))))
                        (if session
                            (let ((session-deferred (claude-code-ide-mcp-session-deferred session)))
                              (puthash storage-key id session-deferred)
                              (claude-code-ide-debug "Stored deferred response in session for %s"
                                                     (claude-code-ide-mcp-session-project-dir session)))
                          (claude-code-ide-debug "Warning: No session found, cannot store deferred response")))
                      ;; Don't send a response yet
                      nil)
                  ;; Normal response
                  (claude-code-ide-debug "Tool %s returned result: %S" tool-name result)
                  (claude-code-ide-mcp--make-response id `((content . ,result))))))
          (mcp-error
           (claude-code-ide-debug "Tool %s threw MCP error: %S" tool-name err)
           (claude-code-ide-mcp--make-error-response
            id -32603 (if (listp (cdr err))
                          (car (cdr err))
                        (cdr err))))
          (error
           (claude-code-ide-debug "Tool %s threw error: %S" tool-name err)
           (claude-code-ide-mcp--make-error-response
            id -32603 (format "Tool execution failed: %s" (error-message-string err)))))
      (progn
        (claude-code-ide-debug "Unknown tool requested: %s" tool-name)
        (claude-code-ide-mcp--make-error-response
         id -32601 (format "Unknown tool: %s" tool-name))))))

(defun claude-code-ide-mcp--handle-message (message &optional session)
  "Handle incoming JSON-RPC MESSAGE from SESSION."
  (when message
    (claude-code-ide-debug "Processing message with method: %s, id: %s"
                           (alist-get 'method message)
                           (alist-get 'id message))
    (let* ((method (alist-get 'method message))
           (id (alist-get 'id message))
           (params (alist-get 'params message))
           (response
            (cond
             ;; Request handlers
             ((string= method "initialize")
              (claude-code-ide-debug "Handling initialize request")
              (claude-code-ide-mcp--handle-initialize id params))
             ((string= method "tools/list")
              (claude-code-ide-debug "Handling tools/list request")
              (claude-code-ide-mcp--handle-tools-list id params))
             ((string= method "tools/call")
              (claude-code-ide-debug "Handling tools/call request")
              (claude-code-ide-mcp--handle-tools-call id params session))
             ((string= method "prompts/list")
              (claude-code-ide-debug "Handling prompts/list request")
              (claude-code-ide-mcp--handle-prompts-list id params))
             ;; Unknown method
             (id
              (claude-code-ide-debug "Unknown method: %s (sending error response)" method)
              (claude-code-ide-mcp--make-error-response
               id -32601 (format "Method not found: %s" method)))
             ;; Notification (no id) - ignore
             (t
              (claude-code-ide-debug "Received notification (no response needed): %s" method)
              nil))))
      ;; Send response if we have one
      (cond
       ;; We have a response to send
       (response
        (let ((client (if session
                          (claude-code-ide-mcp-session-client session)
                        ;; Fallback: try to find session from current buffer using cache
                        (when-let* ((s (or (when (and claude-code-ide-mcp--buffer-cache-valid
                                                     claude-code-ide-mcp--buffer-session-cache)
                                            claude-code-ide-mcp--buffer-session-cache)
                                          (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
                                                      (found-session (claude-code-ide-mcp--get-session-for-project project-dir)))
                                            ;; Cache the session (only cache non-nil values)
                                            (when found-session
                                              (setq claude-code-ide-mcp--buffer-session-cache found-session
                                                    claude-code-ide-mcp--buffer-cache-valid t))
                                            found-session))))
                          (claude-code-ide-mcp-session-client s)))))
          (if client
              (let ((response-text (json-encode response)))
                (claude-code-ide-debug "Sending response for method %s (id %s): %s" method id response-text)
                (claude-code-ide-debug "MCP sending response for %s: %s" method response-text)
                (condition-case err
                    (websocket-send-text client response-text)
                  (error
                   (claude-code-ide-debug "Error sending response: %s" err)
                   (claude-code-ide-debug "Error sending MCP response: %s" err))))
            (claude-code-ide-debug "No client connected, cannot send response"))))
       ;; No response but we have an ID (deferred response)
       ((and id (not response))
        (claude-code-ide-debug "No response generated for method %s (id %s) - likely deferred" method id)
        ;; Check if it's stored as deferred in any session
        (let ((tool-name (alist-get 'name params))
              (found nil))
          (when tool-name
            (maphash (lambda (_proj-dir s)
                       (when (gethash tool-name (claude-code-ide-mcp-session-deferred s))
                         (setq found t)))
                     claude-code-ide-mcp--sessions)
            (when found
              (claude-code-ide-debug "Confirmed: %s is waiting for deferred response" tool-name)))))
       ;; No response and no ID (notification)
       (t
        (claude-code-ide-debug "No response needed for notification: %s" method))))))

;;; WebSocket Server


(defun claude-code-ide-mcp--find-free-port ()
  "Find a free port in the configured range."
  (let ((min-port (car claude-code-ide-mcp-port-range))
        (max-port (cdr claude-code-ide-mcp-port-range))
        (max-attempts claude-code-ide-mcp-max-port-attempts)
        (attempts 0)
        (found-port nil))
    (claude-code-ide-debug "Starting port search in range %d-%d" min-port max-port)
    (while (and (< attempts max-attempts) (not found-port))
      (let* ((port (+ min-port (random (- max-port min-port))))
             (server (condition-case err
                         (progn
                           (claude-code-ide-debug "Trying to bind to port %d" port)
                           (let ((ws-server (websocket-server
                                             port
                                             :host "127.0.0.1"
                                             :on-open #'claude-code-ide-mcp--on-open
                                             :on-message #'claude-code-ide-mcp--on-message
                                             :on-error #'claude-code-ide-mcp--on-error
                                             :on-close #'claude-code-ide-mcp--on-close
                                             :on-ping #'claude-code-ide-mcp--on-ping
                                             :protocol '("mcp"))))
                             ;; Add debug filter to see raw data (only if debugging)
                             (when (and ws-server claude-code-ide-debug)
                               (set-process-filter ws-server
                                                   (lambda (proc string)
                                                     ;; Only log if it looks like text (not binary WebSocket frames)
                                                     (if (string-match-p "^[[:print:][:space:]]+$" string)
                                                         (claude-code-ide-debug "Server received text data: %S" string)
                                                       (claude-code-ide-debug "Server received binary frame (%d bytes)" (length string)))
                                                     (websocket-server-filter proc string))))
                             ws-server))
                       (error
                        (claude-code-ide-debug "Failed to bind to port %d: %s" port err)
                        (claude-code-ide-debug "Failed to start server on port %d: %s" port err)
                        nil))))
        (if server
            (progn
              (setq found-port (cons server port))
              (claude-code-ide-debug "Successfully bound to port %d" port)
              (claude-code-ide-debug "Server object: %S" server)
              (claude-code-ide-debug "WebSocket server started on port %d" port))
          (cl-incf attempts))))
    (or found-port
        (error "Could not find free port in range %d-%d" min-port max-port))))

(defun claude-code-ide-mcp--on-open (ws)
  "Handle new WebSocket connection WS."
  (claude-code-ide-debug "=== WebSocket connection opened ===")
  (claude-code-ide-debug "WebSocket object: %S" ws)
  (claude-code-ide-debug "WebSocket state: %s" (websocket-ready-state ws))
  (claude-code-ide-debug "WebSocket URL: %s" (websocket-url ws))

  ;; Find the session that owns this connection
  ;; We need to extract the port from the websocket connection info
  (let ((session nil)
        (port nil))
    ;; Try to extract port from the websocket string representation
    ;; Format: "websocket server on port XXXXX <127.0.0.1:YYYYY>"
    (let ((ws-string (format "%s" ws)))
      (claude-code-ide-debug "WebSocket string representation: %s" ws-string)
      (when (string-match "on port \\([0-9]+\\)" ws-string)
        (setq port (string-to-number (match-string 1 ws-string)))
        (claude-code-ide-debug "Extracted port: %d" port)))
    ;; If we couldn't extract port from string, we'll have to search all sessions
    ;; Find session by matching port
    (when port
      (maphash (lambda (_project-dir s)
                 (when (eq (claude-code-ide-mcp-session-port s) port)
                   (setq session s)))
               claude-code-ide-mcp--sessions))

    (if session
        (progn
          ;; Update session with client
          (setf (claude-code-ide-mcp-session-client session) ws)
          (claude-code-ide-debug "Claude Code connected to MCP server for %s"
                                 (file-name-nondirectory
                                  (directory-file-name (claude-code-ide-mcp-session-project-dir session))))

          ;; Send initial active editor notification if we have one in the project
          (let ((file-path (buffer-file-name))
                (project-dir (claude-code-ide-mcp-session-project-dir session)))
            (when (and file-path
                       project-dir
                       (string-prefix-p (expand-file-name project-dir)
                                        (expand-file-name file-path)))
              (setf (claude-code-ide-mcp-session-last-buffer session) (current-buffer))
              ;; Update MCP tools server's last active buffer
              (when-let* ((session-id (gethash project-dir claude-code-ide--session-ids)))
                (claude-code-ide-mcp-server-update-last-active-buffer session-id (current-buffer)))
              (run-at-time claude-code-ide-mcp-initial-notification-delay nil
                           (lambda ()
                             (when-let* ((s (gethash project-dir claude-code-ide-mcp--sessions)))
                               (let ((file-path (buffer-file-name)))
                                 (claude-code-ide-mcp--send-notification
                                  "workspace/didChangeActiveEditor"
                                  `((uri . ,(concat "file://" file-path))
                                    (path . ,file-path)
                                    (name . ,(buffer-name))))))))))
          (claude-code-ide-debug "Warning: Could not find session for WebSocket connection")))))

(defun claude-code-ide-mcp--on-message (ws frame)
  "Handle incoming WebSocket message from WS in FRAME."
  (claude-code-ide-debug "=== Received WebSocket frame ===")

  ;; Check if frame is actually a frame struct
  ;; In some edge cases, the websocket library might pass something else
  (condition-case err
      (progn
        ;; Try to get the opcode - this will fail if frame is not a proper struct
        (claude-code-ide-debug "Frame opcode: %s" (websocket-frame-opcode frame))

        ;; Find the session for this websocket
        (let ((session (claude-code-ide-mcp--find-session-by-websocket ws)))
          (if session
              (progn
                (let* ((text (websocket-frame-text frame))
                       (message (condition-case err
                                    (json-read-from-string text)
                                  (error
                                   (claude-code-ide-debug "JSON parse error: %s" err)
                                   (claude-code-ide-debug "Raw text: %s" text)
                                   (claude-code-ide-debug "Failed to parse JSON: %s" err)
                                   nil))))
                  (claude-code-ide-debug "Received: %s" text)
                  (claude-code-ide-debug "MCP received: %s" text)
                  (when message
                    (claude-code-ide-mcp--handle-message message session))))
            (claude-code-ide-debug "Warning: Could not find session for WebSocket message"))))
    (error
     ;; If we get an error accessing frame properties, log it and continue
     (claude-code-ide-debug "Error processing WebSocket frame: %s" err)
     (claude-code-ide-debug "Frame type: %s, Frame value: %S" (type-of frame) frame)
     ;; If frame is a string, it might be raw text data
     (when (stringp frame)
       (claude-code-ide-debug "Received raw string instead of frame: %s" frame))
     ;; Don't crash the connection, just skip this message
     nil)))

(defun claude-code-ide-mcp--on-error (ws type err)
  "Handle WebSocket error from WS of TYPE with ERR."
  (claude-code-ide-debug "=== WebSocket error ===")
  (claude-code-ide-debug "Error type: %s" type)
  (claude-code-ide-debug "Error details: %S" err)
  (claude-code-ide-debug "WebSocket state: %s" (websocket-ready-state ws))
  (claude-code-ide-log "MCP WebSocket error (%s): %s" type err))

(defun claude-code-ide-mcp--on-close (ws)
  "Handle WebSocket close for WS."
  (claude-code-ide-debug "=== WebSocket connection closed ===")

  ;; Find the session for this websocket
  (let ((session (claude-code-ide-mcp--find-session-by-websocket ws)))
    (when session
      ;; Clear the client in the session
      (setf (claude-code-ide-mcp-session-client session) nil)
      ;; Stop the ping timer for this session
      (claude-code-ide-mcp--stop-ping-timer session)
      (claude-code-ide-debug "Final WebSocket state: %s" (websocket-ready-state ws))
      (claude-code-ide-debug "Claude Code disconnected from MCP server for %s"
                             (file-name-nondirectory
                              (directory-file-name (claude-code-ide-mcp-session-project-dir session)))))))

(defun claude-code-ide-mcp--on-ping (_ws _frame)
  "Handle WebSocket ping from WS in FRAME."
  (claude-code-ide-debug "Received ping frame, sending pong")
  ;; websocket.el automatically sends pong response, we just log it
  )

;;; Ping/Pong Keepalive

(defun claude-code-ide-mcp--start-ping-timer (session)
  "Start the ping timer for keepalive for SESSION."
  (claude-code-ide-mcp--stop-ping-timer session)
  (let ((timer (run-with-timer claude-code-ide-mcp-ping-interval claude-code-ide-mcp-ping-interval
                               (lambda ()
                                 (claude-code-ide-mcp--send-ping session)))))
    (setf (claude-code-ide-mcp-session-ping-timer session) timer)))

(defun claude-code-ide-mcp--stop-ping-timer (session)
  "Stop the ping timer for SESSION."
  (when-let* ((timer (claude-code-ide-mcp-session-ping-timer session)))
    (cancel-timer timer)
    (setf (claude-code-ide-mcp-session-ping-timer session) nil)))

(defun claude-code-ide-mcp--send-ping (session)
  "Send a ping frame to keep connection alive for SESSION."
  (when-let* ((client (claude-code-ide-mcp-session-client session)))
    (condition-case err
        (websocket-send client
                        (make-websocket-frame :opcode 'ping
                                              :payload ""))
      (error
       (claude-code-ide-debug "Failed to send ping: %s" err)))))

;;; Cache Management

(defun claude-code-ide-mcp--invalidate-buffer-cache ()
  "Invalidate the buffer-local cache for project and session.
This should be called when the buffer's context might have changed."
  (setq claude-code-ide-mcp--buffer-project-cache nil
        claude-code-ide-mcp--buffer-session-cache nil
        claude-code-ide-mcp--buffer-cache-valid nil))

(defun claude-code-ide-mcp--setup-buffer-cache-hooks ()
  "Set up hooks to invalidate cache when buffer context changes."
  ;; Invalidate cache when file is saved to a new location
  (add-hook 'after-save-hook #'claude-code-ide-mcp--invalidate-buffer-cache nil t)
  ;; Invalidate cache when buffer's file association changes
  (add-hook 'after-change-major-mode-hook #'claude-code-ide-mcp--invalidate-buffer-cache nil t))

;;; Selection and Buffer Tracking

(defun claude-code-ide-mcp--track-selection ()
  "Track selection changes and notify Claude for the current buffer's project."
  ;; Early exit for non-file buffers
  (when (buffer-file-name)
    ;; Try to use cached session first
    (let ((session (if (and claude-code-ide-mcp--buffer-cache-valid
                            claude-code-ide-mcp--buffer-session-cache)
                       ;; Use cached session (only if non-nil)
                       claude-code-ide-mcp--buffer-session-cache
                     ;; No valid cache or cached nil, look up session
                     (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
                                 (found-session (claude-code-ide-mcp--get-session-for-project project-dir)))
                       ;; Cache the session (only cache non-nil values)
                       (setq claude-code-ide-mcp--buffer-session-cache found-session
                             claude-code-ide-mcp--buffer-cache-valid t)
                       found-session))))
      ;; Only proceed if we have a session
      (when session
        ;; Cancel any existing timer for this session
        (when-let* ((timer (claude-code-ide-mcp-session-selection-timer session)))
          (cancel-timer timer))
        ;; Set new timer for this session
        (let ((project-dir (claude-code-ide-mcp-session-project-dir session))
              (current-buffer (current-buffer)))
          (setf (claude-code-ide-mcp-session-selection-timer session)
                (run-with-timer claude-code-ide-mcp-selection-delay nil
                                (lambda ()
                                  ;; Make sure we're in the right buffer context when timer fires
                                  (when (buffer-live-p current-buffer)
                                    (with-current-buffer current-buffer
                                      (claude-code-ide-mcp--send-selection-for-project project-dir)))))))))))

(defun claude-code-ide-mcp--send-selection-for-project (project-dir)
  "Send current selection to Claude for PROJECT-DIR."
  (when-let* ((session (claude-code-ide-mcp--get-session-for-project project-dir)))
    ;; Clear the timer in the session
    (setf (claude-code-ide-mcp-session-selection-timer session) nil)

    (let ((file-path (buffer-file-name)))
      ;; Only process if we have a client and a file-backed buffer
      (when (and (claude-code-ide-mcp-session-client session)
                 file-path)
        ;; Check if file is within project
        (let ((file-in-project (string-prefix-p (expand-file-name project-dir)
                                                (expand-file-name file-path))))
          (if file-in-project
              ;; File is in project - check cursor/selection changes
              (let* ((cursor-pos (point))
                     (current-state (if (use-region-p)
                                        (list cursor-pos (region-beginning) (region-end))
                                      (list cursor-pos cursor-pos cursor-pos)))
                     (last-state (claude-code-ide-mcp-session-last-selection session))
                     (state-changed (not (equal current-state last-state))))
                ;; Send notification if cursor or selection changed
                (when state-changed
                  (setf (claude-code-ide-mcp-session-last-selection session) current-state)
                  (let ((selection (claude-code-ide-mcp-handle-get-current-selection nil)))
                    (claude-code-ide-mcp--send-notification "selection_changed" selection))))
            ;; File outside project - reset selection state
            (setf (claude-code-ide-mcp-session-last-selection session) nil))))
      ;; Reset selection state for non-file buffers
      (unless file-path
        (setf (claude-code-ide-mcp-session-last-selection session) nil)))))

(defun claude-code-ide-mcp--send-selection ()
  "Send current selection to Claude."
  ;; Try to use cached session first to avoid redundant project lookups
  (let ((session (if (and claude-code-ide-mcp--buffer-cache-valid
                          claude-code-ide-mcp--buffer-session-cache)
                     ;; Use cached session (only if non-nil)
                     claude-code-ide-mcp--buffer-session-cache
                   ;; No valid cache or cached nil, look up session
                   (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
                               (found-session (claude-code-ide-mcp--get-session-for-project project-dir)))
                     ;; Cache the session (only cache non-nil values)
                     (setq claude-code-ide-mcp--buffer-session-cache found-session
                           claude-code-ide-mcp--buffer-cache-valid t)
                     found-session))))
    (when session
      (let ((project-dir (claude-code-ide-mcp-session-project-dir session)))
        (claude-code-ide-mcp--send-selection-for-project project-dir)))))

(defun claude-code-ide-mcp--track-active-buffer ()
  "Track active buffer changes and notify Claude for the current buffer's project."
  (let ((current-buffer (current-buffer))
        (file-path (buffer-file-name)))
    ;; Early exit for non-file buffers
    (when file-path
      ;; Try to use cached session first
      (let ((session (if (and claude-code-ide-mcp--buffer-cache-valid
                              claude-code-ide-mcp--buffer-session-cache)
                         ;; Use cached session (only if non-nil)
                         claude-code-ide-mcp--buffer-session-cache
                       ;; No valid cache or cached nil, look up session
                       (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
                                   (found-session (claude-code-ide-mcp--get-session-for-project project-dir)))
                         ;; Cache the session (only cache non-nil values)
                         (setq claude-code-ide-mcp--buffer-session-cache found-session
                               claude-code-ide-mcp--buffer-cache-valid t)
                         found-session))))
        ;; Only proceed if we have a session with a client
        (when (and session (claude-code-ide-mcp-session-client session))
          (let ((project-dir (claude-code-ide-mcp-session-project-dir session)))
            ;; Check if this is a different buffer than last tracked in the session
            ;; and that the file is within the project directory
            (when (and (not (eq current-buffer (claude-code-ide-mcp-session-last-buffer session)))
                       (string-prefix-p (expand-file-name project-dir)
                                        (expand-file-name file-path)))
              (setf (claude-code-ide-mcp-session-last-buffer session) current-buffer)
              ;; Update MCP tools server's last active buffer
              (when-let* ((session-id (gethash project-dir claude-code-ide--session-ids)))
                (claude-code-ide-mcp-server-update-last-active-buffer session-id current-buffer))
              ;; Send notification
              (claude-code-ide-mcp--send-notification
               "workspace/didChangeActiveEditor"
               `((uri . ,(concat "file://" file-path))
                 (path . ,file-path)
                 (name . ,(buffer-name current-buffer)))))))))))

;;; Public API

(defun claude-code-ide-mcp-start (&optional project-directory)
  "Start the MCP server for PROJECT-DIRECTORY."
  (claude-code-ide-debug "=== Starting MCP server ===")

  (let* ((project-dir (expand-file-name (or project-directory default-directory)))
         (existing-session (gethash project-dir claude-code-ide-mcp--sessions)))

    ;; If there's an existing session for this project, return its port
    (if existing-session
        (progn
          (claude-code-ide-debug "Reusing existing session for %s" project-dir)
          (claude-code-ide-mcp-session-port existing-session))

      ;; Create new session
      (let* ((session (make-claude-code-ide-mcp-session
                       :project-dir project-dir
                       :deferred (make-hash-table :test 'equal)
                       :active-diffs (make-hash-table :test 'equal)
                       :original-tab (when (fboundp 'tab-bar--current-tab)
                                       (tab-bar--current-tab))))
             (server-and-port (claude-code-ide-mcp--find-free-port))
             (server (car server-and-port))
             (port (cdr server-and-port)))

        ;; Set port and server in session
        (setf (claude-code-ide-mcp-session-port session) port
              (claude-code-ide-mcp-session-server session) server)

        ;; Store session
        (puthash project-dir session claude-code-ide-mcp--sessions)

        (claude-code-ide-debug "Project directory: %s" project-dir)
        (claude-code-ide-debug "Creating lockfile for port %d" port)
        (claude-code-ide-mcp--create-lockfile port project-dir)

        ;; Set up hooks for selection and buffer tracking
        (add-hook 'post-command-hook #'claude-code-ide-mcp--track-selection)
        (add-hook 'post-command-hook #'claude-code-ide-mcp--track-active-buffer)

        (claude-code-ide-debug "MCP server ready on port %d" port)
        (claude-code-ide-debug "MCP server started on port %d for %s" port
                               (file-name-nondirectory (directory-file-name project-dir)))
        port))))

(defun claude-code-ide-mcp-stop-session (project-dir)
  "Stop the MCP session for PROJECT-DIR."
  (when-let* ((session (gethash project-dir claude-code-ide-mcp--sessions)))
    (claude-code-ide-debug "Stopping MCP session for %s" project-dir)

    ;; Close server and client
    (when-let* ((server (claude-code-ide-mcp-session-server session)))
      (websocket-server-close server))

    ;; Stop timers
    (when-let* ((ping-timer (claude-code-ide-mcp-session-ping-timer session)))
      (cancel-timer ping-timer))
    (when-let* ((sel-timer (claude-code-ide-mcp-session-selection-timer session)))
      (cancel-timer sel-timer))

    ;; Remove lockfile
    (when-let* ((port (claude-code-ide-mcp-session-port session)))
      (claude-code-ide-debug "Removing lockfile for port %d" port)
      (claude-code-ide-mcp--remove-lockfile port))

    ;; Remove session from registry
    (remhash project-dir claude-code-ide-mcp--sessions)

    ;; Invalidate cache in all buffers that belong to this project
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and claude-code-ide-mcp--buffer-project-cache
                   (string= claude-code-ide-mcp--buffer-project-cache project-dir))
          (claude-code-ide-mcp--invalidate-buffer-cache))))

    ;; Remove hooks if no more sessions
    (when (= 0 (hash-table-count claude-code-ide-mcp--sessions))
      (remove-hook 'post-command-hook #'claude-code-ide-mcp--track-selection)
      (remove-hook 'post-command-hook #'claude-code-ide-mcp--track-active-buffer))

    (claude-code-ide-debug "MCP server stopped for %s"
                           (file-name-nondirectory (directory-file-name project-dir)))))

(defun claude-code-ide-mcp-stop ()
  "Stop the MCP server for the current project or directory."
  (claude-code-ide-debug "Stopping MCP server...")

  ;; Try to determine which session to stop
  (let ((project-dir (claude-code-ide-mcp--get-buffer-project)))

    (if project-dir
        (claude-code-ide-mcp-stop-session project-dir)
      ;; No specific project - stop all sessions (backward compatibility)
      (let ((sessions (hash-table-keys claude-code-ide-mcp--sessions)))
        (if sessions
            (dolist (dir sessions)
              (claude-code-ide-mcp-stop-session dir))
          (claude-code-ide-debug "No MCP servers running"))))))

(defun claude-code-ide-mcp-send-at-mentioned ()
  "Send at-mentioned notification.
If a region is selected, send the selected lines.
Otherwise, send the current line."
  (let* ((file-path (or (buffer-file-name) ""))
         (start-line (if (use-region-p)
                         (1- (line-number-at-pos (region-beginning)))
                       (1- (line-number-at-pos (point)))))
         (end-line (if (use-region-p)
                       (1- (line-number-at-pos (region-end)))
                     (1- (line-number-at-pos (point))))))
    (claude-code-ide-mcp--send-notification
     "at_mentioned"
     `((filePath . ,file-path)
       (lineStart . ,start-line)
       (lineEnd . ,end-line)))))

(defun claude-code-ide-mcp-complete-deferred (session tool-name result &optional unique-key)
  "Complete a deferred response for SESSION and TOOL-NAME with RESULT.
SESSION is the MCP session that owns the deferred response.
If UNIQUE-KEY is provided, it's used to disambiguate multiple deferred
responses."
  (let* ((lookup-key (if unique-key
                         (format "%s-%s" tool-name unique-key)
                       tool-name)))
    (claude-code-ide-debug "Complete deferred for %s" lookup-key)
    (if (not session)
        (claude-code-ide-debug "No session provided for completing deferred response %s" lookup-key)
      ;; Use the provided session directly
      (let* ((session-deferred (claude-code-ide-mcp-session-deferred session))
             (id (gethash lookup-key session-deferred)))
        (if id
            (let ((client (claude-code-ide-mcp-session-client session)))
              (claude-code-ide-debug "Found deferred response id %s in session for %s"
                                     id (claude-code-ide-mcp-session-project-dir session))
              (remhash lookup-key session-deferred)
              (if client
                  (let* ((response (claude-code-ide-mcp--make-response id `((content . ,result))))
                         (json-response (json-encode response)))
                    (claude-code-ide-debug "Sending deferred response: %s" json-response)
                    (websocket-send-text client json-response)
                    (claude-code-ide-debug "Deferred response sent"))
                (claude-code-ide-debug "No client connected for session, cannot send deferred response")))
          (claude-code-ide-debug "No deferred response found for %s" lookup-key))))))

;;; Cleanup on exit

(defun claude-code-ide-mcp--cleanup ()
  "Cleanup all MCP sessions on Emacs exit."
  ;; Stop all sessions
  (maphash (lambda (project-dir _session)
             (claude-code-ide-mcp-stop-session project-dir))
           claude-code-ide-mcp--sessions))

(add-hook 'kill-emacs-hook #'claude-code-ide-mcp--cleanup)

(provide 'claude-code-ide-mcp)

;;; claude-code-ide-mcp.el ends here
