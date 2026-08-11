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
(declare-function websocket-close "websocket" (websocket))
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
  "Hash table mapping session IDs to MCP sessions.
A project directory may own any number of sessions; use
`claude-code-ide-mcp--sessions-for-project' to enumerate them.")

(defvar claude-code-ide-mcp--selection-timers (make-hash-table :test 'equal)
  "Hash table mapping project directories to selection debounce timers.
One timer per project; when it fires the selection is fanned out to
every session of that project.")

(defvar-local claude-code-ide--session nil
  "The MCP session owned by this terminal buffer.
Set in Claude Code terminal buffers only; this backpointer is the
authoritative way to recognize a Claude buffer and identify its
instance.")

;; Buffer-local cache variables for performance optimization
(defvar-local claude-code-ide-mcp--buffer-project-cache nil
  "Cached project directory for the current buffer.
This avoids repeated project lookups on every cursor movement.")

(defvar-local claude-code-ide-mcp--buffer-cache-valid nil
  "Whether the buffer-local cache is valid.
Set to nil when cache needs to be invalidated.")

;;; Error Definition

(define-error 'mcp-error "MCP Error" 'error)

;;; Session Management

(cl-defstruct claude-code-ide-mcp-session
  "Structure to hold all state for a single Claude Code instance."
  session-id       ; Unique session ID string (registry key, immutable)
  instance-name    ; Instance name string, or nil for the unnamed instance
  buffer           ; Terminal buffer running the CLI
  process          ; Terminal process running the CLI
  window-slot      ; Side-window slot assigned to this instance
  last-used        ; Float-time of last user interaction (per-project MRU)
  cleanup-done     ; Non-nil once this session has been cleaned up
  server           ; WebSocket server instance
  client           ; Connected WebSocket client
  port             ; Server port
  project-dir      ; Project directory
  deferred         ; Hash table of deferred responses
  ping-timer       ; Ping timer
  last-selection   ; Last selection state
  last-buffer      ; Last active buffer
  active-diffs     ; Hash table of active diffs
  original-tab     ; Original tab-bar tab where Claude was opened
  cli-pid)         ; PID of the connected CLI process

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

(defun claude-code-ide-mcp--get-session-by-id (session-id)
  "Get the MCP session with SESSION-ID, or nil."
  (when session-id
    (gethash session-id claude-code-ide-mcp--sessions)))

(defun claude-code-ide-mcp--sessions-for-project (project-dir)
  "Return the list of MCP sessions whose project is PROJECT-DIR.
Sessions are returned in no particular order."
  (when project-dir
    (let ((expanded (expand-file-name project-dir))
          (sessions '()))
      (maphash (lambda (_id session)
                 (when (string= (expand-file-name
                                 (claude-code-ide-mcp-session-project-dir session))
                                expanded)
                   (push session sessions)))
               claude-code-ide-mcp--sessions)
      sessions)))

(defun claude-code-ide-mcp--mru-session (project-dir)
  "Return the most recently used session of PROJECT-DIR, or nil."
  (let ((best nil))
    (dolist (session (claude-code-ide-mcp--sessions-for-project project-dir))
      (when (or (null best)
                (> (or (claude-code-ide-mcp-session-last-used session) 0)
                   (or (claude-code-ide-mcp-session-last-used best) 0)))
        (setq best session)))
    best))

(defun claude-code-ide-mcp--get-current-session ()
  "Get the MCP session the current buffer belongs to.
Resolution order: the terminal buffer's own session backpointer, the
sole session of the buffer's project, or the project's most recently
used session."
  (or claude-code-ide--session
      (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project)))
        (let ((sessions (claude-code-ide-mcp--sessions-for-project project-dir)))
          (if (cdr sessions)
              (claude-code-ide-mcp--mru-session project-dir)
            (car sessions))))))

(defun claude-code-ide-mcp--active-sessions ()
  "Return a list of all active MCP sessions."
  (let ((sessions '()))
    (maphash (lambda (_id session)
               (push session sessions))
             claude-code-ide-mcp--sessions)
    sessions))

;;; Backward Compatibility Layer

;;; Lockfile Management

(defun claude-code-ide-mcp--lockfile-directory ()
  "Return the directory for MCP lockfiles.
The Claude Code CLI discovers the MCP server by scanning this directory for
`<port>.lock' files, so both sides must agree on it.  The CLI derives it from
the `CLAUDE_CONFIG_DIR' environment variable, defaulting to `~/.claude'; we
mirror that here.  Since the CLI runs as a subprocess of Emacs and inherits
this environment, setting `CLAUDE_CONFIG_DIR' relocates the lockfile on both
sides in lockstep.

An empty `CLAUDE_CONFIG_DIR' is treated as unset (matching the wider Claude
Code convention), so a misconfigured shell does not resolve the lockfile
relative to `default-directory'."
  (let ((config-dir (getenv "CLAUDE_CONFIG_DIR")))
    (expand-file-name
     "ide/"
     (file-name-as-directory
      (if (and config-dir (not (string= config-dir "")))
          config-dir
        "~/.claude")))))

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

(defun claude-code-ide-mcp--send-notification (method params session)
  "Send a JSON-RPC notification with METHOD and PARAMS to SESSION.
SESSION is required; broadcasting to several instances is done by
calling this once per session."
  (when-let* ((client (and session
                           (claude-code-ide-mcp-session-client session))))
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

(defun claude-code-ide-mcp--handle-initialize (id _params &optional session)
  "Handle the initialize request with ID for SESSION."
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
                     (make-hash-table :test 'equal)
                     session)))

  (let ((response `((protocolVersion . ,claude-code-ide-mcp-version)
                    (capabilities . ((tools . ((listChanged . t)))
                                     (resources . ((subscribe . :json-false)
                                                   (listChanged . :json-false)))
                                     (prompts . ((listChanged . t)))
                                     (logging . ,(make-hash-table :test 'equal))))
                    (serverInfo . ((name . "claude-code-ide-mcp")
                                   (version . "0.3.0"))))))
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

(defun claude-code-ide-mcp--handle-tools-call (id params &optional session ws)
  "Handle the tools/call request with ID and PARAMS.
Optional SESSION contains the MCP session context; WS is the websocket
the request arrived on, used to detect a client swap during a yielding
handler."
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
              (let ((result (funcall handler arguments session)))
                ;; Check if this is a deferred response
                (if (alist-get 'deferred result)
                    (progn
                      (claude-code-ide-debug "Tool %s returned deferred, storing id %s" tool-name id)
                      ;; Correlate strictly with the session whose websocket
                      ;; carried the request; a guessed session could file the
                      ;; id under a sibling instance and answer the wrong CLI.
                      (let* ((unique-key (alist-get 'unique-key result))
                             (storage-key (if unique-key
                                              (format "%s-%s" tool-name unique-key)
                                            tool-name)))
                        (cond
                         ;; The requester was replaced while the handler ran;
                         ;; its ids are dead — storing one would answer the
                         ;; successor with an id it never sent
                         ((and ws session
                               (not (eq ws (claude-code-ide-mcp-session-client session))))
                          (claude-code-ide-debug
                           "Discarding deferred id %s: client replaced mid-request" id))
                         (session
                          (let ((session-deferred (claude-code-ide-mcp-session-deferred session)))
                            (puthash storage-key id session-deferred)
                            (claude-code-ide-debug "Stored deferred response in session for %s"
                                                   (claude-code-ide-mcp-session-project-dir session))))
                         (t
                          (claude-code-ide-debug "Warning: No session found, cannot store deferred response"))))
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

(defun claude-code-ide-mcp--handle-message (message &optional session ws)
  "Handle incoming JSON-RPC MESSAGE from SESSION.
WS is the websocket the message arrived on; a handler can yield (e.g.
`sit-for' inside executeCode), letting a CLI reconnect swap the
session's client mid-request, so the response must go back to WS — the
socket that sent the request — or be dropped, never to its successor."
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
              (claude-code-ide-mcp--handle-initialize id params session))
             ((string= method "tools/list")
              (claude-code-ide-debug "Handling tools/list request")
              (claude-code-ide-mcp--handle-tools-list id params))
             ((string= method "tools/call")
              (claude-code-ide-debug "Handling tools/call request")
              (claude-code-ide-mcp--handle-tools-call id params session ws))
             ((string= method "prompts/list")
              (claude-code-ide-debug "Handling prompts/list request")
              (claude-code-ide-mcp--handle-prompts-list id params))
             ;; Unknown method
             (id
              (claude-code-ide-debug "Unknown method: %s (sending error response)" method)
              (claude-code-ide-mcp--make-error-response
               id -32601 (format "Method not found: %s" method)))
             ;; Notifications (no id)
             ((string= method "ide_connected")
              (when-let* ((pid (alist-get 'pid params)))
                (claude-code-ide-debug "CLI connected with PID: %s" pid)
                (when session
                  (setf (claude-code-ide-mcp-session-cli-pid session) pid)))
              nil)
             (t
              (claude-code-ide-debug "Received notification (no response needed): %s" method)
              nil))))
      ;; Send response if we have one
      (cond
       ;; We have a response to send
       (response
        (let* ((current-client (and session
                                    (claude-code-ide-mcp-session-client session)))
               ;; If the session's client changed while the handler ran,
               ;; the requester is gone — drop the response
               (client (if (and ws (not (eq ws current-client)))
                           (progn
                             (claude-code-ide-debug
                              "Dropping response for %s (id %s): client replaced mid-request"
                              method id)
                             nil)
                         current-client)))
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
        ;; Check if it's stored as deferred in the requesting session
        ;; (ids are keyed as TOOL-NAME or TOOL-NAME-UNIQUE-KEY)
        (let ((tool-name (alist-get 'name params))
              (found nil))
          (when (and tool-name session)
            (maphash (lambda (key _stored-id)
                       (when (or (equal key tool-name)
                                 (string-prefix-p (concat tool-name "-") key))
                         (setq found t)))
                     (claude-code-ide-mcp-session-deferred session))
            (when found
              (claude-code-ide-debug "Confirmed: %s is waiting for deferred response" tool-name)))))
       ;; No response and no ID (notification)
       (t
        (claude-code-ide-debug "No response needed for notification: %s" method))))))

;;; WebSocket Server


(defun claude-code-ide-mcp--find-free-port (session)
  "Start a WebSocket server for SESSION on a free port in the configured range.
The server callbacks close over SESSION, so every connection event is
routed to its owning instance without any registry lookup.
Returns a cons cell (SERVER . PORT)."
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
                                             :on-open (lambda (ws)
                                                        (claude-code-ide-mcp--on-open session ws))
                                             :on-message (lambda (ws frame)
                                                           (claude-code-ide-mcp--on-message session ws frame))
                                             :on-error (lambda (ws type err)
                                                         (claude-code-ide-mcp--on-error session ws type err))
                                             :on-close (lambda (ws)
                                                         (claude-code-ide-mcp--on-close session ws))
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

(defun claude-code-ide-mcp--on-open (session ws)
  "Handle new WebSocket connection WS for SESSION."
  (claude-code-ide-debug "=== WebSocket connection opened ===")
  (claude-code-ide-debug "WebSocket object: %S" ws)
  (claude-code-ide-debug "WebSocket state: %s" (websocket-ready-state ws))
  (claude-code-ide-debug "WebSocket URL: %s" (websocket-url ws))

  ;; Adopt the new connection, dropping any previous client (CLI
  ;; reconnect).  Pending deferred ids died with the old socket, so flush
  ;; them; active diffs stay — the user may be mid-ediff.
  (let ((old-client (claude-code-ide-mcp-session-client session)))
    (when (and old-client (not (eq old-client ws)))
      (claude-code-ide-debug "Replacing existing client connection")
      (ignore-errors (websocket-close old-client))
      (clrhash (claude-code-ide-mcp-session-deferred session))
      (setf (claude-code-ide-mcp-session-last-selection session) nil)))
  (setf (claude-code-ide-mcp-session-client session) ws)
  (claude-code-ide-debug "Claude Code connected to MCP server for %s"
                         (file-name-nondirectory
                          (directory-file-name (claude-code-ide-mcp-session-project-dir session))))

  ;; Track initial active editor if we have one in the project
  (let ((file-path (buffer-file-name))
        (project-dir (claude-code-ide-mcp-session-project-dir session)))
    (when (and file-path
               project-dir
               (string-prefix-p (expand-file-name project-dir)
                                (expand-file-name file-path)))
      (setf (claude-code-ide-mcp-session-last-buffer session) (current-buffer))
      ;; Update MCP tools server's last active buffer
      (claude-code-ide-mcp-server-update-last-active-buffer
       (claude-code-ide-mcp-session-session-id session)
       (current-buffer)))))

(defun claude-code-ide-mcp--on-message (session ws frame)
  "Handle incoming WebSocket message from WS in FRAME for SESSION."
  (claude-code-ide-debug "=== Received WebSocket frame ===")

  ;; Check if frame is actually a frame struct
  ;; In some edge cases, the websocket library might pass something else
  (condition-case err
      (progn
        ;; Try to get the opcode - this will fail if frame is not a proper struct
        (claude-code-ide-debug "Frame opcode: %s" (websocket-frame-opcode frame))

        ;; Ignore traffic from a socket this session no longer owns
        ;; (it was replaced by a reconnect and is about to close)
        (if (eq ws (claude-code-ide-mcp-session-client session))
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
                (claude-code-ide-mcp--handle-message message session ws)))
          (claude-code-ide-debug "Ignoring message from replaced WebSocket client")))
    (error
     ;; If we get an error accessing frame properties, log it and continue
     (claude-code-ide-debug "Error processing WebSocket frame: %s" err)
     (claude-code-ide-debug "Frame type: %s, Frame value: %S" (type-of frame) frame)
     ;; If frame is a string, it might be raw text data
     (when (stringp frame)
       (claude-code-ide-debug "Received raw string instead of frame: %s" frame))
     ;; Don't crash the connection, just skip this message
     nil)))

(defun claude-code-ide-mcp--on-error (session ws type err)
  "Handle WebSocket error from WS of TYPE with ERR for SESSION."
  (claude-code-ide-debug "=== WebSocket error ===")
  (claude-code-ide-debug "Error type: %s" type)
  (claude-code-ide-debug "Error details: %S" err)
  (claude-code-ide-debug "WebSocket state: %s" (websocket-ready-state ws))
  (claude-code-ide-log "MCP WebSocket error in %s (%s): %s"
                       (file-name-nondirectory
                        (directory-file-name (claude-code-ide-mcp-session-project-dir session)))
                       type err))

(defun claude-code-ide-mcp--on-close (session ws)
  "Handle WebSocket close of WS for SESSION."
  (claude-code-ide-debug "=== WebSocket connection closed ===")

  ;; A socket that was already replaced by a reconnect no longer owns the
  ;; session; its close event must not clear the successor.
  (when (eq ws (claude-code-ide-mcp-session-client session))
    (setf (claude-code-ide-mcp-session-client session) nil)
    ;; Pending JSON-RPC ids and selection dedupe state died with the
    ;; socket; flushing here (not only in on-open) covers the reconnect
    ;; order where the old socket closes before the new one opens
    (clrhash (claude-code-ide-mcp-session-deferred session))
    (setf (claude-code-ide-mcp-session-last-selection session) nil)
    ;; Stop the ping timer for this session
    (claude-code-ide-mcp--stop-ping-timer session)
    (claude-code-ide-debug "Final WebSocket state: %s" (websocket-ready-state ws))
    (claude-code-ide-debug "Claude Code disconnected from MCP server for %s"
                           (file-name-nondirectory
                            (directory-file-name (claude-code-ide-mcp-session-project-dir session))))))

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
  "Invalidate the buffer-local project cache.
This should be called when the buffer's context might have changed."
  (setq claude-code-ide-mcp--buffer-project-cache nil
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
    (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project)))
      ;; Only proceed if the project has sessions
      (when (claude-code-ide-mcp--sessions-for-project project-dir)
        ;; One debounce timer per project; when it fires the selection is
        ;; fanned out to every session of the project.
        (when-let* ((timer (gethash project-dir claude-code-ide-mcp--selection-timers)))
          (cancel-timer timer))
        (let ((current-buffer (current-buffer)))
          (puthash project-dir
                   (run-with-timer claude-code-ide-mcp-selection-delay nil
                                   (lambda ()
                                     ;; Make sure we're in the right buffer context when timer fires
                                     (when (buffer-live-p current-buffer)
                                       (with-current-buffer current-buffer
                                         (claude-code-ide-mcp--send-selection-for-project project-dir)))))
                   claude-code-ide-mcp--selection-timers))))))

(defun claude-code-ide-mcp--get-current-selection ()
  "Build the current selection payload for the selection_changed notification.
Returns an alist with `text', `filePath', and `selection' keys matching
the CLI's SelectionChangedSchema."
  (let ((file-path (or (buffer-file-name) "")))
    (if (use-region-p)
        (let* ((start (region-beginning))
               (end (region-end))
               (text (buffer-substring-no-properties start end))
               (start-line (line-number-at-pos start))
               (end-line (line-number-at-pos end))
               (start-col (save-excursion
                            (goto-char start)
                            (1+ (current-column))))
               (end-col (save-excursion
                          (goto-char end)
                          (1+ (current-column)))))
          `((text . ,text)
            (filePath . ,file-path)
            (selection . ((start . ((line . ,start-line)
                                    (character . ,start-col)))
                          (end . ((line . ,end-line)
                                  (character . ,end-col)))))))
      ;; No selection - return cursor position
      (let* ((cursor-line (line-number-at-pos))
             (cursor-col (1+ (current-column))))
        `((text . "")
          (filePath . ,file-path)
          (selection . ((start . ((line . ,cursor-line)
                                  (character . ,cursor-col)))
                        (end . ((line . ,cursor-line)
                                (character . ,cursor-col))))))))))

(defun claude-code-ide-mcp--send-selection-for-project (project-dir)
  "Send current selection to every Claude instance of PROJECT-DIR.
The payload is computed once; dedupe state stays per session so a
freshly connected instance still receives state its siblings already
deduplicated."
  (remhash project-dir claude-code-ide-mcp--selection-timers)
  (let* ((sessions (claude-code-ide-mcp--sessions-for-project project-dir))
         (file-path (buffer-file-name))
         (file-in-project (and file-path
                               (string-prefix-p (expand-file-name project-dir)
                                                (expand-file-name file-path))))
         ;; The file path is part of the dedupe state: switching to another
         ;; file at identical coordinates is still a context change
         (current-state (when file-in-project
                          (let ((cursor-pos (point)))
                            (if (use-region-p)
                                (list file-path cursor-pos (region-beginning) (region-end))
                              (list file-path cursor-pos cursor-pos cursor-pos)))))
         (selection (when file-in-project
                      (claude-code-ide-mcp--get-current-selection))))
    (dolist (session sessions)
      (cond
       ;; File in project - send to each client whose state changed
       ((and file-in-project
             (claude-code-ide-mcp-session-client session))
        (unless (equal current-state
                       (claude-code-ide-mcp-session-last-selection session))
          (setf (claude-code-ide-mcp-session-last-selection session) current-state)
          (claude-code-ide-mcp--send-notification "selection_changed" selection session)))
       ;; File outside project or non-file buffer - reset selection state
       ((not file-in-project)
        (setf (claude-code-ide-mcp-session-last-selection session) nil))))))

(defun claude-code-ide-mcp--track-active-buffer ()
  "Track active buffer changes for every instance of the current project."
  (let ((current-buffer (current-buffer))
        (file-path (buffer-file-name)))
    ;; Early exit for non-file buffers
    (when file-path
      (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project)))
        (when (string-prefix-p (expand-file-name project-dir)
                               (expand-file-name file-path))
          (dolist (session (claude-code-ide-mcp--sessions-for-project project-dir))
            (when (and (claude-code-ide-mcp-session-client session)
                       (not (eq current-buffer
                                (claude-code-ide-mcp-session-last-buffer session))))
              (setf (claude-code-ide-mcp-session-last-buffer session) current-buffer)
              ;; Update MCP tools server's last active buffer
              (claude-code-ide-mcp-server-update-last-active-buffer
               (claude-code-ide-mcp-session-session-id session)
               current-buffer))))))))

;;; Public API

(defun claude-code-ide-mcp-create-session (project-dir session-id &optional instance-name)
  "Create a new MCP session for PROJECT-DIR with SESSION-ID.
INSTANCE-NAME is the user-visible instance name, or nil for the
unnamed instance.  Always creates a fresh session with its own
WebSocket server, port and lockfile; a project may own any number of
concurrent sessions.  Returns the session object."
  (claude-code-ide-debug "=== Starting MCP server ===")
  (let* ((project-dir (expand-file-name project-dir))
         (session (make-claude-code-ide-mcp-session
                   :session-id session-id
                   :instance-name instance-name
                   :project-dir project-dir
                   :last-used (float-time)
                   :deferred (make-hash-table :test 'equal)
                   :active-diffs (make-hash-table :test 'equal)
                   :original-tab (when (fboundp 'tab-bar--current-tab)
                                   (tab-bar--current-tab))))
         (server-and-port (claude-code-ide-mcp--find-free-port session))
         (server (car server-and-port))
         (port (cdr server-and-port)))

    ;; Set port and server in session
    (setf (claude-code-ide-mcp-session-port session) port
          (claude-code-ide-mcp-session-server session) server)

    ;; Anything fallible must happen BEFORE the session is registered:
    ;; a failure past registration would leak a phantom session holding
    ;; a live server that no cleanup path can ever reap
    (claude-code-ide-debug "Project directory: %s" project-dir)
    (claude-code-ide-debug "Creating lockfile for port %d" port)
    (condition-case err
        (claude-code-ide-mcp--create-lockfile port project-dir)
      ((error quit)
       (ignore-errors (websocket-server-close server))
       (signal (car err) (cdr err))))

    ;; Store session
    (puthash session-id session claude-code-ide-mcp--sessions)

    ;; Set up hooks for selection and buffer tracking (add-hook is idempotent)
    (add-hook 'post-command-hook #'claude-code-ide-mcp--track-selection)
    (add-hook 'post-command-hook #'claude-code-ide-mcp--track-active-buffer)

    (claude-code-ide-debug "MCP server ready on port %d" port)
    (claude-code-ide-debug "MCP server started on port %d for %s" port
                           (file-name-nondirectory (directory-file-name project-dir)))
    session))

(defun claude-code-ide-mcp--stop-session (session)
  "Stop SESSION: close its server, timers and lockfile, deregister it.
Deregistration and hook removal run unconditionally: a failure freeing
one resource (say, an unwritable lockfile directory) must not leave a
phantom session wedged in the registry with the global hooks alive."
  (let ((project-dir (claude-code-ide-mcp-session-project-dir session)))
    (claude-code-ide-debug "Stopping MCP session %s for %s"
                           (claude-code-ide-mcp-session-session-id session)
                           project-dir)

    (unwind-protect
        (progn
          ;; Detach the client first so a callback for an already-queued
          ;; frame finds no owner and gets ignored
          (setf (claude-code-ide-mcp-session-client session) nil)

          ;; Close server and client (best effort)
          (when-let* ((server (claude-code-ide-mcp-session-server session)))
            (ignore-errors (websocket-server-close server)))

          ;; Stop timers
          (when-let* ((ping-timer (claude-code-ide-mcp-session-ping-timer session)))
            (ignore-errors (cancel-timer ping-timer)))

          ;; Remove lockfile
          (when-let* ((port (claude-code-ide-mcp-session-port session)))
            (claude-code-ide-debug "Removing lockfile for port %d" port)
            (claude-code-ide-mcp--remove-lockfile port)))

      ;; Remove session from registry
      (remhash (claude-code-ide-mcp-session-session-id session)
               claude-code-ide-mcp--sessions)

      ;; Drop the project's selection timer when its last session is gone
      (unless (claude-code-ide-mcp--sessions-for-project project-dir)
        (when-let* ((timer (gethash project-dir claude-code-ide-mcp--selection-timers)))
          (ignore-errors (cancel-timer timer)))
        (remhash project-dir claude-code-ide-mcp--selection-timers))

      ;; Remove hooks if no more sessions
      (when (= 0 (hash-table-count claude-code-ide-mcp--sessions))
        (remove-hook 'post-command-hook #'claude-code-ide-mcp--track-selection)
        (remove-hook 'post-command-hook #'claude-code-ide-mcp--track-active-buffer)))

    (claude-code-ide-debug "MCP server stopped for %s"
                           (file-name-nondirectory (directory-file-name project-dir)))))

(defun claude-code-ide-mcp-stop-session (project-dir)
  "Stop every MCP session of PROJECT-DIR."
  (dolist (session (claude-code-ide-mcp--sessions-for-project
                    (expand-file-name project-dir)))
    (claude-code-ide-mcp--stop-session session)))

(defun claude-code-ide-mcp-stop ()
  "Stop the MCP sessions for the current project, or all of them."
  (claude-code-ide-debug "Stopping MCP server...")

  ;; Try to determine which sessions to stop
  (let ((project-dir (claude-code-ide-mcp--get-buffer-project)))
    (if project-dir
        (claude-code-ide-mcp-stop-session project-dir)
      ;; No specific project - stop all sessions (backward compatibility)
      (let ((sessions (claude-code-ide-mcp--active-sessions)))
        (if sessions
            (dolist (session sessions)
              (claude-code-ide-mcp--stop-session session))
          (claude-code-ide-debug "No MCP servers running"))))))

(defun claude-code-ide-mcp-send-at-mentioned (session)
  "Send at-mentioned notification to SESSION.
If a region is selected, send the selected lines.
Otherwise, send the current line.  Unlike selection tracking, this is
an explicit prompt insertion, so it targets exactly one instance."
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
       (lineEnd . ,end-line))
     session)))

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
  (dolist (session (claude-code-ide-mcp--active-sessions))
    (claude-code-ide-mcp--stop-session session)))

(add-hook 'kill-emacs-hook #'claude-code-ide-mcp--cleanup)

(provide 'claude-code-ide-mcp)

;;; claude-code-ide-mcp.el ends here
