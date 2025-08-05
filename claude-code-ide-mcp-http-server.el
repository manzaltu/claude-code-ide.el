;;; claude-code-ide-mcp-http-server.el --- HTTP server for MCP tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, claude, mcp, http

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

;; This module implements the HTTP server for the MCP tools server.
;; It uses the web-server package to handle HTTP requests and implements
;; the MCP Streamable HTTP transport protocol.
;;
;; Session IDs are passed via URL paths (e.g., /mcp/session-id) to enable
;; context-aware operations for multiple concurrent Claude sessions.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'url-parse)
(require 'claude-code-ide-debug)
(require 'claude-code-ide-mcp-server)

(defvar claude-code-ide-mcp-server--current-session-id)

;; Require web-server at runtime to avoid batch mode issues
(unless (featurep 'web-server)
  (condition-case err
      (require 'web-server)
    (error
     (claude-code-ide-debug "Failed to load web-server package: %s" (error-message-string err)))))

;; Web-server declarations
(declare-function ws-process "web-server" (server))
(declare-function ws-start "web-server" (handlers port &optional log-buffer &rest network-args))
(declare-function ws-stop "web-server" (server))
(declare-function ws-send-404 "web-server" (proc &optional info))
(declare-function ws-headers "web-server" (request))
(declare-function ws-body "web-server" (request))
(declare-function ws-send "web-server" (proc msg))
(declare-function ws-response-header "web-server" (proc code &rest headers))

;;; Server State

(defvar claude-code-ide-mcp-http-server--server nil
  "The web-server instance.")


;;; Helper Functions

(defun claude-code-ide-mcp-http-server--extract-session-id-from-path (headers)
  "Extract session ID from URL path in HEADERS.
The web-server package stores the URL path in headers with the
HTTP method as the key (e.g., :POST -> \"/mcp/session-id\").
Returns the session ID or nil if not found."
  (let* ((url (or (cdr (assoc :POST headers))
                  (cdr (assoc :GET headers))))
         (_ (when url (claude-code-ide-debug "MCP request URL: %s" url))))
    ;; Match /mcp/session-id pattern
    (when (and url (string-match "^/mcp/\\([^/?]+\\)" url))
      (match-string 1 url))))

;;; Public Functions

(defun claude-code-ide-mcp-http-server-start (&optional port)
  "Start the MCP HTTP server on PORT.
If PORT is nil, a random available port is selected.
Returns a cons cell of (server . port)."
  (unless (featurep 'web-server)
    (error "web-server package is not available"))
  (claude-code-ide-debug "Attempting to start MCP server on port %s" (or port "auto"))
  (condition-case err
      (let* ((selected-port (or port 0))  ; 0 means auto-select
             (server (ws-start
                      `(((:GET . "^/mcp\\(/.*\\)?$") . ,#'claude-code-ide-mcp-http-server--handle-get)
                        ((:POST . "^/mcp\\(/.*\\)?$") . ,#'claude-code-ide-mcp-http-server--handle-post))
                      selected-port
                      nil  ; log buffer
                      :host "127.0.0.1")))  ; local only
        (setq claude-code-ide-mcp-http-server--server server)
        ;; Get the actual port from the server's process
        (let* ((process (ws-process server))
               (actual-port (process-contact process :service)))
          (claude-code-ide-debug "MCP server started on port %d" actual-port)
          ;; Set up process sentinel to detect crashes
          (set-process-sentinel process
                                (lambda (proc event)
                                  (claude-code-ide-debug "MCP server process event: %s" event)
                                  (when (string-match-p "\\(exited\\|killed\\|terminated\\)" event)
                                    (claude-code-ide-debug "MCP server died unexpectedly"))))
          (cons server actual-port)))
    (error
     (claude-code-ide-debug "Failed to start web server: %s" (error-message-string err))
     (signal 'error (list (format "Failed to start web server: %s" (error-message-string err)))))))

(defun claude-code-ide-mcp-http-server-stop (server)
  "Stop the MCP HTTP server SERVER."
  (when server
    (ws-stop server)
    (setq claude-code-ide-mcp-http-server--server nil)))

;;; Request Handlers

(defun claude-code-ide-mcp-http-server--handle-get (request)
  "Handle GET request to /mcp endpoint for SSE fallback."
  ;; For now, return 404 as we're implementing Streamable HTTP only
  ;; This could be extended to support SSE for backward compatibility
  (with-slots (process) request
    (ws-send-404 process)))

(defun claude-code-ide-mcp-http-server--handle-post (request)
  "Handle POST request to /mcp/* endpoints.
Extracts session ID from the URL path and processes the request
with the appropriate session context."
  (claude-code-ide-debug "MCP server received POST request")
  (condition-case err
      (let* ((headers (ws-headers request))
             (body (ws-body request))
             ;; Extract session ID from URL path
             (url-session-id (claude-code-ide-mcp-http-server--extract-session-id-from-path headers))
             (json-object (json-parse-string body :object-type 'alist))
             (method (alist-get 'method json-object))
             (params (alist-get 'params json-object))
             (id (alist-get 'id json-object)))

        (claude-code-ide-debug "MCP request - method: %s, id: %s, session-id: %s"
                               method id url-session-id)

        ;; Check if this is a notification (no id field)
        (if (null id)
            ;; Notifications don't require a response
            (progn
              (claude-code-ide-debug "Received notification: %s" method)
              ;; Still close the connection for HTTP transport
              (claude-code-ide-mcp-http-server--send-empty-response request))
          ;; Process the request with session context
          (let* ((claude-code-ide-mcp-server--current-session-id url-session-id)
                 (result (claude-code-ide-mcp-http-server--dispatch method params)))
            (claude-code-ide-debug "MCP response result computed")
            ;; Send response
            (claude-code-ide-mcp-http-server--send-json-response
             request 200
             `((jsonrpc . "2.0")
               (id . ,id)
               (result . ,result)))
            (claude-code-ide-debug "MCP response sent"))))

    (json-parse-error
     (claude-code-ide-mcp-http-server--send-json-error
      request nil -32700 "Parse error"))

    (quit
     (claude-code-ide-debug "Request cancelled by user (C-<escape>)")
     (claude-code-ide-mcp-http-server--send-json-error
      request nil -32001 "Operation cancelled by user"))

    (error
     (claude-code-ide-debug "Error handling request: %s"
                            (error-message-string err))
     (claude-code-ide-mcp-http-server--send-json-error
      request nil -32603 (format "Internal error: %s" (error-message-string err))))))

;;; MCP Protocol Implementation

(defun claude-code-ide-mcp-http-server--dispatch (method params)
  "Dispatch MCP method calls.
METHOD is the JSON-RPC method name.
PARAMS is the parameters alist."
  (pcase method
    ("initialize"
     (claude-code-ide-mcp-http-server--handle-initialize params))
    ("tools/list"
     (claude-code-ide-mcp-http-server--handle-tools-list params))
    ("tools/call"
     (claude-code-ide-mcp-http-server--handle-tools-call params))
    (_
     (signal 'json-rpc-error (list -32601 "Method not found")))))

(defun claude-code-ide-mcp-http-server--handle-initialize (_params)
  "Handle the initialize method."
  `((protocolVersion . "2024-11-05")
    (capabilities . ((tools . ((listChanged . :json-false)))
                     (logging . ,(make-hash-table :test 'equal))))
    (serverInfo . ((name . "claude-code-ide-mcp-tools")
                   (version . "0.1.0")))))

(defun claude-code-ide-mcp-http-server--handle-tools-list (_params)
  "Handle the tools/list method."
  (let ((tools (mapcar #'claude-code-ide-mcp-http-server--tool-to-mcp
                       claude-code-ide-mcp-server-tools)))
    (claude-code-ide-debug "MCP server returning %d tools" (length tools))
    (dolist (tool tools)
      (claude-code-ide-debug "  Tool: %s" (alist-get 'name tool)))
    `((tools . ,tools))))

(defun claude-code-ide-mcp-http-server--handle-tools-call (params)
  "Handle the tools/call method with PARAMS."
  (let* ((tool-name (alist-get 'name params))
         (tool-args (alist-get 'arguments params))
         (tool-symbol (intern tool-name))
         (tool-config (assq tool-symbol claude-code-ide-mcp-server-tools)))

    (unless tool-config
      (signal 'json-rpc-error (list -32602 (format "Unknown tool: %s" tool-name))))

    ;; Validate and extract arguments
    (let* ((param-specs (plist-get (cdr tool-config) :parameters))
           (args (claude-code-ide-mcp-http-server--validate-args tool-args param-specs)))

      ;; Call the function
      (condition-case err
          (let ((result (apply tool-symbol args)))
            `((content . (((type . "text")
                           (text . ,(claude-code-ide-mcp-http-server--format-result result)))))))
        (quit
         `((content . (((type . "text")
                        (text . "Operation cancelled by user"))))))
        (error
         `((content . (((type . "text")
                        (text . ,(format "Error: %s" (error-message-string err))))))))))))

;;; Helper Functions

(defun claude-code-ide-mcp-http-server--tool-to-mcp (tool-spec)
  "Convert TOOL-SPEC to MCP tool format."
  (let* ((name (car tool-spec))
         (plist (cdr tool-spec))
         (description (plist-get plist :description))
         (params (plist-get plist :parameters)))
    `((name . ,(symbol-name name))
      (description . ,description)
      (inputSchema . ((type . "object")
                      (properties . ,(claude-code-ide-mcp-http-server--params-to-schema params))
                      (required . ,(claude-code-ide-mcp-http-server--required-params params)))))))

(defun claude-code-ide-mcp-http-server--params-to-schema (params)
  "Convert PARAMS list to JSON Schema properties."
  (if params
      (let ((schema '()))
        (dolist (param params (nreverse schema))
          (let* ((name (plist-get param :name))
                 (type (plist-get param :type))
                 (desc (plist-get param :description))
                 (prop-schema `((type . ,type))))
            ;; Only add description if it's non-nil
            (when desc
              (setq prop-schema (append prop-schema `((description . ,desc)))))
            (push (cons (intern name) prop-schema) schema))))
    ;; Return empty hash table for no parameters (encodes as {} not [])
    (make-hash-table :test 'equal)))

(defun claude-code-ide-mcp-http-server--required-params (params)
  "Extract required parameter names from PARAMS."
  (let ((required '()))
    (dolist (param params)
      (when (plist-get param :required)
        (push (plist-get param :name) required)))
    ;; Return as a vector (JSON array) to ensure proper encoding
    (vconcat (nreverse required))))

(defun claude-code-ide-mcp-http-server--validate-args (args param-specs)
  "Validate and extract ARGS according to PARAM-SPECS.
Returns a list of arguments in the correct order."
  (let ((result '()))
    (dolist (spec param-specs (nreverse result))
      (let* ((name (plist-get spec :name))
             (required (plist-get spec :required))
             (value (alist-get (intern name) args)))
        (when (and required (not value))
          (signal 'json-rpc-error
                  (list -32602 (format "Missing required parameter: %s" name))))
        (push value result)))))

(defun claude-code-ide-mcp-http-server--format-result (result)
  "Format RESULT for display to Claude."
  (cond
   ((stringp result) result)
   ((listp result)
    (mapconcat (lambda (item)
                 (format "%s" item))
               result "\n"))
   (t (format "%s" result))))

(defun claude-code-ide-mcp-http-server--send-json-response (request status body)
  "Send JSON response to REQUEST with STATUS and BODY."
  (with-slots (process) request
    (let ((headers (list (cons "Content-Type" "application/json")
                         (cons "Access-Control-Allow-Origin" "*"))))
      (apply #'ws-response-header process status headers)
      (ws-send process (json-encode body))
      ;; Close the connection after sending response
      (throw 'close-connection nil))))

(defun claude-code-ide-mcp-http-server--send-empty-response (request)
  "Send an empty HTTP 200 response for notifications."
  (with-slots (process) request
    (ws-response-header process 200
                        (cons "Content-Type" "text/plain")
                        (cons "Content-Length" "0"))
    ;; Close the connection
    (throw 'close-connection nil)))

(defun claude-code-ide-mcp-http-server--send-json-error (request id code message)
  "Send JSON-RPC error response."
  (claude-code-ide-mcp-http-server--send-json-response
   request 200
   `((jsonrpc . "2.0")
     (id . ,id)
     (error . ((code . ,code)
               (message . ,message))))))

(provide 'claude-code-ide-mcp-http-server)
;;; claude-code-ide-mcp-http-server.el ends here
