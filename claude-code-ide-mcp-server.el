;;; claude-code-ide-mcp-server.el --- MCP tools server for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, claude, mcp, tools

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

;; This module provides an MCP tools server that exposes Emacs functions
;; to Claude Code.  Unlike the IDE MCP server (which uses WebSocket),
;; this uses HTTP/Streamable HTTP transport and provides access to
;; general Emacs functionality like xref, project navigation, etc.
;;
;; The server lifecycle is managed automatically:
;; - Starts when the first Claude session begins
;; - Stops when the last Claude session ends

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'claude-code-ide-debug)

(declare-function ws-process "web-server" (server))

;;; Customization

(defgroup claude-code-ide-mcp-server nil
  "MCP tools server settings for Claude Code IDE."
  :group 'claude-code-ide
  :prefix "claude-code-ide-mcp-server-")

(defcustom claude-code-ide-enable-mcp-server nil
  "Enable MCP tools server for exposing Emacs functions to Claude.
When enabled, a separate MCP server will be started to provide
Claude with access to configured Emacs functions."
  :type 'boolean
  :group 'claude-code-ide-mcp-server)

(defcustom claude-code-ide-mcp-server-port nil
  "Port for the MCP tools server.
If nil, a random available port will be selected automatically."
  :type '(choice (const :tag "Auto-select" nil)
                 (integer :tag "Fixed port"))
  :group 'claude-code-ide-mcp-server)

(defcustom claude-code-ide-mcp-server-tools nil
  "Alist of Emacs functions to expose via MCP tools server.
Each entry is (FUNCTION . PLIST) where PLIST contains:
  :description - Human-readable description of the function
  :parameters - List of parameter specifications, each with:
    :name - Parameter name
    :type - Parameter type (string, number, boolean)
    :required - Whether parameter is required
    :description - Parameter description"
  :type '(alist :key-type symbol
                :value-type (plist :key-type keyword
                                   :value-type sexp))
  :group 'claude-code-ide-mcp-server)

;;; State Management

(defvar claude-code-ide-mcp-server--server nil
  "The MCP tools server process.")

(defvar claude-code-ide-mcp-server--port nil
  "The port the MCP tools server is running on.")

(defvar claude-code-ide-mcp-server--session-count 0
  "Number of active Claude sessions using the MCP tools server.")

;;; Public Functions

(defun claude-code-ide-mcp-server-ensure-server ()
  "Ensure the MCP tools server is running.
Starts the server if not already running.
Returns the port number on success, nil on failure."
  (when claude-code-ide-enable-mcp-server
    (unless (and claude-code-ide-mcp-server--server
                 claude-code-ide-mcp-server--port
                 (claude-code-ide-mcp-server--server-alive-p))
      (claude-code-ide-mcp-server--start-server))
    claude-code-ide-mcp-server--port))

(defun claude-code-ide-mcp-server-get-port ()
  "Get the port number of the running MCP tools server.
Returns nil if server is not running."
  (when (and claude-code-ide-mcp-server--server
             claude-code-ide-mcp-server--port
             (claude-code-ide-mcp-server--server-alive-p))
    claude-code-ide-mcp-server--port))

(defun claude-code-ide-mcp-server-session-started ()
  "Notify that a Claude session has started.
Increments the session counter."
  (cl-incf claude-code-ide-mcp-server--session-count)
  (claude-code-ide-debug "MCP session started. Count: %d"
                         claude-code-ide-mcp-server--session-count))

(defun claude-code-ide-mcp-server-session-ended ()
  "Notify that a Claude session has ended.
Decrements the session counter and stops server if no sessions remain."
  (when (> claude-code-ide-mcp-server--session-count 0)
    (cl-decf claude-code-ide-mcp-server--session-count)
    (claude-code-ide-debug "MCP session ended. Count: %d"
                           claude-code-ide-mcp-server--session-count)
    (when (= claude-code-ide-mcp-server--session-count 0)
      (claude-code-ide-mcp-server--stop-server))))

(defun claude-code-ide-mcp-server-get-config ()
  "Get the MCP configuration for the tools server.
Returns an alist suitable for JSON encoding."
  (when-let ((port (claude-code-ide-mcp-server-get-port)))
    `((mcpServers . ((emacs-tools . ((type . "http")
                                     (url . ,(format "http://localhost:%d/mcp" port)))))))))

;;; Internal Functions

(defun claude-code-ide-mcp-server--server-alive-p ()
  "Check if the MCP tools server is still alive."
  (when claude-code-ide-mcp-server--server
    (condition-case nil
        (let ((process (ws-process claude-code-ide-mcp-server--server)))
          (and process (process-live-p process)))
      (error nil))))

(defun claude-code-ide-mcp-server--start-server ()
  "Start the MCP HTTP server."
  (condition-case err
      (progn
        ;; Load the server implementation
        (require 'claude-code-ide-mcp-http-server)
        ;; Check if web-server is available
        (unless (featurep 'web-server)
          (error "The web-server package is required for MCP tools support. Please install it with: M-x package-install RET web-server RET"))
        ;; Start the server
        (let ((result (claude-code-ide-mcp-http-server-start
                       claude-code-ide-mcp-server-port)))
          (setq claude-code-ide-mcp-server--server (car result)
                claude-code-ide-mcp-server--port (cdr result))
          claude-code-ide-mcp-server--port))
    (error
     (claude-code-ide-debug "Failed to start MCP server: %s"
                            (error-message-string err))
     (message "Warning: Failed to start MCP server: %s"
              (error-message-string err))
     nil)))

(defun claude-code-ide-mcp-server--stop-server ()
  "Stop the MCP HTTP server."
  (when claude-code-ide-mcp-server--server
    (condition-case err
        (progn
          (claude-code-ide-mcp-http-server-stop claude-code-ide-mcp-server--server)
          (setq claude-code-ide-mcp-server--server nil
                claude-code-ide-mcp-server--port nil)
          (claude-code-ide-debug "MCP server stopped"))
      (error
       (claude-code-ide-debug "Error stopping MCP server: %s"
                              (error-message-string err))))))

(provide 'claude-code-ide-mcp-server)
;;; claude-code-ide-mcp-server.el ends here
