;;; claude-code-ide-debug.el --- Debug logging for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Keywords: ai, claude, mcp, debug

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

;; This file provides debug logging functionality for Claude Code IDE.
;; It supports structured logging of WebSocket messages, JSON-RPC
;; communication, and general debug information with session context.

;;; Code:

(require 'json)
(require 'project)

;;; Customization

(defcustom claude-code-ide-debug nil
  "When non-nil, enable debug logging for Claude Code IDE."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-log-with-context t
  "When non-nil, include session context in log messages."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-debug-buffer "*claude-code-ide-debug*"
  "Buffer name for debug output."
  :type 'string
  :group 'claude-code-ide)

;;; Debug Functions

(defun claude-code-ide--get-session-context ()
  "Get current session context for logging."
  (if claude-code-ide-log-with-context
      (format "[%s]" (or (ignore-errors
                           (file-name-nondirectory
                            (directory-file-name
                             (project-root (project-current)))))
                         "no-project"))
    ""))

(defmacro claude-code-ide-debug (format-string &rest args)
  "Log debug message with FORMAT-STRING and ARGS if debug is enabled.
This is a macro to avoid evaluating ARGS when debugging is disabled."
  `(when claude-code-ide-debug
     (let ((message (format ,format-string ,@args))
           (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
           (context (claude-code-ide--get-session-context)))
       (with-current-buffer (get-buffer-create claude-code-ide-debug-buffer)
         (goto-char (point-max))
         (insert (format "%s %s%s\n" timestamp context message))))))

(defun claude-code-ide-log (format-string &rest args)
  "Log message with FORMAT-STRING and ARGS."
  (let ((message (apply #'format format-string args)))
    (message "%s %s" (claude-code-ide--get-session-context) message)))

;;;###autoload
(defun claude-code-ide-show-debug ()
  "Show the debug buffer."
  (interactive)
  (display-buffer (get-buffer-create claude-code-ide-debug-buffer)))

;;;###autoload
(defun claude-code-ide-clear-debug ()
  "Clear the debug buffer."
  (interactive)
  (with-current-buffer (get-buffer-create claude-code-ide-debug-buffer)
    (erase-buffer)
    (message "Debug buffer cleared")))

(provide 'claude-code-ide-debug)

;;; claude-code-ide-debug.el ends here
