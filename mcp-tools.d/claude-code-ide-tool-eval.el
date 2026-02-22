;;; claude-code-ide-tool-eval.el --- Emacs Lisp evaluation MCP tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Keywords: ai, claude, mcp, eval

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

;; This file provides an MCP tool for evaluating Emacs Lisp expressions.
;; This is more efficient than using emacsclient --eval and provides
;; better error handling and result formatting.
;;
;; Security: This tool can execute arbitrary Emacs Lisp code. Use with
;; caution and only enable when needed. Consider requiring user approval
;; via Claude Code permissions settings.

;;; Code:

(require 'claude-code-ide-mcp-server)

;;; Customization

(defcustom claude-code-ide-eval-enabled nil
  "Enable the Emacs Lisp eval MCP tool.
When nil, the tool will refuse to evaluate code.
Set to t to enable evaluation."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-eval-log t
  "Log all eval calls to the *claude-code-ide-eval-log* buffer.
This provides an audit trail of all evaluated expressions."
  :type 'boolean
  :group 'claude-code-ide)

;;; Eval Functions

(defun claude-code-ide-eval--log (expression result error)
  "Log EXPRESSION, RESULT, and ERROR to the eval log buffer."
  (when claude-code-ide-eval-log
    (with-current-buffer (get-buffer-create "*claude-code-ide-eval-log*")
      (goto-char (point-max))
      (insert (format "\n[%s]\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Expression: %s\n" expression))
      (if error
          (insert (format "Error: %s\n" error))
        (insert (format "Result: %s\n" result))))))

(defun claude-code-ide-mcp-eval (expression)
  "Evaluate EXPRESSION as Emacs Lisp and return the result.
EXPRESSION should be a string containing valid Emacs Lisp code.

Returns a formatted string with the result or error message.

Security: This function can execute arbitrary code. Only enable via
claude-code-ide-eval-enabled customization when needed."
  (claude-code-ide-mcp-server-with-session-context nil
    (if (not claude-code-ide-eval-enabled)
        "Eval is disabled. Set claude-code-ide-eval-enabled to t to enable."
      (condition-case err
          (let* ((form (read expression))
                 (result (eval form t))
                 (result-str (prin1-to-string result)))
            (claude-code-ide-eval--log expression result-str nil)
            (format "Expression: %s\nResult: %s\nType: %s"
                    expression
                    result-str
                    (type-of result)))
        (error
         (let ((error-msg (error-message-string err)))
           (claude-code-ide-eval--log expression nil error-msg)
           (format "Error evaluating expression: %s\nExpression: %s\nError: %s"
                   expression
                   error-msg
                   (prin1-to-string err))))))))

;;;###autoload
(defun claude-code-ide-eval-show-log ()
  "Show the eval log buffer."
  (interactive)
  (display-buffer (get-buffer-create "*claude-code-ide-eval-log*")))

;;;###autoload
(defun claude-code-ide-eval-clear-log ()
  "Clear the eval log buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*claude-code-ide-eval-log*")
    (erase-buffer)
    (message "Eval log cleared")))

;;;###autoload
(defun claude-code-ide-eval-toggle ()
  "Toggle eval tool enabled/disabled."
  (interactive)
  (setq claude-code-ide-eval-enabled (not claude-code-ide-eval-enabled))
  (message "Claude Code eval tool %s"
           (if claude-code-ide-eval-enabled "enabled" "disabled")))

;;; Tool Registration

;;;###autoload
(defun claude-code-ide-tool-eval-setup ()
  "Register Emacs Lisp eval MCP tool."
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-eval
   :name "claude-code-ide-mcp-eval"
   :description "Evaluate Emacs Lisp expressions and return results. More efficient than emacsclient --eval with better error handling. All evaluations are logged for audit purposes. Must be explicitly enabled via claude-code-ide-eval-enabled."
   :args '((:name "expression"
                  :type string
                  :description "Emacs Lisp expression to evaluate (as a string)"))))

(provide 'claude-code-ide-tool-eval)
;;; claude-code-ide-tool-eval.el ends here
