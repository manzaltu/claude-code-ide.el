;;; claude-code-ide-diagnostics.el --- Diagnostic integration for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, claude, diagnostics, flycheck

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

;; This file provides diagnostic integration for Claude Code IDE.
;; It collects diagnostics from Flycheck and converts them to
;; VS Code format for the MCP protocol.

;;; Code:

(require 'cl-lib)
(require 'flycheck)
(require 'json)

;; Forward declarations
(declare-function claude-code-ide-mcp-session-project-dir "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-debug "claude-code-ide-debug" (format-string &rest args))

;;; Diagnostic Collection

(defun claude-code-ide-diagnostics--severity-to-vscode (severity)
  "Convert diagnostic SEVERITY to VS Code format.
Returns: 1 (Error), 2 (Warning), 3 (Information), 4 (Hint)."
  (pcase severity
    ;; Flycheck severities
    ('error 1)
    ('warning 2)
    ('info 3)
    ('hint 4)
    ;; Default
    (_ 3)))

(defun claude-code-ide-diagnostics--severity-to-string (severity)
  "Convert diagnostic SEVERITY to VS Code string format."
  (pcase severity
    ;; Flycheck severities
    ('error "Error")
    ('warning "Warning")
    ('info "Information")
    ('hint "Hint")
    ;; Default
    (_ "Information")))

(defun claude-code-ide-diagnostics--get-flycheck-diagnostics (buffer)
  "Get Flycheck diagnostics for BUFFER in VS Code format."
  (when (featurep 'flycheck)
    (with-current-buffer buffer
      (when (bound-and-true-p flycheck-mode)
        (mapcar (lambda (err)
                  `((range . ((start . ((line . ,(flycheck-error-line err))
                                        (character . ,(if (flycheck-error-column err)
                                                          (1+ (flycheck-error-column err))
                                                        1))))
                              (end . ((line . ,(or (flycheck-error-end-line err)
                                                   (flycheck-error-line err)))
                                      (character . ,(if (or (flycheck-error-end-column err)
                                                            (flycheck-error-column err))
                                                        (1+ (or (flycheck-error-end-column err)
                                                                (flycheck-error-column err)))
                                                      1))))))
                    (severity . ,(claude-code-ide-diagnostics--severity-to-string
                                  (flycheck-error-level err)))
                    (source . ,(or (flycheck-error-checker err) "flycheck"))
                    (message . ,(flycheck-error-message err))))
                flycheck-current-errors)))))


(defun claude-code-ide-diagnostics-get-all (buffer)
  "Get Flycheck diagnostics for BUFFER.
Returns diagnostics in VS Code format."
  ;; Return as vector for JSON encoding
  (or (vconcat (claude-code-ide-diagnostics--get-flycheck-diagnostics buffer))
      []))

;;; MCP Handler

(defun claude-code-ide-uri-to-file-path (uri)
  "Convert a file URI to a file path."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defun claude-code-ide-file-path-to-uri (file-path)
  "Convert a FILE-PATH to a file URI."
  (concat "file://" (url-hexify-string file-path)))

(defun claude-code-ide-diagnostics-handler (params &optional session)
  "Handle getDiagnostics tool request with PARAMS.
Optional SESSION contains the MCP session context."
  (let* ((uri (alist-get 'uri params))
         (diagnostics-by-file '())
         (project-dir (when session
                        (claude-code-ide-mcp-session-project-dir session))))
    (claude-code-ide-debug "Diagnostics handler called with URI: %s, project-dir: %s" uri project-dir)
    (if (and uri (not (string-empty-p uri)))
        ;; Get diagnostics for specific file
        (let* ((file-path (claude-code-ide-uri-to-file-path uri))
               (buffer (get-file-buffer (expand-file-name file-path))))
          (when buffer
            (let ((diags (claude-code-ide-diagnostics-get-all buffer)))
              (when (> (length diags) 0)
                (push `((uri . ,uri)
                        (diagnostics . ,diags))
                      diagnostics-by-file)))))
      ;; Get diagnostics for all files in the session's project
      (let ((buffer-count 0)
            (checked-count 0))
        (dolist (buffer (buffer-list))
          (when-let ((file (buffer-file-name buffer)))
            (setq buffer-count (1+ buffer-count))
            ;; Filter by project directory if session is available
            (when (or (not project-dir)
                      (string-prefix-p (expand-file-name project-dir)
                                       (expand-file-name file)))
              (setq checked-count (1+ checked-count))
              (claude-code-ide-debug "Checking buffer: %s" file)
              (let ((diags (claude-code-ide-diagnostics-get-all buffer)))
                (claude-code-ide-debug "Found %d diagnostics for %s" (length diags) file)
                (when (> (length diags) 0)
                  (push `((uri . ,(claude-code-ide-file-path-to-uri file))
                          (diagnostics . ,diags))
                        diagnostics-by-file))))))
        (claude-code-ide-debug "Checked %d/%d buffers with files" checked-count buffer-count)))
    ;; Return JSON-encoded string in content array format
    (let ((json-str (if diagnostics-by-file
                        (json-encode (vconcat (nreverse diagnostics-by-file)))
                      "[]")))
      (list `((type . "text")
              (text . ,json-str))))))

(provide 'claude-code-ide-diagnostics)

;;; claude-code-ide-diagnostics.el ends here
