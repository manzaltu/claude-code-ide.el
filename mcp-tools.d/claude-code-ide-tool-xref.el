;;; claude-code-ide-tool-xref.el --- LSP-aware xref MCP tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Keywords: ai, claude, mcp, xref, lsp

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

;; This file provides LSP-aware xref functionality for Claude Code IDE.
;; The LSP-aware version uses position-based symbol resolution, which is
;; required for LSP backends (like pylsp, rust-analyzer, etc.) that need
;; buffer position context to properly resolve symbols.
;;
;; For etags/ctags backends, use the standard xref-find-references instead.

;;; Code:

(require 'claude-code-ide-mcp-server)
(require 'xref)

;;; LSP-Aware XRef Functions

(defun claude-code-ide-mcp-xref-find-references-lsp (identifier file-path)
  "LSP-aware version: Find references to IDENTIFIER using position-based resolution.
FILE-PATH specifies which file's buffer context to use for the search.

This version works with LSP backends that require position context to resolve
symbols. It searches for the identifier in the buffer to get its position,
then uses xref-backend-identifier-at-point to properly resolve it before
finding references.

For etags/ctags backends, use claude-code-ide-mcp-xref-find-references instead."
  (if (not file-path)
      (error "file_path parameter is required. Please specify the file where you want to search for %s" identifier)
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((target-buffer (or (find-buffer-visiting file-path)
                               (find-file-noselect file-path)))
            (identifier-str (format "%s" identifier)))
        (with-current-buffer target-buffer
          (condition-case err
              (let ((backend (xref-find-backend)))
                (if (not backend)
                    (format "No xref backend available for %s" file-path)
                  ;; Search for the identifier in the buffer to get position context
                  (save-excursion
                    (goto-char (point-min))
                    (let ((found-pos nil))
                      ;; Try to find the identifier as a symbol in the buffer
                      (while (and (not found-pos)
                                  (search-forward identifier-str nil t))
                        ;; Check if this is at a symbol boundary
                        (when (and (or (eolp) (not (looking-at "\\sw\\|\\s_")))
                                   (save-excursion
                                     (backward-char (length identifier-str))
                                     (or (bolp) (not (looking-back "\\sw\\|\\s_" 1)))))
                          (setq found-pos (- (point) (length identifier-str)))))

                      (if (not found-pos)
                          (format "Unable to find symbol '%s' in %s. Try navigating to the symbol first."
                                  identifier-str file-path)
                        ;; Move to the found position
                        (goto-char found-pos)
                        ;; Use xref-backend-identifier-at-point to get proper identifier
                        (let* ((id (xref-backend-identifier-at-point backend))
                               (xref-items (when id
                                             (xref-backend-references backend id))))
                          (if xref-items
                              (mapcar (lambda (item)
                                        (let* ((location (xref-item-location item))
                                               (file (xref-location-group location))
                                               (marker (xref-location-marker location))
                                               (line (with-current-buffer (marker-buffer marker)
                                                       (save-excursion
                                                         (goto-char marker)
                                                         (line-number-at-pos))))
                                               (summary (xref-item-summary item)))
                                          (format "%s:%d: %s" file line summary)))
                                      xref-items)
                            (format "No references found for '%s'" identifier-str))))))))
            (error
             (format "Error searching for '%s' in %s: %s"
                     identifier-str file-path (error-message-string err)))))))))

;;; Tool Registration

;;;###autoload
(defun claude-code-ide-tool-xref-setup ()
  "Register LSP-aware xref MCP tool."
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-xref-find-references-lsp
   :name "claude-code-ide-mcp-xref-find-references-lsp"
   :description "Find where a function, variable, or class is used (LSP-aware version). Uses position-based symbol resolution for LSP backends like pylsp, rust-analyzer, etc. Works better than the standard version when using Language Server Protocol."
   :args '((:name "identifier"
                  :type string
                  :description "The identifier to find references for")
           (:name "file_path"
                  :type string
                  :description "File path to use as context for the search"))))

(provide 'claude-code-ide-tool-xref)
;;; claude-code-ide-tool-xref.el ends here
