;;; claude-code-ide-emacs-tools.el --- Emacs MCP tools for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, claude, mcp, tools, xref, emacs

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

;; This file provides Emacs-specific MCP tools for Claude Code IDE.
;; These tools expose Emacs functionality such as xref (cross-references)
;; and project information to Claude, enabling AI-assisted code navigation
;; and understanding within the correct project context.

;;; Code:

(require 'claude-code-ide-mcp-server)
(require 'xref)
(require 'project)
(require 'cl-lib)
(require 'imenu)

;;; Tool Functions

(defun claude-code-ide-mcp-xref-find-references (identifier file-path)
  "Find references to IDENTIFIER in the current session's project.
FILE-PATH specifies which file's buffer context to use for the search.
This function uses the session context to operate in the correct project."
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
                  (let ((xref-items (xref-backend-references backend identifier-str)))
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
                      (format "No references found for '%s'" identifier-str)))))
            (error
             (format "Error searching for '%s' in %s: %s"
                     identifier-str file-path (error-message-string err)))))))))

(defun claude-code-ide-mcp-xref-find-apropos (pattern file-path)
  "Find symbols matching PATTERN across the entire project.
FILE-PATH specifies which file's buffer context to use for the search.
This function uses the session context to operate in the correct project."
  (if (not file-path)
      (error "file_path parameter is required. Please specify the file where you want to search for pattern %s" pattern)
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((target-buffer (or (find-buffer-visiting file-path)
                               (find-file-noselect file-path)))
            (pattern-str (format "%s" pattern)))
        (with-current-buffer target-buffer
          (condition-case err
              (let ((backend (xref-find-backend)))
                (cond
                 ((not backend)
                  (format "No xref backend available for %s" file-path))
                 ;; Special handling for etags without tags table
                 ((and (eq backend 'etags)
                       (not (or (and (boundp 'tags-file-name) tags-file-name
                                     (file-exists-p tags-file-name))
                                (and (boundp 'tags-table-list) tags-table-list
                                     (cl-some #'file-exists-p tags-table-list)))))
                  (format "No tags table available for %s" file-path))
                 (t
                  (let ((xref-items (xref-backend-apropos backend pattern-str)))
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
                      (format "No symbols found matching pattern '%s'" pattern-str))))))
            (error
             (format "Error searching for pattern '%s' in %s: %s"
                     pattern-str file-path (error-message-string err)))))))))

(defun claude-code-ide-mcp-project-info ()
  "Get information about the current session's project.
Returns project directory, active buffer, and file count."
  (let ((context (claude-code-ide-mcp-server-get-session-context)))
    (if context
        (let ((project-dir (plist-get context :project-dir))
              (buffer (plist-get context :buffer)))
          (format "Project: %s\nBuffer: %s\nFiles: %d"
                  project-dir
                  (if (and buffer (buffer-live-p buffer))
                      (buffer-name buffer)
                    "No active buffer")
                  (length (project-files (project-current nil project-dir)))))
      "No session context available")))

(defun claude-code-ide-mcp-imenu-list-symbols (file-path)
  "List all symbols in FILE-PATH using imenu.
Returns a list of symbols with their types and positions."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              ;; Generate or update imenu index
              (imenu--make-index-alist)
              (if imenu--index-alist
                  (let ((results '()))
                    ;; Process the imenu index
                    (dolist (item imenu--index-alist)
                      (cond
                       ;; Skip special entries
                       ((string-match-p "^\\*" (car item)) nil)
                       ;; Handle simple entries (name . position)
                       ((markerp (cdr item))
                        (let ((line (line-number-at-pos (marker-position (cdr item)))))
                          (push (format "%s:%d: %s"
                                        file-path
                                        line
                                        (car item))
                                results)))
                       ;; Handle position numbers
                       ((numberp (cdr item))
                        (let ((line (line-number-at-pos (cdr item))))
                          (push (format "%s:%d: %s"
                                        file-path
                                        line
                                        (car item))
                                results)))
                       ;; Handle nested entries (category . items)
                       ((listp (cdr item))
                        (let ((category (car item)))
                          (dolist (subitem (cdr item))
                            (when (and (consp subitem)
                                       (or (markerp (cdr subitem))
                                           (numberp (cdr subitem))))
                              (let ((line (line-number-at-pos
                                           (if (markerp (cdr subitem))
                                               (marker-position (cdr subitem))
                                             (cdr subitem)))))
                                (push (format "%s:%d: [%s] %s"
                                              file-path
                                              line
                                              category
                                              (car subitem))
                                      results))))))))
                    (if results
                        (nreverse results)
                      (format "No symbols found in %s" file-path)))
                (format "No imenu support or no symbols found in %s" file-path))))
        (error
         (format "Error listing symbols in %s: %s"
                 file-path (error-message-string err)))))))

;;; Tool Configuration

(defvar claude-code-ide-emacs-tools
  '((claude-code-ide-mcp-xref-find-references
     :description "Find where a function, variable, or class is used throughout your codebase. Perfect for understanding code dependencies and impact analysis"
     :parameters ((:name "identifier"
                         :type "string"
                         :required t
                         :description "The identifier to find references for")
                  (:name "file_path"
                         :type "string"
                         :required t
                         :description "File path to use as context for the search")))

    (claude-code-ide-mcp-xref-find-apropos
     :description "Search for functions, variables, or classes by name pattern across your project. Helps you discover code elements when you know part of the name"
     :parameters ((:name "pattern"
                         :type "string"
                         :required t
                         :description "The pattern to search for symbols")
                  (:name "file_path"
                         :type "string"
                         :required t
                         :description "File path to use as context for the search")))

    (claude-code-ide-mcp-project-info
     :description "Get quick overview of your current project context including directory, active file, and project size"
     :parameters nil)

    (claude-code-ide-mcp-imenu-list-symbols
     :description "Navigate and explore a file's structure by listing all its functions, classes, and variables with their locations"
     :parameters ((:name "file_path"
                         :type "string"
                         :required t
                         :description "Path to the file to analyze for symbols"))))
  "Emacs MCP tools configuration.")

;;; Helper Functions

(defun claude-code-ide-emacs-tools-get-all-names ()
  "Get a list of all Emacs MCP tool names in the format expected by --allowedTools."
  (mapcar (lambda (tool-spec)
            (let* ((func-symbol (car tool-spec))
                   (func-name (symbol-name func-symbol)))
              ;; Convert function name to MCP tool name format
              (concat "mcp__emacs-tools__" func-name)))
          claude-code-ide-emacs-tools))

;;; Setup Function

(defun claude-code-ide-emacs-tools-setup ()
  "Set up Emacs MCP tools for Claude Code IDE."
  (setq claude-code-ide-enable-mcp-server t)
  (setq claude-code-ide-mcp-server-tools
        (append claude-code-ide-mcp-server-tools
                claude-code-ide-emacs-tools)))

(provide 'claude-code-ide-emacs-tools)
;;; claude-code-ide-emacs-tools.el ends here
