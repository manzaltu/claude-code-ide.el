;;; claude-code-ide-gptel-compat.el --- Gptel/LLM compatibility for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Keywords: ai, claude, mcp, tools, gptel, llm, compatibility

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

;; This file provides compatibility functions for using gptel and llm tool
;; definitions with Claude Code IDE. It allows you to easily register tools
;; that were originally designed for gptel or llm with claude-code-ide.
;;
;; This enables reuse of existing tool libraries like:
;; - https://github.com/aard-fi/gptel-tool-library
;; - Other gptel/llm tool collections
;;
;; Usage:
;; (require 'claude-code-ide-gptel-compat)
;; (claude-code-ide-gptel-compat-register-tool my-gptel-tool)
;; (claude-code-ide-gptel-compat-register-tools my-gptel-tools-list)

;;; Code:

(require 'claude-code-ide-emacs-tools)

;;; Gptel/LLM Tool Format Support

(defun claude-code-ide-gptel-compat-register-tool (tool)
  "Register a GPTEL or LLM TOOL with Claude Code IDE.
TOOL should be a plist with the following keys:
  :name - Tool name (string)
  :description - Tool description (string)
  :parameters - List of parameter specifications
  :function - Function symbol to call
  :categories - Optional list of categories (gptel only)

Each parameter in :parameters should be a plist with:
  :name - Parameter name (string)
  :type - Parameter type (string, number, boolean, array, object)
  :required - Whether parameter is required (boolean)
  :description - Parameter description (string)

Example:
(claude-code-ide-gptel-compat-register-tool
 '(:name \"search-files\"
   :description \"Search for files in the current project\"
   :parameters ((:name \"pattern\"
                 :type \"string\"
                 :required t
                 :description \"Search pattern\"))
   :function my-search-function))"
  (claude-code-ide-emacs-tools-register-gptel-tool tool))

(defun claude-code-ide-gptel-compat-register-tools (tools)
  "Register multiple GPTEL or LLM TOOLS with Claude Code IDE.
TOOLS should be a list of tool plists, each following the format
described in `claude-code-ide-gptel-compat-register-tool'."
  (claude-code-ide-emacs-tools-register-gptel-tools tools))

(defun claude-code-ide-gptel-compat-register-llm-tool (tool)
  "Register an LLM TOOL with Claude Code IDE.
This is an alias for `claude-code-ide-gptel-compat-register-tool'
since LLM and gptel use the same format."
  (claude-code-ide-gptel-compat-register-tool tool))

(defun claude-code-ide-gptel-compat-register-llm-tools (tools)
  "Register multiple LLM TOOLS with Claude Code IDE.
This is an alias for `claude-code-ide-gptel-compat-register-tools'
since LLM and gptel use the same format."
  (claude-code-ide-gptel-compat-register-tools tools))

;;; Convenience Functions for Common Tool Libraries

(defun claude-code-ide-gptel-compat-register-from-library (library-path)
  "Register tools from a gptel tool library at LIBRARY-PATH.
LIBRARY-PATH should be the path to a file containing gptel tool definitions.
This function will load the file and register any tools it finds."
  (interactive "fLoad gptel tool library from: ")
  (let ((tools '()))
    (load library-path)
    ;; Look for common variable names that might contain tools
    (dolist (var-name '("gptel-tools" "gptel-tool-library" "tools"))
      (when (boundp (intern var-name))
        (let ((var-value (symbol-value (intern var-name))))
          (when (listp var-value)
            (setq tools (append tools var-value))))))
    (if tools
        (progn
          (claude-code-ide-gptel-compat-register-tools tools)
          (message "Registered %d tools from %s" (length tools) library-path))
      (message "No tools found in %s" library-path))))

(defun claude-code-ide-gptel-compat-register-from-gptel-tool-library ()
  "Register tools from the gptel-tool-library if available.
This function attempts to load and register tools from the
gptel-tool-library package if it's installed."
  (interactive)
  (condition-case err
      (progn
        (require 'gptel-tool-library nil t)
        (when (boundp 'gptel-tool-library)
          (let ((tools (symbol-value 'gptel-tool-library)))
            (if tools
                (progn
                  (claude-code-ide-gptel-compat-register-tools tools)
                  (message "Registered %d tools from gptel-tool-library" (length tools)))
              (message "No tools found in gptel-tool-library"))))
        (message "gptel-tool-library not available"))
    (error
     (message "Error loading gptel-tool-library: %s" (error-message-string err)))))

;;; Example Tools

(defun claude-code-ide-gptel-compat-example-tools ()
  "Register example tools in gptel/llm format.
This function demonstrates how to register tools using the gptel/llm format."
  (let ((example-tools
         '((:name "search-files"
            :description "Search for files in the current project"
            :parameters ((:name "pattern"
                          :type "string"
                          :required t
                          :description "Search pattern to match filenames"))
            :function claude-code-ide-gptel-compat--search-files)
           (:name "count-lines"
            :description "Count lines in a file"
            :parameters ((:name "file"
                          :type "string"
                          :required t
                          :description "Path to the file to count lines in"))
            :function claude-code-ide-gptel-compat--count-lines))))
    (claude-code-ide-gptel-compat-register-tools example-tools)
    (message "Registered %d example tools" (length example-tools))))

;;; Example Tool Implementations

(defun claude-code-ide-gptel-compat--search-files (pattern)
  "Search for files matching PATTERN in the current project."
  (claude-code-ide-mcp-server-with-session-context nil
    (let* ((project-dir default-directory)
           (files (project-files (project-current nil project-dir)))
           (matching-files (cl-remove-if-not
                           (lambda (file)
                             (string-match-p pattern (file-name-nondirectory file)))
                           files)))
      (if matching-files
          (mapconcat 'identity matching-files "\n")
        (format "No files found matching pattern '%s'" pattern)))))

(defun claude-code-ide-gptel-compat--count-lines (file)
  "Count lines in FILE."
  (condition-case err
      (let ((buffer (find-file-noselect file)))
        (with-current-buffer buffer
          (format "File %s has %d lines" file (count-lines (point-min) (point-max)))))
    (error
     (format "Error counting lines in %s: %s" file (error-message-string err)))))

;;; Setup Function

;;;###autoload
(defun claude-code-ide-gptel-compat-setup ()
  "Set up gptel/llm compatibility for Claude Code IDE.
This function enables the compatibility layer and registers any
pre-configured tools."
  (interactive)
  ;; Ensure emacs-tools are set up
  (claude-code-ide-emacs-tools-setup)
  (message "Gptel/LLM compatibility layer enabled for Claude Code IDE"))

(provide 'claude-code-ide-gptel-compat)
;;; claude-code-ide-gptel-compat.el ends here
