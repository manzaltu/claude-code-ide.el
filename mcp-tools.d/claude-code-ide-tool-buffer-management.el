;;; claude-code-ide-tool-buffer-management.el --- Buffer management MCP tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author (this file only): John D. Blair
;; Keywords: ai, claude, mcp, buffer

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

;; This file provides MCP tools for buffer management operations:
;; - Listing open buffers
;; - Reading buffer contents
;; - Navigating to specific locations
;; - Reloading buffers from disk

;;; Code:

(require 'claude-code-ide-mcp-server)

;;; Buffer Management Functions

(defun claude-code-ide-mcp-list-buffers (&optional include-internal)
  "List all open buffers with their key properties.
Returns a list of buffers with name, file path, modified status, and major mode.
If INCLUDE-INTERNAL is non-nil, include internal buffers (those starting with space)."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((buffers (buffer-list))
          (current-buf (current-buffer))
          (results '()))
      (dolist (buf buffers)
        (let ((buf-name (buffer-name buf)))
          ;; Skip internal buffers unless requested
          (unless (and (not include-internal)
                       (string-prefix-p " " buf-name))
            (with-current-buffer buf
              (let* ((file-path (buffer-file-name))
                     (modified (buffer-modified-p))
                     (mode (symbol-name major-mode))
                     (is-current (eq buf current-buf))
                     (size (buffer-size)))
                (push (format "%s%s%s | %s | %s | %d bytes%s"
                              (if is-current "* " "  ")
                              buf-name
                              (if modified " [modified]" "")
                              mode
                              (if file-path
                                  (abbreviate-file-name file-path)
                                "(no file)")
                              size
                              (if file-path
                                  (format " | %s" file-path)
                                ""))
                      results))))))
      (if results
          (string-join (nreverse results) "\n")
        "No buffers open"))))

(defun claude-code-ide-mcp-read-buffer (buffer-name &optional start-line end-line)
  "Read contents from BUFFER-NAME.
BUFFER-NAME can be a buffer name or a file path.
If START-LINE and END-LINE are provided, read only those lines (1-based).
Negative values count from the end (e.g., -15 means last 15 lines).
If only START-LINE is negative, reads that many lines from the end.
If both are nil, reads the entire buffer."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((buffer (or (get-buffer buffer-name)
                          (find-buffer-visiting buffer-name))))
          (if (not buffer)
              (format "Buffer not found: %s" buffer-name)
            (with-current-buffer buffer
              (let* ((total-lines (count-lines (point-min) (point-max)))
                     ;; Handle negative start-line (e.g., -15 = last 15 lines)
                     (start (cond ((not start-line) 1)
                                  ((and (< start-line 0) (not end-line))
                                   ;; -15 with no end means "last 15 lines"
                                   (max 1 (+ total-lines start-line 1)))
                                  ((< start-line 0)
                                   ;; -15 with end means "15 from end to end"
                                   (max 1 (+ total-lines start-line 1)))
                                  (t start-line)))
                     (end (cond ((not end-line) total-lines)
                                ((< end-line 0) (max 1 (+ total-lines end-line 1)))
                                (t end-line))))
                ;; Validate range
                (when (> start end)
                  (error "Invalid range: start-line (%d) > end-line (%d)" start end))
                (when (> start total-lines)
                  (error "start-line (%d) exceeds total lines (%d)" start total-lines))
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- start))
                  (let ((start-pos (point)))
                    (forward-line (- end start))
                    (end-of-line)
                    (let ((content (buffer-substring-no-properties start-pos (point))))
                      (format "Buffer: %s (lines %d-%d of %d)\n%s"
                              buffer-name
                              start
                              end
                              total-lines
                              content))))))))
      (error
       (format "Error reading buffer: %s" (error-message-string err))))))

(defun claude-code-ide-mcp-goto-location (file-path line &optional column highlight)
  "Jump to a specific line and column in a file.
FILE-PATH is the absolute path to the file.
LINE is the line number (1-based).
COLUMN is the column number (0-based, optional).
HIGHLIGHT temporarily highlights the line if non-nil (optional)."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (progn
          ;; Open the file
          (find-file (expand-file-name file-path))
          ;; Go to the specified line
          (goto-char (point-min))
          (forward-line (1- line))
          ;; Go to column if specified
          (when column
            (move-to-column column))
          ;; Recenter to make sure the location is visible
          (recenter)
          ;; Highlight if requested
          (when highlight
            (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
              (overlay-put overlay 'face 'highlight)
              (run-with-timer 0.5 nil (lambda () (delete-overlay overlay)))))
          ;; Return success message
          (format "Jumped to %s:%d%s"
                  file-path
                  line
                  (if column (format ":%d" column) "")))
      (error
       (format "Failed to goto location: %s" (error-message-string err))))))

(defun claude-code-ide-mcp-reload-buffer (file-path)
  "Reload a buffer from disk, syncing with external file modifications.
FILE-PATH is the absolute path to the file to reload."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((expanded-path (expand-file-name file-path))
               (buffer (find-buffer-visiting expanded-path)))
          (if buffer
              (progn
                (with-current-buffer buffer
                  ;; Revert buffer from disk without confirmation
                  ;; Args: ignore-auto noconfirm preserve-modes
                  (revert-buffer t t t))
                (format "Buffer reloaded from disk: %s" file-path))
            ;; Buffer not currently open - that's fine, just report it
            (format "Buffer not open (no reload needed): %s" file-path)))
      (error
       (format "Failed to reload buffer: %s" (error-message-string err))))))

;;; Tool Registration

;;;###autoload
(defun claude-code-ide-tool-buffer-management-setup ()
  "Register buffer management MCP tools."
  ;; Register list buffers tool
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-list-buffers
   :name "claude-code-ide-mcp-list-buffers"
   :description "List all open buffers with their properties including name, file path, modified status, major mode, and size. Shows which buffer is currently active with an asterisk (*)"
   :args '((:name "include_internal"
                  :type boolean
                  :description "Include internal buffers (those starting with space)"
                  :optional t)))

  ;; Register goto-location tool
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-goto-location
   :name "claude-code-ide-mcp-goto-location"
   :description "Jump to a specific line and column in a file, optionally highlighting the location. Use this after finding diagnostics or search results to navigate the user's editor to the relevant location."
   :args '((:name "file_path"
                  :type string
                  :description "Absolute path to the file")
           (:name "line"
                  :type number
                  :description "Line number (1-based)")
           (:name "column"
                  :type number
                  :description "Column number (0-based)"
                  :optional t)
           (:name "highlight"
                  :type boolean
                  :description "Whether to temporarily highlight the line"
                  :optional t)))

  ;; Register reload-buffer tool
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-reload-buffer
   :name "claude-code-ide-mcp-reload-buffer"
   :description "Reload a buffer from disk to sync with external file modifications. Use this after Edit, Write, or shell commands modify files to ensure the user's editor shows the updated content."
   :args '((:name "file_path"
                  :type string
                  :description "Absolute path to the file to reload")))

  ;; Register read-buffer tool
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-read-buffer
   :name "claude-code-ide-mcp-read-buffer"
   :description "Read contents from an arbitrary buffer (including terminal buffers). Can read entire buffer or specific line ranges. Supports negative line numbers to read from the end (e.g., -15 for last 15 lines)."
   :args '((:name "buffer_name"
                  :type string
                  :description "Buffer name or file path")
           (:name "start_line"
                  :type number
                  :description "Starting line number (1-based). Use negative values to count from end (e.g., -15 for last 15 lines)."
                  :optional t)
           (:name "end_line"
                  :type number
                  :description "Ending line number (1-based). Use negative values to count from end."
                  :optional t))))

(provide 'claude-code-ide-tool-buffer-management)
;;; claude-code-ide-tool-buffer-management.el ends here
