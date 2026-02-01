;;; claude-code-ide-mcp-handlers.el --- MCP tool handlers for Claude Code IDE  -*- lexical-binding: t; -*-

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

;; This file contains the MCP tool handler implementations for Claude Code IDE.
;; These handlers implement the actual functionality that Claude can invoke
;; through the MCP protocol.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ediff)
(require 'claude-code-ide-diagnostics)
(require 'claude-code-ide-debug)

(declare-function claude-code-ide-mcp-complete-deferred "claude-code-ide-mcp" (session method result &optional unique-key))
(declare-function claude-code-ide-mcp--get-current-session "claude-code-ide-mcp" ())
(declare-function claude-code-ide-mcp--get-session-for-project "claude-code-ide-mcp" (project-dir))
(declare-function claude-code-ide-mcp--get-buffer-project "claude-code-ide-mcp" ())
(declare-function claude-code-ide-mcp-session-active-diffs "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-original-tab "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-project-dir "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp--setup-buffer-cache-hooks "claude-code-ide-mcp" ())
(declare-function claude-code-ide--get-buffer-name "claude-code-ide" (&optional directory))
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide" (buffer))
(defvar ediff-control-buffer)
(defvar ediff-window-setup-function)
(defvar ediff-split-window-function)
(defvar ediff-control-buffer-suffix)
(defvar claude-code-ide-mcp--sessions)
(defvar claude-code-ide-show-claude-window-in-ediff)
(defvar claude-code-ide-focus-claude-after-ediff)
(defvar claude-code-ide-switch-tab-on-ediff)
(defvar claude-code-ide-use-ide-diff)
(defvar claude-code-ide-ediff-split-orientation)
(defvar claude-code-ide-ediff-control-panel-style)

;;; Tool Registry - Define variables first to ensure they're available

(defvar claude-code-ide-mcp-tools nil
  "Alist of tool names to handler functions.")

(defvar claude-code-ide-mcp-tool-schemas nil
  "Tool input schemas for MCP protocol.")

(defvar claude-code-ide-mcp-tool-descriptions nil
  "Tool descriptions for MCP protocol.")

;;; Constants

(defconst claude-code-ide-mcp-handlers-idle-timer-delay 0
  "Delay in seconds for idle timer when deferring responses.")

;;; Helper Functions

(defun claude-code-ide-mcp--find-session-for-file (file-path)
  "Find the MCP session that owns FILE-PATH.
Returns the session if found, nil otherwise."
  (let ((expanded-file (expand-file-name file-path))
        (found-session nil))
    (catch 'found
      (maphash (lambda (project-dir session)
                 (when (string-prefix-p (expand-file-name project-dir)
                                        expanded-file)
                   (setq found-session session)
                   (throw 'found t)))
               claude-code-ide-mcp--sessions))
    found-session))

(defun claude-code-ide-mcp--find-claude-side-window ()
  "Find the Claude Code side window in the current frame.
Returns the window if found, nil otherwise."
  (let ((claude-buffer-name (claude-code-ide--get-buffer-name)))
    (cl-find-if (lambda (window)
                  (and (window-parameter window 'window-side)
                       (equal (buffer-name (window-buffer window))
                              claude-buffer-name)))
                (window-list))))

(defun claude-code-ide-mcp--find-claude-frame (&optional session)
  "Find the frame containing the Claude Code buffer for SESSION.
Returns the frame if found, nil otherwise."
  (let* ((project-dir (if session
                          (claude-code-ide-mcp-session-project-dir session)
                        (claude-code-ide-mcp--get-buffer-project)))
         (claude-buffer-name (claude-code-ide--get-buffer-name project-dir))
         (claude-buffer (get-buffer claude-buffer-name)))
    (when (and claude-buffer (buffer-live-p claude-buffer))
      ;; Find frame containing the claude buffer
      (catch 'found
        (dolist (frame (frame-list))
          (dolist (window (window-list frame))
            (when (equal (buffer-name (window-buffer window))
                         claude-buffer-name)
              (throw 'found frame))))
        nil))))

(defun claude-code-ide-mcp--choose-ediff-frame (session)
  "Choose the appropriate frame for displaying ediff based on SESSION.
Returns a frame or nil to use the current frame."
  (when (display-graphic-p)
    ;; Only do frame management in GUI mode
    (let ((claude-frame (claude-code-ide-mcp--find-claude-frame session))
          (frame-count (length (frame-list))))
      (when claude-code-ide-show-claude-window-in-ediff
        ;; User wants to keep Claude window visible
        (cond
         ;; Only one frame - create a new frame for ediff
         ((<= frame-count 1)
          (make-frame))
         ;; Multiple frames - find a different frame than claude's
         (t
          (catch 'found
            (dolist (frame (frame-list))
              (unless (eq frame claude-frame)
                (throw 'found frame)))
            ;; Fallback: create new frame if all frames are the claude frame
            (make-frame))))))))

(defun claude-code-ide-mcp--setup-ediff-in-frame (target-frame buffer-A buffer-B setup-fn split-fn tab-name)
  "Set up ediff in TARGET-FRAME with BUFFER-A and BUFFER-B.
SETUP-FN and SPLIT-FN are the ediff configuration functions.
TAB-NAME is used for the control buffer suffix.
Returns the ediff control buffer."
  (if target-frame
      ;; Use the target frame
      (with-selected-frame target-frame
        (let ((ediff-control-buffer-suffix (format "<%s>" tab-name))
              (ediff-window-setup-function setup-fn)
              (ediff-split-window-function split-fn))
          (ediff-buffers buffer-A buffer-B)
          ediff-control-buffer))
    ;; Use current frame
    (let ((ediff-control-buffer-suffix (format "<%s>" tab-name))
          (ediff-window-setup-function setup-fn)
          (ediff-split-window-function split-fn))
      (ediff-buffers buffer-A buffer-B)
      ediff-control-buffer)))


(defun claude-code-ide-mcp--get-active-diffs (&optional session)
  "Get the active diffs hash table for the current session.
If SESSION is provided, use it instead of looking up the current session."
  (if session
      (claude-code-ide-mcp-session-active-diffs session)
    (if-let* ((current-session (claude-code-ide-mcp--get-current-session)))
        (claude-code-ide-mcp-session-active-diffs current-session)
      ;; No session found - return nil
      nil)))

(defun claude-code-ide-mcp--create-diff-buffers (old-file-path new-file-contents tab-name)
  "Create buffers for diff comparison.
OLD-FILE-PATH is the path to the original file.
NEW-FILE-CONTENTS is the new content to diff against.
TAB-NAME is used for naming the new buffer.
Returns a cons cell (buffer-A . buffer-B)."
  (let ((file-exists (file-exists-p old-file-path))
        buffer-A buffer-B)
    ;; Create or find buffer A (original file)
    (if file-exists
        ;; File exists - use it
        (setq buffer-A (find-file-noselect old-file-path))
      ;; New file - create empty buffer
      (setq buffer-A (generate-new-buffer (format "*New file: %s*"
                                                  (file-name-nondirectory old-file-path))))
      (with-current-buffer buffer-A
        (setq buffer-file-name old-file-path)))

    ;; Create buffer B (new content)
    (setq buffer-B (generate-new-buffer (format "*%s*" tab-name)))
    (with-current-buffer buffer-B
      (insert new-file-contents)
      ;; Set the mode based on the file extension
      (let ((mode (assoc-default old-file-path auto-mode-alist 'string-match)))
        (when mode
          (condition-case err
              (funcall mode)
            (error
             ;; If mode activation fails (e.g., syntax errors), use fundamental-mode
             (claude-code-ide-debug "Failed to activate %s for diff buffer: %s. Using fundamental-mode."
                                    mode (error-message-string err))
             (fundamental-mode))))))

    (cons buffer-A buffer-B)))

(defun claude-code-ide-mcp--setup-diff-hooks (tab-name session saved-winconf)
  "Set up ediff hooks for TAB-NAME with SESSION and SAVED-WINCONF.
Returns a cons cell (before-setup-hook-fn . startup-hook-fn)."
  (let* ((captured-tab-name tab-name)
         (captured-session session)
         (captured-winconf saved-winconf)
         (before-setup-hook-fn nil)
         (startup-hook-fn nil))

    ;; Define the before-setup hook function
    (setq before-setup-hook-fn
          (lambda ()
            ;; Remove this hook after use
            (remove-hook 'ediff-before-setup-hook before-setup-hook-fn)))

    ;; Define the startup hook function with lexical closure
    (setq startup-hook-fn
          (lambda ()
            (claude-code-ide-mcp--handle-ediff-startup
             captured-tab-name captured-session captured-winconf startup-hook-fn)))

    (cons before-setup-hook-fn startup-hook-fn)))

(defun claude-code-ide-mcp--handle-ediff-startup (tab-name session saved-winconf startup-hook-fn)
  "Handle ediff startup for TAB-NAME with SESSION and SAVED-WINCONF.
STARTUP-HOOK-FN is the hook function to remove after use."
  ;; Capture the control buffer and store it in diff-info
  (when ediff-control-buffer
    ;; Store the control buffer in our diff-info
    (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session))
          (control-buffer ediff-control-buffer))
      (when-let* ((diff-info (gethash tab-name active-diffs)))
        (setf (alist-get 'control-buffer diff-info) control-buffer)
        (puthash tab-name diff-info active-diffs)))

    (with-current-buffer ediff-control-buffer
      ;; Set up quit hook with captured values in lexical closure
      (setq-local ediff-quit-hook
                  (list (lambda ()
                          ;; Check if this quit was initiated by Claude
                          (let* ((active-diffs (claude-code-ide-mcp--get-active-diffs session))
                                 (diff-info (gethash tab-name active-diffs))
                                 (quit-from-claude (alist-get 'quit-from-claude diff-info)))
                            (unless quit-from-claude
                              ;; Only handle quit if not initiated by Claude
                              (claude-code-ide-mcp--handle-ediff-quit
                               tab-name
                               session)))
                          ;; Always restore window configuration
                          (when saved-winconf
                            (condition-case nil
                                (set-window-configuration saved-winconf)
                              (error nil)))))))

    ;; Jump to the first difference if there are any
    (ignore-errors (ediff-next-difference))

    ;; Get frame information from diff-info
    (let* ((active-diffs (claude-code-ide-mcp--get-active-diffs session))
           (diff-info (gethash tab-name active-diffs))
           (ediff-frame (alist-get 'ediff-frame diff-info))
           (claude-frame (alist-get 'claude-frame diff-info))
           (original-window (selected-window))
           (claude-window nil))

      ;; Restore Claude side window only if user wants it shown during ediff
      (when claude-code-ide-show-claude-window-in-ediff
        (when-let* ((project-dir (claude-code-ide-mcp-session-project-dir session))
                    (claude-buffer-name (claude-code-ide--get-buffer-name project-dir))
                    (claude-buffer (get-buffer claude-buffer-name)))
          (when (buffer-live-p claude-buffer)
            ;; If claude-frame and ediff-frame are different, display Claude in the claude-frame
            ;; Otherwise display in current frame
            (if (and claude-frame ediff-frame
                     (not (eq claude-frame ediff-frame))
                     (frame-live-p claude-frame))
                ;; Different frames - display Claude buffer in the original claude frame
                (with-selected-frame claude-frame
                  (setq claude-window (claude-code-ide--display-buffer-in-side-window claude-buffer)))
              ;; Same frame or no frame management - display normally
              (setq claude-window (claude-code-ide--display-buffer-in-side-window claude-buffer))))))

      ;; Handle frame raising and focus based on user preferences
      (cond
       ;; If user wants Claude window focus and it's visible, raise its frame and focus it
       ((and claude-code-ide-focus-claude-after-ediff claude-window)
        (when (and (display-graphic-p) claude-frame (frame-live-p claude-frame))
          (raise-frame claude-frame))
        (select-window claude-window))

       ;; Otherwise, raise the ediff frame (if different) and focus the ediff control window
       (t
        (when (and (display-graphic-p) ediff-frame (frame-live-p ediff-frame))
          (raise-frame ediff-frame))
        (select-window original-window))))

    ;; Remove this startup hook after use
    (remove-hook 'ediff-startup-hook startup-hook-fn)))

;;; Tool Handler Functions

(defun claude-code-ide-mcp-handle-open-file (arguments)
  "Open a file with optional text selection.
ARGUMENTS should contain:
- `path': File path to open
- `startLine' (optional): Start line for selection
- `endLine' (optional): End line for selection
- `startText' (optional): Start text pattern for selection
- `endText' (optional): End text pattern for selection"
  (let ((path (alist-get 'path arguments))
        (start-line (alist-get 'startLine arguments))
        (end-line (alist-get 'endLine arguments))
        (start-text (alist-get 'startText arguments))
        (end-text (alist-get 'endText arguments)))
    (unless path
      (signal 'mcp-error '("Missing required parameter: path")))
    (condition-case err
        (progn
          (find-file path)
          ;; Set up buffer cache hooks for the newly opened file
          (with-current-buffer (current-buffer)
            (claude-code-ide-mcp--setup-buffer-cache-hooks))
          ;; Text pattern selection takes precedence over line numbers
          (cond
           ;; Both start and end text patterns provided
           ((and start-text end-text)
            (goto-char (point-min))
            (if (search-forward start-text nil t)
                (let ((start-pos (match-beginning 0)))
                  (if (search-forward end-text nil t)
                      (let ((end-pos (match-end 0)))
                        (goto-char start-pos)
                        (push-mark end-pos t t)
                        (activate-mark))
                    ;; End text not found, just go to start
                    (goto-char start-pos)))
              ;; Start text not found, fall back to line numbers if provided
              (when start-line
                (goto-char (point-min))
                (forward-line (1- start-line))
                (when end-line
                  (push-mark (point) t t)
                  (forward-line (- end-line start-line))
                  (end-of-line)
                  (activate-mark)))))
           ;; Only start text provided
           (start-text
            (goto-char (point-min))
            (when (search-forward start-text nil t)
              (goto-char (match-beginning 0))))
           ;; Line number selection
           (start-line
            (goto-char (point-min))
            (forward-line (1- start-line))
            (when end-line
              (push-mark (point) t t)
              (forward-line (- end-line start-line))
              (end-of-line)
              (activate-mark))))
          ;; Return in VS Code format
          (list `((type . "text")
                  (text . "FILE_OPENED"))))
      (error
       (signal 'mcp-error (list (format "Failed to open file: %s"
                                        (error-message-string err))))))))

(defun claude-code-ide-mcp-handle-get-current-selection (_arguments)
  "Get the currently selected text and its context."
  (let ((file-path (or (buffer-file-name) ""))
        (file-url (when (buffer-file-name)
                    (concat "file://" (buffer-file-name)))))
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
            ,@(when file-url `((fileUrl . ,file-url)))
            (selection . ((start . ((line . ,start-line)
                                    (character . ,start-col)))
                          (end . ((line . ,end-line)
                                  (character . ,end-col)))
                          (isEmpty . :json-false)))))
      ;; No selection - return cursor position
      (let* ((cursor-line (line-number-at-pos))
             (cursor-col (1+ (current-column))))
        `((text . "")
          (filePath . ,file-path)
          ,@(when file-url `((fileUrl . ,file-url)))
          (selection . ((start . ((line . ,cursor-line)
                                  (character . ,cursor-col)))
                        (end . ((line . ,cursor-line)
                                (character . ,cursor-col)))
                        (isEmpty . t))))))))

(defun claude-code-ide-mcp-handle-get-open-editors (_arguments)
  "Get list of all open editors/buffers with file paths."
  (let ((editors '())
        (project-dir (claude-code-ide-mcp--get-buffer-project)))
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        ;; Only include files within the project directory
        (when (or (not project-dir)
                  (string-prefix-p (expand-file-name project-dir)
                                   (expand-file-name file)))
          (push `((path . ,file)
                  (name . ,(buffer-name buffer))
                  (active . ,(eq buffer (current-buffer)))
                  (isDirty . ,(if (buffer-modified-p buffer) t :json-false))
                  (fileUrl . ,(concat "file://" file)))
                editors))))
    `((editors . ,(vconcat (nreverse editors))))))

(defun claude-code-ide-mcp-handle-get-workspace-folders (_arguments)
  "Get the current workspace folders (project roots)."
  ;; Return the specific project directory for this MCP instance
  (let ((project-dir (or (claude-code-ide-mcp--get-buffer-project)
                         default-directory)))
    `((folders . ,(vconcat (list (expand-file-name project-dir)))))))

(defun claude-code-ide-mcp-handle-get-diagnostics (arguments &optional session)
  "Get diagnostics (errors/warnings) for the current workspace.
ARGUMENTS may contain an optional `uri' parameter.
Optional SESSION contains the MCP session context."
  (claude-code-ide-diagnostics-handler arguments session))

(defun claude-code-ide-mcp-handle-save-document (arguments)
  "Save a document.
ARGUMENTS should contain `path' of the file to save."
  (let ((path (alist-get 'path arguments)))
    (unless path
      (signal 'mcp-error '("Missing required parameter: path")))
    (condition-case err
        (let ((buffer (find-buffer-visiting path)))
          (if buffer
              (with-current-buffer buffer
                (save-buffer)
                ;; Return in VS Code format
                (list `((type . "text")
                        (text . "DOCUMENT_SAVED"))))
            (signal 'mcp-error (list (format "No buffer visiting %s" path)))))
      (error
       (signal 'mcp-error (list (format "Failed to save: %s"
                                        (error-message-string err))))))))

(defun claude-code-ide-mcp-handle-close-tab (arguments)
  "Close a tab/buffer.
ARGUMENTS should contain `path' or `tab_name' of the file to close."
  (let ((path (alist-get 'path arguments))
        (tab-name (alist-get 'tab_name arguments)))
    (cond
     (path
      (let ((buffer (find-buffer-visiting path)))
        (if buffer
            (progn
              (kill-buffer buffer)
              ;; Return in VS Code format
              (list `((type . "text")
                      (text . "TAB_CLOSED"))))
          ;; Error case
          (signal 'mcp-error (list (format "No buffer visiting %s" path))))))
     (tab-name
      ;; Check if it's a diff tab first - need to check all sessions
      (let* ((found-session nil)
             (found-diff-info nil))
        ;; Search all sessions for this diff tab
        (catch 'found
          (maphash (lambda (_proj-dir session)
                     (let* ((session-diffs (claude-code-ide-mcp-session-active-diffs session))
                            (diff-info (gethash tab-name session-diffs)))
                       (when diff-info
                         (setq found-session session
                               found-diff-info diff-info)
                         (throw 'found t))))
                   claude-code-ide-mcp--sessions))
        (if found-diff-info
            (progn
              ;; Check if ediff is still active and quit it using stored control buffer
              (when-let* ((control-buf (alist-get 'control-buffer found-diff-info)))
                (when (buffer-live-p control-buf)
                  ;; Set a flag in diff-info to indicate this quit is from Claude
                  (setf (alist-get 'quit-from-claude found-diff-info) t)
                  ;; Store all ediff buffer references in diff-info before quitting
                  (with-current-buffer control-buf
                    (when (and (boundp 'ediff-error-buffer) ediff-error-buffer)
                      (setf (alist-get 'error-buffer found-diff-info) ediff-error-buffer))
                    (when (and (boundp 'ediff-diff-buffer) ediff-diff-buffer)
                      (setf (alist-get 'diff-buffer found-diff-info) ediff-diff-buffer))
                    (when (and (boundp 'ediff-fine-diff-buffer) ediff-fine-diff-buffer)
                      (setf (alist-get 'fine-diff-buffer found-diff-info) ediff-fine-diff-buffer))
                    (when (and (boundp 'ediff-custom-diff-buffer) ediff-custom-diff-buffer)
                      (setf (alist-get 'custom-diff-buffer found-diff-info) ediff-custom-diff-buffer)))
                  (puthash tab-name found-diff-info (claude-code-ide-mcp-session-active-diffs found-session))
                  ;; Use ediff's proper quit mechanism if available
                  (condition-case err
                      (if (fboundp 'ediff-really-quit)
                          ;; Properly quit ediff which will run our quit hooks
                          (with-current-buffer control-buf
                            (ediff-really-quit nil))
                        ;; Fallback: just kill the control buffer
                        (kill-buffer control-buf))
                    (error
                     ;; If ediff-really-quit fails (e.g., side window issues),
                     ;; just kill the control buffer directly
                     (claude-code-ide-debug "Error quitting ediff: %s. Killing control buffer directly."
                                            (error-message-string err))
                     (when (buffer-live-p control-buf)
                       (kill-buffer control-buf))))))

              ;; Clean up the diff buffers with the found session
              (claude-code-ide-mcp--cleanup-diff tab-name found-session)

              ;; Return in VS Code format
              (list `((type . "text")
                      (text . "TAB_CLOSED"))))
          ;; Not a diff - treat tab_name as buffer name
          (let ((buffer (get-buffer tab-name)))
            (if buffer
                (progn
                  (kill-buffer buffer)
                  ;; Return in VS Code format
                  (list `((type . "text")
                          (text . "TAB_CLOSED"))))
              ;; Error case - VS Code returns isError: true
              (signal 'mcp-error (list (format "No buffer named %s" tab-name))))))))
     (t
      (signal 'mcp-error '("Either 'path' or 'tab_name' must be provided"))))))

(defun claude-code-ide-mcp-handle-open-diff (arguments)
  "Open a diff view using ediff.
ARGUMENTS should contain:
- `old_file_path': Original file path
- `new_file_path': New file path (usually same as old)
- `new_file_contents': New content to diff against
- `tab_name': Name for the diff tab"
  (let ((old-file-path (alist-get 'old_file_path arguments))
        (new-file-path (alist-get 'new_file_path arguments))
        (new-file-contents (alist-get 'new_file_contents arguments))
        (tab-name (alist-get 'tab_name arguments))
        session)
    ;; Validate required parameters
    (unless (and old-file-path new-file-path new-file-contents tab-name)
      (signal 'mcp-error '("Missing required parameters for openDiff")))

    ;; Try to find session based on the file being diffed first
    (setq session (or (claude-code-ide-mcp--find-session-for-file old-file-path)
                      ;; Fall back to current buffer's session
                      (claude-code-ide-mcp--get-current-session)))

    ;; Ensure we have a valid session
    (unless session
      (signal 'mcp-error '("No active MCP session found")))

    ;; Get the active diffs for this specific session
    (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session)))
      ;; Check if there's already a diff with this tab_name
      (when-let* ((existing-diff (gethash tab-name active-diffs)))
        ;; Clean up existing diff
        (claude-code-ide-mcp--cleanup-diff tab-name session)))

    ;; Switch to original tab if we're on a different one (when configured)
    (when (and claude-code-ide-switch-tab-on-ediff
               (claude-code-ide-mcp-session-original-tab session))
      (let ((original-tab (claude-code-ide-mcp-session-original-tab session)))
        (when (and (fboundp 'tab-bar-mode)
                   tab-bar-mode
                   (fboundp 'tab-bar--current-tab)
                   (fboundp 'tab-bar-select-tab-by-name))
          (let ((current-tab (tab-bar--current-tab)))
            ;; Compare tab names or indices
            (when (and original-tab current-tab
                       (not (equal (alist-get 'name original-tab)
                                   (alist-get 'name current-tab))))
              ;; Switch to the original tab
              (tab-bar-select-tab-by-name (alist-get 'name original-tab)))))))

    ;; Save current window configuration and frame
    (let* ((saved-winconf (current-window-configuration))
           (original-frame (selected-frame))  ; Capture frame BEFORE creating new frame
           (buffers (claude-code-ide-mcp--create-diff-buffers
                     old-file-path new-file-contents tab-name))
           (buffer-A (car buffers))
           (buffer-B (cdr buffers))
           (file-exists (file-exists-p old-file-path)))

      ;; Determine which frame to use for ediff AFTER capturing original frame
      (let* ((active-diffs (claude-code-ide-mcp--get-active-diffs session))
             (target-frame (claude-code-ide-mcp--choose-ediff-frame session)))

        ;; Store frame info in diff-info
        (puthash tab-name
                 `((buffer-A . ,buffer-A)
                   (buffer-B . ,buffer-B)
                   (old-file-path . ,old-file-path)
                   (new-file-path . ,new-file-path)
                   (file-exists . ,file-exists)
                   (saved-winconf . ,saved-winconf)
                   (session . ,session)  ; Store the session reference
                   (claude-frame . ,original-frame)  ; Store original frame (where Claude is)
                   (ediff-frame . ,target-frame)  ; Store which frame ediff is in
                   (created-at . ,(current-time)))
                 active-diffs)

        ;; If using a different frame, switch to it NOW before setting up ediff
        ;; This ensures ediff places all windows in the new frame
        (when target-frame
          (select-frame-set-input-focus target-frame)
          ;; Delete side windows in the new frame
          (dolist (window (window-list target-frame))
            (when (window-parameter window 'window-side)
              (delete-window window))))

        ;; Set up startup hook to configure ediff after it's fully initialized
        (let* ((hooks (claude-code-ide-mcp--setup-diff-hooks tab-name session saved-winconf))
               (before-setup-hook-fn (car hooks))
               (startup-hook-fn (cdr hooks)))

          ;; Add hooks
          (add-hook 'ediff-before-setup-hook before-setup-hook-fn)
          (add-hook 'ediff-startup-hook startup-hook-fn)

          ;; Start ediff
          (condition-case err
              (progn
                ;; Determine ediff window setup function based on control panel style
                (let ((setup-fn
                       (pcase claude-code-ide-ediff-control-panel-style
                         ('gui 'ediff-setup-windows-default)
                         ('terminal 'ediff-setup-windows-plain)
                         ('auto (if (display-graphic-p)
                                    'ediff-setup-windows-default
                                  'ediff-setup-windows-plain))
                         (_ 'ediff-setup-windows-plain)))
                      ;; Determine split direction based on orientation
                      (split-fn
                       (cond
                        ((eq claude-code-ide-ediff-split-orientation 'vertical)
                         'split-window-vertically)
                        ((eq claude-code-ide-ediff-split-orientation 'horizontal)
                         'split-window-horizontally)
                        ((eq claude-code-ide-ediff-split-orientation 'sensible)
                         'split-window-sensibly)
                        (t 'split-window-sensibly))))

                  ;; Start ediff - we're already in the right frame
                  (let ((ediff-control-buffer-suffix (format "<%s>" tab-name))
                        (ediff-window-setup-function setup-fn)
                        (ediff-split-window-function split-fn))
                    (ediff-buffers buffer-A buffer-B))))
            (error
             ;; Handle ediff startup errors
             (when buffer-B
               (kill-buffer buffer-B))
             (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session)))
               (remhash tab-name active-diffs))
             ;; Remove the hooks we added
             (remove-hook 'ediff-before-setup-hook before-setup-hook-fn)
             (remove-hook 'ediff-startup-hook startup-hook-fn)
             ;; Re-signal the error
             (signal (car err) (cdr err))))

          ;; Return deferred indicator with session
          `((deferred . t)
            (unique-key . ,tab-name)
            (session . ,session)))))))

(defun claude-code-ide-mcp--handle-ediff-quit (tab-name &optional session)
  "Handle ediff quit for TAB-NAME.
Prompts user to accept/reject changes and sends appropriate MCP response.
SESSION is the MCP session to use - if not provided, uses diff-info stored
session."
  (let* ((temp-session session)  ; Store the passed session
         (active-diffs (claude-code-ide-mcp--get-active-diffs temp-session))
         (diff-info (gethash tab-name active-diffs)))
    (if (not diff-info)
        ;; Already cleaned up, just exit
        nil
      ;; Get session from diff-info if not provided
      (unless temp-session
        (setq temp-session (alist-get 'session diff-info)))
      ;; Process the diff
      (let* ((buffer-B (alist-get 'buffer-B diff-info))
             (final-session (or temp-session
                                (claude-code-ide-mcp--get-current-session))))

        (let ((accept-changes (y-or-n-p "Accept the changes? ")))

          ;; Window configuration is now restored by the quit hook closure
          ;; so we don't need to restore it here

          ;; Defer the response to ensure ediff cleanup completes first
          (run-with-idle-timer claude-code-ide-mcp-handlers-idle-timer-delay nil
                               (lambda ()
                                 (if accept-changes
                                     ;; User accepted changes
                                     (let ((final-content (with-current-buffer buffer-B
                                                            (buffer-string))))
                                       ;; Don't save the file - let Claude Code handle the actual file modification
                                       ;; Just return the content that should be saved

                                       ;; Send FILE_SAVED response with the new content
                                       (claude-code-ide-mcp-complete-deferred
                                        final-session
                                        "openDiff"
                                        (list `((type . "text") (text . "FILE_SAVED"))
                                              `((type . "text") (text . ,final-content)))
                                        tab-name)

                                       ;; Mark that we've responded so close_tab knows ediff should be cleaned up
                                       (when final-session
                                         (let ((session-diffs (claude-code-ide-mcp--get-active-diffs final-session)))
                                           (puthash tab-name
                                                    (cons '(responded . t) diff-info)
                                                    session-diffs))))

                                   ;; User rejected changes - send DIFF_REJECTED response
                                   (claude-code-ide-mcp-complete-deferred
                                    final-session
                                    "openDiff"
                                    (list `((type . "text") (text . "DIFF_REJECTED"))
                                          `((type . "text") (text . ,tab-name)))
                                    tab-name)

                                   ;; Mark that we've responded so close_tab knows ediff should be cleaned up
                                   ;; Don't clean up immediately - let ediff quit process complete first
                                   (when final-session
                                     (let ((session-diffs (claude-code-ide-mcp--get-active-diffs final-session)))
                                       (puthash tab-name
                                                (cons '(responded . t) diff-info)
                                                session-diffs)))))))))))

(defun claude-code-ide-mcp--cleanup-diff (tab-name &optional session)
  "Clean up diff session for TAB-NAME.
SESSION is the MCP session to use - if not provided, tries to determine it."
  (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session)))
    (when-let* ((diff-info (gethash tab-name active-diffs)))
      ;; If session wasn't provided, try to get it from diff-info
      (unless session
        (setq session (alist-get 'session diff-info)))
      (let ((buffer-A (alist-get 'buffer-A diff-info))
            (buffer-B (alist-get 'buffer-B diff-info))
            (control-buf (alist-get 'control-buffer diff-info))
            (file-exists (alist-get 'file-exists diff-info))
            ;; First try to get buffers from diff-info (stored during close_tab)
            (error-buffer (alist-get 'error-buffer diff-info))
            (diff-buffer (alist-get 'diff-buffer diff-info))
            (fine-diff-buffer (alist-get 'fine-diff-buffer diff-info))
            (custom-diff-buffer (alist-get 'custom-diff-buffer diff-info)))
        ;; Get all ediff buffers from control buffer before killing it
        (when (and control-buf (buffer-live-p control-buf))
          (with-current-buffer control-buf
            ;; Capture all ediff-related buffers
            (claude-code-ide-debug "Capturing ediff buffers from control buffer")
            (when (and (boundp 'ediff-error-buffer) ediff-error-buffer (not error-buffer))
              (setq error-buffer ediff-error-buffer))
            (when (and (boundp 'ediff-diff-buffer) ediff-diff-buffer (not diff-buffer))
              (setq diff-buffer ediff-diff-buffer))
            (when (and (boundp 'ediff-fine-diff-buffer) ediff-fine-diff-buffer (not fine-diff-buffer))
              (setq fine-diff-buffer ediff-fine-diff-buffer))
            (when (and (boundp 'ediff-custom-diff-buffer) ediff-custom-diff-buffer (not custom-diff-buffer))
              (setq custom-diff-buffer ediff-custom-diff-buffer))
            (setq ediff-quit-hook nil))  ; Prevent quit hooks from running
          (kill-buffer control-buf))
        ;; Kill all the auxiliary buffers
        (dolist (buf (list error-buffer diff-buffer fine-diff-buffer custom-diff-buffer))
          (when (and buf (buffer-live-p buf))
            (claude-code-ide-debug "Killing ediff auxiliary buffer: %s" (buffer-name buf))
            (kill-buffer buf)))
        ;; Kill the temporary buffer (buffer B)
        (when (and buffer-B (buffer-live-p buffer-B))
          (kill-buffer buffer-B))
        ;; Kill buffer A only if it was created for a new file
        (when (and buffer-A (buffer-live-p buffer-A) (not file-exists))
          ;; This is a *New file: buffer that we created
          (kill-buffer buffer-A))
        ;; Remove from active diffs
        (remhash tab-name active-diffs)))))

(defun claude-code-ide-mcp-handle-close-all-diff-tabs (_arguments)
  "Close all diff tabs/buffers for the current session only."
  (let ((closed-count 0)
        (current-session (claude-code-ide-mcp--get-current-session)))
    (if current-session
        ;; Only clean up diffs for the current session
        (let ((session-diffs (claude-code-ide-mcp-session-active-diffs current-session)))
          (maphash (lambda (tab-name _diff-info)
                     (claude-code-ide-mcp--cleanup-diff tab-name current-session)
                     (setq closed-count (1+ closed-count)))
                   session-diffs))
      ;; Fallback to project directory if no current session
      (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project)))
        (when-let* ((session (claude-code-ide-mcp--get-session-for-project
                              project-dir)))
          (let ((session-diffs (claude-code-ide-mcp-session-active-diffs session)))
            (maphash (lambda (tab-name _diff-info)
                       (claude-code-ide-mcp--cleanup-diff tab-name session)
                       (setq closed-count (1+ closed-count)))
                     session-diffs)))))
    ;; Return success in VS Code format
    (list `((type . "text")
            (text . ,(format "CLOSED_%d_DIFF_TABS" closed-count))))))

(defun claude-code-ide-mcp-handle-check-document-dirty (arguments)
  "Check if document is dirty.
ARGUMENTS should contain `filePath`."
  (let ((path (alist-get 'filePath arguments)))
    (unless path
      (signal 'mcp-error '("Missing required parameter: filePath")))
    (let ((buffer (find-buffer-visiting path)))
      (if buffer
          `((isDirty . ,(if (buffer-modified-p buffer) t :json-false)))
        `((isDirty . :json-false))))))

;;; Tool Registry - Set the values

(defun claude-code-ide-mcp--build-tool-list ()
  "Build the tool list, conditionally including ediff tools."
  `(("openFile" . claude-code-ide-mcp-handle-open-file)
    ("getCurrentSelection" . claude-code-ide-mcp-handle-get-current-selection)
    ("getOpenEditors" . claude-code-ide-mcp-handle-get-open-editors)
    ("getWorkspaceFolders" . claude-code-ide-mcp-handle-get-workspace-folders)
    ("getDiagnostics" . claude-code-ide-mcp-handle-get-diagnostics)
    ("saveDocument" . claude-code-ide-mcp-handle-save-document)
    ("close_tab" . claude-code-ide-mcp-handle-close-tab)
    ,@(when (bound-and-true-p claude-code-ide-use-ide-diff)
        '(("openDiff" . claude-code-ide-mcp-handle-open-diff)
          ("closeAllDiffTabs" . claude-code-ide-mcp-handle-close-all-diff-tabs)))
    ("checkDocumentDirty" . claude-code-ide-mcp-handle-check-document-dirty)))

(setq claude-code-ide-mcp-tools (claude-code-ide-mcp--build-tool-list))

(defun claude-code-ide-mcp--build-tool-schemas ()
  "Build the tool schemas, conditionally including ediff tools."
  `(("openFile" . ((type . "object")
                   (properties . ((path . ((type . "string")
                                           (description . "Path to the file to open")))
                                  (startLine . ((type . "integer")
                                                (description . "Start line for selection")))
                                  (endLine . ((type . "integer")
                                              (description . "End line for selection")))
                                  (startText . ((type . "string")
                                                (description . "Start text pattern for selection (takes precedence over line numbers)")))
                                  (endText . ((type . "string")
                                              (description . "End text pattern for selection")))))
                   (required . ["path"])))
    ("getCurrentSelection" . ((type . "object")
                              (properties . :json-empty)))
    ("getOpenEditors" . ((type . "object")
                         (properties . :json-empty)))
    ("getWorkspaceFolders" . ((type . "object")
                              (properties . :json-empty)))
    ("getDiagnostics" . ((type . "object")
                         (properties . ((uri . ((type . "string")
                                                (description . "Optional file URI to get diagnostics for. If not provided, gets diagnostics for all files.")))))
                         (required . [])))
    ("saveDocument" . ((type . "object")
                       (properties . ((path . ((type . "string")
                                               (description . "Path to the file to save")))))
                       (required . ["path"])))
    ("close_tab" . ((type . "object")
                    (properties . ((path . ((type . "string")
                                            (description . "Path to the file to close")))
                                   (tab_name . ((type . "string")
                                                (description . "Name of the tab to close")))))
                    (required . [])))
    ,@(when (bound-and-true-p claude-code-ide-use-ide-diff)
        '(("openDiff" . ((type . "object")
                         (properties . ((old_file_path . ((type . "string")
                                                          (description . "Path to the original file")))
                                        (new_file_path . ((type . "string")
                                                          (description . "Path to the new file (usually same as old)")))
                                        (new_file_contents . ((type . "string")
                                                              (description . "New content to diff against")))
                                        (tab_name . ((type . "string")
                                                     (description . "Name for the diff tab")))))
                         (required . ["old_file_path" "new_file_path" "new_file_contents" "tab_name"])))
          ("closeAllDiffTabs" . ((type . "object")
                                 (properties . :json-empty)))))
    ("checkDocumentDirty" . ((type . "object")
                             (properties . ((filePath . ((type . "string")
                                                         (description . "Path to the file to check")))))
                             (required . ["filePath"])))))

(setq claude-code-ide-mcp-tool-schemas (claude-code-ide-mcp--build-tool-schemas))

(defun claude-code-ide-mcp--build-tool-descriptions ()
  "Build the tool descriptions, conditionally including ediff tools."
  `(("openFile" . "Open a file in the editor and optionally select a range of text")
    ("getCurrentSelection" . "Get the currently selected text and its location")
    ("getOpenEditors" . "Get the list of currently open editors/buffers")
    ("getWorkspaceFolders" . "Get the current workspace/project folders")
    ("getDiagnostics" . "Get language diagnostics from Emacs")
    ("saveDocument" . "Save a document to disk")
    ("close_tab" . "Close a tab/buffer")
    ,@(when (bound-and-true-p claude-code-ide-use-ide-diff)
        '(("openDiff" . "Open a diff view comparing old and new file contents")
          ("closeAllDiffTabs" . "Close all open diff tabs in the current session")))
    ("checkDocumentDirty" . "Check if a document has unsaved changes")))

(setq claude-code-ide-mcp-tool-descriptions (claude-code-ide-mcp--build-tool-descriptions))

(provide 'claude-code-ide-mcp-handlers)

;;; claude-code-ide-mcp-handlers.el ends here
