;;; claude-code-ide-tool-magit.el --- Magit refresh MCP tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Keywords: ai, claude, mcp, magit, git

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

;; This file provides an MCP tool for refreshing the Magit status buffer.
;; After Claude Code makes git changes (commits, stashes, etc.), the Magit
;; status buffer becomes stale.  This tool lets Claude trigger a refresh
;; to keep the user's git UI in sync.

;;; Code:

(require 'claude-code-ide-mcp-server)

(declare-function magit-get-mode-buffer "magit-mode" (mode &optional value frame))
(declare-function magit-refresh-buffer "magit-mode" ())

(defun claude-code-ide-mcp-magit-refresh ()
  "Refresh the Magit status buffer for the current session's project.
Uses session context to determine the project directory, then schedules
an asynchronous refresh of the corresponding magit-status buffer.
Returns immediately so Claude Code is not blocked."
  (claude-code-ide-mcp-server-with-session-context nil
    (cond
     ((not (fboundp 'magit-get-mode-buffer))
      "Magit is not installed or not loaded")
     (t
      (let ((buf (magit-get-mode-buffer 'magit-status-mode))
            (dir default-directory))
        (if (not buf)
            (format "No magit-status buffer open for %s" dir)
          (run-at-time 0 nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (magit-refresh-buffer)))))
          (format "Scheduled magit-status refresh for %s" dir)))))))

;;; Tool Registration

;;;###autoload
(defun claude-code-ide-tool-magit-setup ()
  "Register Magit refresh MCP tool."
  (claude-code-ide-make-tool
   :function #'claude-code-ide-mcp-magit-refresh
   :name "claude-code-ide-mcp-magit-refresh"
   :description "Refresh the Magit status buffer for the current project. Use after git operations (commit, stash, checkout, etc.) to keep the user's Magit UI in sync."
   :args nil))

(provide 'claude-code-ide-tool-magit)
;;; claude-code-ide-tool-magit.el ends here
