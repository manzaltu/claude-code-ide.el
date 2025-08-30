;;; claude-code-ide-transient.el --- Transient menus for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Keywords: ai, claude, transient, menu

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

;; This file provides transient menus for Claude Code IDE, offering
;; a convenient interface for all Claude Code operations.

;;; Code:

(require 'transient)
(require 'claude-code-ide-debug)

;; Declare functions from other files to avoid circular dependencies
(declare-function claude-code-ide "claude-code-ide" ())
(declare-function claude-code-ide-resume "claude-code-ide" ())
(declare-function claude-code-ide-continue "claude-code-ide" ())
(declare-function claude-code-ide-stop "claude-code-ide" ())
(declare-function claude-code-ide-list-sessions "claude-code-ide" ())
(declare-function claude-code-ide-switch-to-buffer "claude-code-ide" ())
(declare-function claude-code-ide-insert-at-mentioned "claude-code-ide" ())
(declare-function claude-code-ide-send-prompt "claude-code-ide" ())
(declare-function claude-code-ide-send-escape "claude-code-ide" ())
(declare-function claude-code-ide-insert-newline "claude-code-ide" ())
(declare-function claude-code-ide-toggle "claude-code-ide" ())
(declare-function claude-code-ide-check-status "claude-code-ide" ())
(declare-function claude-code-ide--ensure-cli "claude-code-ide" ())
(declare-function claude-code-ide-mcp--active-sessions "claude-code-ide-mcp" ())
(declare-function claude-code-ide-mcp-session-project-dir "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-port "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-client "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-buffer "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-last-buffer "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp--get-current-session "claude-code-ide-mcp" ())
(declare-function claude-code-ide--get-working-directory "claude-code-ide" ())

;; Declare variables
(defvar claude-code-ide-cli-path)
(defvar claude-code-ide-debug)
(defvar claude-code-ide-window-side)
(defvar claude-code-ide-window-width)
(defvar claude-code-ide-window-height)
(defvar claude-code-ide-focus-on-open)
(defvar claude-code-ide-focus-claude-after-ediff)
(defvar claude-code-ide-show-claude-window-in-ediff)
(defvar claude-code-ide-use-ide-diff)
(defvar claude-code-ide-switch-tab-on-ediff)
(defvar claude-code-ide-use-side-window)
(defvar claude-code-ide-cli-debug)
(defvar claude-code-ide-cli-extra-flags)
(defvar claude-code-ide-system-prompt)

;;; Helper Functions

(defun claude-code-ide--has-active-session-p ()
  "Check if there's an active Claude Code session for the current buffer."
  (when (claude-code-ide-mcp--get-current-session) t))

(defun claude-code-ide--start-description ()
  "Dynamic description for start command based on session status."
  (if (claude-code-ide--has-active-session-p)
      (propertize "Start new Claude Code session (session already running)"
                  'face 'transient-inactive-value)
    "Start new Claude Code session"))

(defun claude-code-ide--start-if-no-session ()
  "Start Claude Code only if no session is active for current buffer."
  (interactive)
  (if (claude-code-ide--has-active-session-p)
      (let ((working-dir (claude-code-ide--get-working-directory)))
        (claude-code-ide-log "Claude Code session already running in %s"
                             (abbreviate-file-name working-dir)))
    (claude-code-ide)))

(defun claude-code-ide--continue-description ()
  "Dynamic description for continue command based on session status."
  (if (claude-code-ide--has-active-session-p)
      (propertize "Continue most recent conversation (session already running)"
                  'face 'transient-inactive-value)
    "Continue most recent conversation"))

(defun claude-code-ide--continue-if-no-session ()
  "Continue Claude Code only if no session is active for current buffer."
  (interactive)
  (if (claude-code-ide--has-active-session-p)
      (let ((working-dir (claude-code-ide--get-working-directory)))
        (claude-code-ide-log "Claude Code session already running in %s"
                             (abbreviate-file-name working-dir)))
    (claude-code-ide-continue)))

(defun claude-code-ide--resume-description ()
  "Dynamic description for resume command based on session status."
  (if (claude-code-ide--has-active-session-p)
      (propertize "Resume session (session already running)"
                  'face 'transient-inactive-value)
    "Resume session (from previous conversation)"))

(defun claude-code-ide--resume-if-no-session ()
  "Resume Claude Code only if no session is active for current buffer."
  (interactive)
  (if (claude-code-ide--has-active-session-p)
      (let ((working-dir (claude-code-ide--get-working-directory)))
        (claude-code-ide-log "Claude Code session already running in %s"
                             (abbreviate-file-name working-dir)))
    (claude-code-ide-resume)))

(defun claude-code-ide--session-status ()
  "Return a string describing the current session status."
  (if-let ((session (claude-code-ide-mcp--get-current-session)))
      (let* ((project-dir (claude-code-ide-mcp-session-project-dir session))
             (project-name (file-name-nondirectory (directory-file-name project-dir)))
             (connected (if (claude-code-ide-mcp-session-client session) "connected" "disconnected")))
        (propertize (format "Active session in [%s] - %s" project-name connected)
                    'face 'success))
    (propertize "No active session" 'face 'transient-inactive-value)))

(defun claude-code-ide-toggle-window ()
  "Toggle visibility of Claude Code window.
If called from a Claude vterm buffer, toggle that window.
Otherwise, if multiple sessions exist, prompt for selection."
  (interactive)
  (claude-code-ide-toggle))

(defun claude-code-ide-show-version-info ()
  "Show detailed version information for Claude Code CLI."
  (interactive)
  (if (claude-code-ide--ensure-cli)
      (let ((version-output
             (with-temp-buffer
               (call-process claude-code-ide-cli-path nil t nil "--version")
               (buffer-string))))
        (with-output-to-temp-buffer "*Claude Code Version*"
          (princ "Claude Code CLI Version Information\n")
          (princ "===================================\n\n")
          (princ version-output)
          (princ "\n\nExecutable path: ")
          (princ (executable-find claude-code-ide-cli-path))))
    (user-error "Claude Code CLI not available")))

(defun claude-code-ide-show-mcp-sessions ()
  "Show information about active MCP sessions."
  (interactive)
  (let ((sessions (claude-code-ide-mcp--active-sessions)))
    (if sessions
        (with-output-to-temp-buffer "*Claude Code MCP Sessions*"
          (princ "Active MCP Sessions\n")
          (princ "==================\n\n")
          (dolist (session sessions)
            (princ (format "Project: %s\n" (claude-code-ide-mcp-session-project-dir session)))
            (princ (format "  Port: %d\n" (claude-code-ide-mcp-session-port session)))
            (princ (format "  Connected: %s\n"
                           (if (claude-code-ide-mcp-session-client session) "Yes" "No")))
            (princ (format "  Buffer: %s\n"
                           (if (claude-code-ide-mcp-session-last-buffer session)
                               (buffer-name (claude-code-ide-mcp-session-last-buffer session))
                             "None")))
            (princ "\n")))
      (claude-code-ide-log "No active MCP sessions"))))

(defun claude-code-ide-show-active-ports ()
  "Show active ports used by MCP servers."
  (interactive)
  (let ((sessions (claude-code-ide-mcp--active-sessions)))
    (if sessions
        (with-output-to-temp-buffer "*Claude Code Active Ports*"
          (princ "Active MCP Server Ports\n")
          (princ "======================\n\n")
          (dolist (session sessions)
            (princ (format "Port %d: %s\n"
                           (claude-code-ide-mcp-session-port session)
                           (abbreviate-file-name (claude-code-ide-mcp-session-project-dir session))))))
      (claude-code-ide-log "No active MCP servers"))))

(defun claude-code-ide-toggle-debug-mode ()
  "Toggle Claude Code debug mode."
  (interactive)
  (setq claude-code-ide-debug (not claude-code-ide-debug))
  (claude-code-ide-log "Debug mode %s" (if claude-code-ide-debug "enabled" "disabled")))

;;; Transient Infix Classes

(transient-define-suffix claude-code-ide--set-window-side (side)
  "Set window side."
  :description "Set window side"
  (interactive (list (intern (completing-read "Window side: "
                                              '("left" "right" "top" "bottom")
                                              nil t nil nil
                                              (symbol-name claude-code-ide-window-side)))))
  (setq claude-code-ide-window-side side)
  (claude-code-ide-log "Window side set to %s" side))

(transient-define-suffix claude-code-ide--set-window-width (width)
  "Set window width."
  :description "Set window width"
  (interactive (list (read-number "Window width: " claude-code-ide-window-width)))
  (setq claude-code-ide-window-width width)
  (claude-code-ide-log "Window width set to %d" width))

(transient-define-suffix claude-code-ide--set-window-height (height)
  "Set window height."
  :description "Set window height"
  (interactive (list (read-number "Window height: " claude-code-ide-window-height)))
  (setq claude-code-ide-window-height height)
  (claude-code-ide-log "Window height set to %d" height))

(transient-define-suffix claude-code-ide--set-cli-path (path)
  "Set CLI path."
  :description "Set CLI path"
  (interactive (list (read-file-name "Claude CLI path: " nil claude-code-ide-cli-path t)))
  (setq claude-code-ide-cli-path path)
  (claude-code-ide-log "CLI path set to %s" path))

(transient-define-suffix claude-code-ide--set-cli-extra-flags (flags)
  "Set additional CLI flags."
  :description "Set additional CLI flags"
  (interactive (list (read-string "Additional CLI flags: " claude-code-ide-cli-extra-flags)))
  (setq claude-code-ide-cli-extra-flags flags)
  (claude-code-ide-log "CLI extra flags set to %s" flags))

(transient-define-suffix claude-code-ide--set-system-prompt (prompt)
  "Set the system prompt to append."
  :description "Set system prompt"
  (interactive (list (if claude-code-ide-system-prompt
                         (read-string "System prompt (leave empty to disable): "
                                      claude-code-ide-system-prompt)
                       (read-string "System prompt: "))))
  (setq claude-code-ide-system-prompt (if (string-empty-p prompt) nil prompt))
  (claude-code-ide-log "System prompt %s"
                       (if claude-code-ide-system-prompt
                           (format "set to: %s" claude-code-ide-system-prompt)
                         "disabled")))

;;; Transient Suffix Functions

(transient-define-suffix claude-code-ide--toggle-focus-on-open ()
  "Toggle focus on open setting."
  (interactive)
  (setq claude-code-ide-focus-on-open (not claude-code-ide-focus-on-open))
  (claude-code-ide-log "Focus on open %s" (if claude-code-ide-focus-on-open "enabled" "disabled")))

(transient-define-suffix claude-code-ide--toggle-focus-after-ediff ()
  "Toggle focus after ediff setting."
  (interactive)
  (setq claude-code-ide-focus-claude-after-ediff (not claude-code-ide-focus-claude-after-ediff))
  (claude-code-ide-log "Focus after ediff %s" (if claude-code-ide-focus-claude-after-ediff "enabled" "disabled")))

(transient-define-suffix claude-code-ide--toggle-show-claude-in-ediff ()
  "Toggle showing Claude window during ediff."
  (interactive)
  (setq claude-code-ide-show-claude-window-in-ediff (not claude-code-ide-show-claude-window-in-ediff))
  (claude-code-ide-log "Show Claude window in ediff %s" (if claude-code-ide-show-claude-window-in-ediff "enabled" "disabled")))

(transient-define-suffix claude-code-ide--toggle-use-side-window ()
  "Toggle use side window setting."
  (interactive)
  (setq claude-code-ide-use-side-window (not claude-code-ide-use-side-window))
  (claude-code-ide-log "Use side window %s" (if claude-code-ide-use-side-window "enabled" "disabled")))

(transient-define-suffix claude-code-ide--toggle-use-ide-diff ()
  "Toggle IDE diff viewer setting."
  (interactive)
  (setq claude-code-ide-use-ide-diff (not claude-code-ide-use-ide-diff))
  (claude-code-ide-log "IDE diff viewer %s" (if claude-code-ide-use-ide-diff "enabled" "disabled")))

(transient-define-suffix claude-code-ide--toggle-switch-tab-on-ediff ()
  "Toggle tab switching on ediff setting."
  (interactive)
  (setq claude-code-ide-switch-tab-on-ediff (not claude-code-ide-switch-tab-on-ediff))
  (claude-code-ide-log "Switch tab on ediff %s" (if claude-code-ide-switch-tab-on-ediff "enabled" "disabled")))

(transient-define-suffix claude-code-ide--toggle-cli-debug ()
  "Toggle CLI debug mode."
  (interactive)
  (setq claude-code-ide-cli-debug (not claude-code-ide-cli-debug))
  (claude-code-ide-log "CLI debug mode %s" (if claude-code-ide-cli-debug "enabled" "disabled")))

(defun claude-code-ide--save-config ()
  "Save current configuration to custom file."
  (interactive)
  (customize-save-variable 'claude-code-ide-window-side claude-code-ide-window-side)
  (customize-save-variable 'claude-code-ide-window-width claude-code-ide-window-width)
  (customize-save-variable 'claude-code-ide-window-height claude-code-ide-window-height)
  (customize-save-variable 'claude-code-ide-focus-on-open claude-code-ide-focus-on-open)
  (customize-save-variable 'claude-code-ide-focus-claude-after-ediff claude-code-ide-focus-claude-after-ediff)
  (customize-save-variable 'claude-code-ide-show-claude-window-in-ediff claude-code-ide-show-claude-window-in-ediff)
  (customize-save-variable 'claude-code-ide-use-ide-diff claude-code-ide-use-ide-diff)
  (customize-save-variable 'claude-code-ide-switch-tab-on-ediff claude-code-ide-switch-tab-on-ediff)
  (customize-save-variable 'claude-code-ide-use-side-window claude-code-ide-use-side-window)
  (customize-save-variable 'claude-code-ide-cli-path claude-code-ide-cli-path)
  (customize-save-variable 'claude-code-ide-cli-extra-flags claude-code-ide-cli-extra-flags)
  (customize-save-variable 'claude-code-ide-system-prompt claude-code-ide-system-prompt)
  (claude-code-ide-log "Configuration saved to custom file"))

;;; Transient Menus

;;;###autoload (autoload 'claude-code-ide-menu "claude-code-ide-transient" "Claude Code IDE main menu." t)
(transient-define-prefix claude-code-ide-menu ()
  "Claude Code IDE main menu."
  [:description claude-code-ide--session-status]
  ["Claude Code IDE"
   ["Session Management"
    ("s" claude-code-ide--start-if-no-session :description claude-code-ide--start-description)
    ("c" claude-code-ide--continue-if-no-session :description claude-code-ide--continue-description)
    ("r" claude-code-ide--resume-if-no-session :description claude-code-ide--resume-description)
    ("q" "Stop current session" claude-code-ide-stop)
    ("l" "List all sessions" claude-code-ide-list-sessions)]
   ["Navigation"
    ("b" "Switch to Claude buffer" claude-code-ide-switch-to-buffer)
    ("w" "Toggle window visibility" claude-code-ide-toggle-window)
    ("W" "Toggle recent window" claude-code-ide-toggle-recent)]
   ["Interaction"
    ("i" "Insert selection" claude-code-ide-insert-at-mentioned)
    ("p" "Send prompt from minibuffer" claude-code-ide-send-prompt)
    ("e" "Send escape key" claude-code-ide-send-escape)
    ("n" "Insert newline" claude-code-ide-insert-newline)]
   ["Submenus"
    ("C" "Configuration" claude-code-ide-config-menu)
    ("d" "Debugging" claude-code-ide-debug-menu)]])

(transient-define-prefix claude-code-ide-config-menu ()
  "Claude Code configuration menu."
  ["Claude Code Configuration"
   ["Window Settings"
    ("s" "Set window side" claude-code-ide--set-window-side)
    ("w" "Set window width" claude-code-ide--set-window-width)
    ("h" "Set window height" claude-code-ide--set-window-height)
    ("f" "Toggle focus on open" claude-code-ide--toggle-focus-on-open
     :description (lambda () (format "Focus on open (%s)"
                                     (if claude-code-ide-focus-on-open "ON" "OFF"))))
    ("e" "Toggle focus after ediff" claude-code-ide--toggle-focus-after-ediff
     :description (lambda () (format "Focus after ediff (%s)"
                                     (if claude-code-ide-focus-claude-after-ediff "ON" "OFF"))))
    ("E" "Toggle show Claude in ediff" claude-code-ide--toggle-show-claude-in-ediff
     :description (lambda () (format "Show Claude in ediff (%s)"
                                     (if claude-code-ide-show-claude-window-in-ediff "ON" "OFF"))))
    ("i" "Toggle IDE diff viewer" claude-code-ide--toggle-use-ide-diff
     :description (lambda () (format "IDE diff viewer (%s)"
                                     (if claude-code-ide-use-ide-diff "ON" "OFF"))))
    ("t" "Toggle tab switching on ediff" claude-code-ide--toggle-switch-tab-on-ediff
     :description (lambda () (format "Tab switch on ediff (%s)"
                                     (if claude-code-ide-switch-tab-on-ediff "ON" "OFF"))))
    ("u" "Toggle side window" claude-code-ide--toggle-use-side-window
     :description (lambda () (format "Use side window (%s)"
                                     (if claude-code-ide-use-side-window "ON" "OFF"))))]
   ["CLI Settings"
    ("p" "Set CLI path" claude-code-ide--set-cli-path)
    ("x" "Set extra CLI flags" claude-code-ide--set-cli-extra-flags)
    ("a" "Set system prompt" claude-code-ide--set-system-prompt)]]
  ["Save"
   ("S" "Save configuration" claude-code-ide--save-config)])

(transient-define-prefix claude-code-ide-debug-menu ()
  "Claude Code debug menu."
  ["Claude Code Debug"
   ["Status"
    ("S" "Check CLI status" claude-code-ide-check-status)
    ("v" "Show version info" claude-code-ide-show-version-info)]
   ["Debug Settings"
    ("d" "Toggle debug mode" claude-code-ide-toggle-debug-mode
     :description (lambda () (format "Debug mode (%s)"
                                     (if claude-code-ide-debug "ON" "OFF"))))
    ("D" "Toggle CLI debug mode" claude-code-ide--toggle-cli-debug
     :description (lambda () (format "CLI debug mode (%s)"
                                     (if claude-code-ide-cli-debug "ON" "OFF"))))]
   ["Debug Logs"
    ("l" "Show debug log" claude-code-ide-show-debug)
    ("c" "Clear debug log" claude-code-ide-clear-debug)]
   ["MCP Server"
    ("m" "Show MCP sessions" claude-code-ide-show-mcp-sessions)
    ("p" "Show active ports" claude-code-ide-show-active-ports)]])

(provide 'claude-code-ide-transient)

;;; claude-code-ide-transient.el ends here
