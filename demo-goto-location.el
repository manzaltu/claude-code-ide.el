;;; demo-goto-location.el --- Demonstrate the gotoLocation MCP tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This script demonstrates the new gotoLocation MCP tool.
;; To run: M-x eval-buffer in this file
;;
;; The demo will jump to 4 different locations in the codebase,
;; highlighting each location briefly to show the feature in action.

;;; Code:

;; Load the handlers file from the current directory
(add-to-list 'load-path default-directory)
(load-file (expand-file-name "claude-code-ide-mcp-handlers.el" default-directory))

(defun demo-goto-location ()
  "Demonstrate the gotoLocation tool with several examples."
  (interactive)
  (message "Starting gotoLocation demonstration...")

  ;; Demo 1: Jump to the goto-location handler itself with highlight
  (message "Demo 1: Jumping to goto-location handler (line 293) with highlight...")
  (sit-for 1)
  (claude-code-ide-mcp-handle-goto-location
   '((file_path . "/home/jdblair/src/claude-code-ide.el/claude-code-ide-mcp-handlers.el")
     (line . 293)
     (highlight . t)))

  (sit-for 2)

  ;; Demo 2: Jump to the test we wrote
  (message "Demo 2: Jumping to goto-location test (line 1116)...")
  (sit-for 1)
  (claude-code-ide-mcp-handle-goto-location
   '((file_path . "/home/jdblair/src/claude-code-ide.el/claude-code-ide-tests.el")
     (line . 1116)
     (column . 0)
     (highlight . t)))

  (sit-for 2)

  ;; Demo 3: Jump to the tool registration with highlight
  (message "Demo 3: Jumping to tool registration (line 778)...")
  (sit-for 1)
  (claude-code-ide-mcp-handle-goto-location
   '((file_path . "/home/jdblair/src/claude-code-ide.el/claude-code-ide-mcp-handlers.el")
     (line . 778)
     (highlight . t)))

  (sit-for 2)

  ;; Demo 4: Jump to a specific column position
  (message "Demo 4: Jumping to line 300, column 20...")
  (sit-for 1)
  (claude-code-ide-mcp-handle-goto-location
   '((file_path . "/home/jdblair/src/claude-code-ide.el/claude-code-ide-mcp-handlers.el")
     (line . 300)
     (column . 20)
     (highlight . t)))

  (message "Demo complete! You should have seen 4 different locations with highlighting."))

;; Run the demo
(demo-goto-location)

;;; demo-goto-location.el ends here
