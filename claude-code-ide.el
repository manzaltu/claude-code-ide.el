;;; claude-code-ide.el --- Claude Code integration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.2.7
;; Package-Requires: ((emacs "28.1") (websocket "1.12") (transient "0.9.0") (web-server "0.1.2"))
;; Keywords: ai, claude, code, assistant, mcp, websocket
;; URL: https://github.com/manzaltu/claude-code-ide.el

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

;; Claude Code IDE integration for Emacs provides seamless integration
;; with Claude Code CLI through the Model Context Protocol (MCP).
;; It supports file operations, diagnostics, and editor state management.
;;
;; This package starts a WebSocket server that Claude Code CLI connects to,
;; enabling real-time communication between Emacs and Claude.  It supports
;; multiple concurrent sessions per project.
;;
;; Features:
;; - Automatic IDE mode activation when starting Claude
;; - MCP WebSocket server for bidirectional communication
;; - Project-aware sessions with automatic working directory detection
;; - Clean session management with automatic cleanup on exit
;; - Selection and buffer state tracking
;; - Tool support for file operations, diagnostics, and more
;; - Emacs MCP tools for xref and project navigation
;;
;; Usage:
;; M-x claude-code-ide - Start Claude Code for current project
;; M-x claude-code-ide-continue - Continue most recent conversation in directory
;; M-x claude-code-ide-resume - Resume Claude Code with previous conversation
;; M-x claude-code-ide-stop - Stop Claude Code for current project
;; M-x claude-code-ide-switch-to-buffer - Switch to project's Claude buffer
;; M-x claude-code-ide-list-sessions - List and switch between all sessions
;; M-x claude-code-ide-check-status - Check CLI availability and version
;; M-x claude-code-ide-insert-at-mentioned - Send selected text to Claude
;;
;; Emacs MCP Tools:
;; To enable Emacs tools for Claude, add to your config:
;;   (claude-code-ide-emacs-tools-setup)

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'claude-code-ide-debug)
(require 'claude-code-ide-mcp)
(require 'claude-code-ide-transient)
(require 'claude-code-ide-mcp-server)
(require 'claude-code-ide-emacs-tools)

;; External variable declarations
(defvar eat-terminal)
(defvar eat--synchronize-scroll-function)
(defvar vterm-shell)
(defvar vterm-environment)
(defvar eat-term-name)
(defvar vterm--process)
(defvar ghostel-set-title-function)
(defvar ghostel-enable-title-tracking)
(defvar ghostel-kill-buffer-on-exit)
(defvar evil-ghostel-mode)
(defvar evil-ghostel--escape-mode)

;; External function declarations for vterm
(declare-function vterm "vterm" (&optional arg))
(declare-function vterm-send-string "vterm" (string))
(declare-function vterm-send-escape "vterm" ())
(declare-function vterm-send-return "vterm" ())
(declare-function vterm--window-adjust-process-window-size "vterm" (&optional frame))

;; External function declarations for eat
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile &rest switches))
(declare-function eat-term-send-string "eat" (terminal string))
(declare-function eat-term-display-cursor "eat" (terminal))
(declare-function eat--adjust-process-window-size "eat" (process windows))

;; External function declarations for ghostel
(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel--window-adjust-process-window-size "ghostel" (process windows))

;;; Customization

(defgroup claude-code-ide nil
  "Claude Code integration for Emacs."
  :group 'tools
  :prefix "claude-code-ide-")

(defcustom claude-code-ide-cli-path "claude"
  "Path to the Claude Code CLI executable."
  :type 'string
  :group 'claude-code-ide)

(defcustom claude-code-ide-buffer-name-function #'claude-code-ide--default-buffer-name
  "Function to generate buffer names for Claude Code sessions.
The function is called with the working directory and should return a
string to use as the buffer name.  It may accept a second optional
argument, the instance name (nil for a project's first, unnamed
instance), to control how additional instances are named.
Single-argument functions keep working: the instance name is then
spliced into (or appended to) their result."
  :type 'function
  :group 'claude-code-ide)

(defcustom claude-code-ide-cli-debug nil
  "When non-nil, launch Claude Code with the -d debug flag."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-cli-extra-flags ""
  "Additional flags to pass to the Claude Code CLI.
This should be a string of space-separated flags, e.g. \"--model opus\"."
  :type 'string
  :group 'claude-code-ide)

(defcustom claude-code-ide-system-prompt nil
  "System prompt to append to Claude's default system prompt.
When non-nil, the --append-system-prompt flag will be added with this value.
Set to nil to disable (default)."
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "System prompt text"))
  :group 'claude-code-ide)

(defcustom claude-code-ide-mcp-allowed-tools 'auto
  "Configuration for allowed MCP tools when MCP server is enabled.
Can be one of:
  'auto - Automatically allow all configured emacs-tools (default)
  nil - Disable the --allowedTools flag
  A string - Custom pattern/tools passed directly to --allowedTools
  A list of strings - List of specific tool names to allow"
  :type '(choice (const :tag "Auto (all emacs-tools)" auto)
                 (const :tag "Disabled" nil)
                 (string :tag "Custom pattern")
                 (repeat :tag "Specific tools" string))
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-side 'right
  "Side of the frame where the Claude Code window should appear.
Can be `'left', `'right', `'top', or `'bottom'."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-width 100
  "Body width of the Claude Code side window when opened on left or right.
This sets the usable text area width, excluding fringes and margins."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-height 20
  "Height of the Claude Code side window when opened on top or bottom."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-focus-on-open t
  "Whether to focus the Claude Code window when it opens."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-focus-claude-after-ediff t
  "Whether to focus the Claude Code window after opening ediff.
When non-nil (default), focus returns to the Claude Code window
after opening ediff.  When nil, focus remains on the ediff control
window, allowing direct interaction with the diff controls."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-show-claude-window-in-ediff t
  "Whether to show the Claude Code side window when viewing diffs.
When non-nil (default), the Claude Code side window is restored
after opening ediff.  When nil, the Claude Code window remains
hidden during diff viewing, giving you more screen space for the
diff comparison."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-enable-execute-code t
  "Whether to expose the executeCode tool to the model.
When non-nil, Claude Code can evaluate Elisp expressions in Emacs
via the executeCode MCP tool.  Set to nil to hide the tool entirely."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-use-ide-diff t
  "Whether to use IDE diff viewer for file differences.
When non-nil (default), Claude Code will open an IDE diff viewer
(ediff) when showing file changes.  When nil, Claude Code will
display diffs in the terminal instead."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-switch-tab-on-ediff t
  "Whether to switch back to Claude's original tab when opening ediff.
When non-nil (default), Claude Code will switch back to the tab
where Claude Code was started when opening an ediff session.
When nil, the current tab remains active when ediff is opened."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-use-side-window t
  "Whether to display Claude Code in a side window.
When non-nil (default), Claude Code opens in a dedicated side window
controlled by `claude-code-ide-window-side' and related settings.
When nil, Claude Code opens in a regular buffer that follows standard
display-buffer behavior."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-terminal-backend 'vterm
  "Terminal backend to use for Claude Code sessions.
Can be `vterm', `eat', or `ghostel'.  The vterm backend is the default
and provides a fully-featured terminal emulator.  The eat backend
is an alternative terminal emulator that may work better in some
environments.  The ghostel backend is the recommended one: it renders
Claude Code's TUI with the fewest artifacts (see the README)."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "eat" eat)
                 (const :tag "ghostel" ghostel))
  :group 'claude-code-ide)

(defcustom claude-code-ide-show-backend-recommendation t
  "When non-nil, recommend the ghostel backend once per Emacs session.
Starting an instance on the vterm or eat backend echoes a one-time
suggestion to try ghostel, which renders Claude Code's TUI with the
fewest artifacts.  Set to nil to silence the suggestion."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-ghostel-evil-escape 'evil
  "How insert-state ESC is routed in the ghostel backend's Claude buffers.
Applied per buffer via `evil-ghostel--escape-mode', so it overrides the
global `evil-ghostel-escape' default for Claude Code sessions only.

Claude Code runs as a full-screen TUI (alt-screen / DECSET 1049), so
`evil-ghostel-escape's default `auto' routes ESC to the terminal and
never enters evil normal state.  The default here is `evil' so ESC
switches to normal state for editing the prompt; note that Claude then
no longer receives ESC as its interrupt key.

Valid values match `evil-ghostel-escape': `auto', `terminal', `evil'.
Set to nil to leave the buffer at the global `evil-ghostel-escape'
default.  Has no effect unless the `evil-ghostel' package is loaded and
the terminal backend is `ghostel'."
  :type '(choice (const :tag "Evil (ESC enters normal state)" evil)
                 (const :tag "Terminal (ESC sent to Claude)" terminal)
                 (const :tag "Auto (alt-screen heuristic)" auto)
                 (const :tag "Leave global default" nil))
  :group 'claude-code-ide)

(defcustom claude-code-ide-no-flicker nil
  "Enable Claude Code's flicker-free terminal renderer.
When non-nil, sets CLAUDE_CODE_NO_FLICKER=1 which activates an
alternative rendering mode that eliminates terminal flicker."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-prevent-reflow-glitch t
  "Workaround for Claude Code terminal scrolling bug #1422.
When non-nil (default), prevents the terminal from reflowing on height-only
changes which can trigger uncontrollable scrolling in Claude Code.
See: https://github.com/anthropics/claude-code/issues/1422
This setting should be removed once the upstream bug is fixed."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-vterm-anti-flicker t
  "Enable intelligent flicker reduction for vterm display.
When enabled, this feature optimizes terminal rendering by detecting
and batching rapid update sequences.  This provides smoother visual
output during complex terminal operations such as expanding text areas
and rapid screen updates.

This optimization applies only to vterm and uses advanced pattern
matching to maintain responsiveness while improving visual quality."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-vterm-render-delay 0.005
  "Rendering optimization delay for batched terminal updates.
This parameter defines the collection window for related terminal
update sequences when anti-flicker mode is active.  The timing
balances visual smoothness with interaction responsiveness.

The 0.005 second (5ms) default delivers optimal rendering quality
with imperceptible latency."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-terminal-initialization-delay 0.1
  "Initialization delay for terminal stability.
Provides a brief stabilization period when launching terminals
to ensure proper layout calculation and rendering.

The delay allows terminals to complete initial dimension calculations,
preventing display artifacts like prompt misalignment and cursor
positioning errors.  The 100ms default ensures reliable initialization
without noticeable latency."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-eat-preserve-position t
  "Maintain terminal scroll position when switching windows.
When enabled, prevents the eat terminal from jumping to the top
when you switch focus to other windows and return.  This provides
a more stable viewing experience when working with multiple windows."
  :type 'boolean
  :group 'claude-code-ide)

(define-obsolete-variable-alias
  'claude-code-ide-eat-initialization-delay
  'claude-code-ide-terminal-initialization-delay
  "0.2.6")

;;; Constants

(defconst claude-code-ide--active-editor-notification-delay 0.1
  "Delay in seconds before sending active editor notification after connection.")

;;; Variables

(defvar claude-code-ide--cli-available nil
  "Whether Claude Code CLI is available and detected.")

(defvar claude-code-ide--session-counter 0
  "Monotonic counter making session IDs unique within this Emacs process.")

(defvar claude-code-ide--backend-recommendation-shown nil
  "Non-nil once the ghostel recommendation was echoed this Emacs session.")

(defvar claude-code-ide--last-accessed-buffer nil
  "The most recently accessed Claude Code buffer.")


;;; Vterm Rendering Optimization

(defvar-local claude-code-ide--vterm-render-queue nil
  "List of pending terminal output strings awaiting batched rendering.
Stored in reverse order for O(1) push, joined at flush time.")

(defvar-local claude-code-ide--vterm-render-timer nil
  "Timer for executing queued rendering operations.")

(defun claude-code-ide--count-escape-sequence (sequence input)
  "Count occurrences of escape SEQUENCE in INPUT.
More efficient than split-string + cl-count-if for simple counting."
  (let ((count 0) (start 0))
    (while (setq start (string-search sequence input start))
      (cl-incf count)
      (cl-incf start (length sequence)))
    count))

(defun claude-code-ide--vterm-smart-renderer (orig-fun process input)
  "Smart rendering filter for optimized vterm display updates.
This advanced filter analyzes terminal output patterns to identify
rapid update sequences that benefit from batched processing.
It significantly improves visual quality during complex operations.

ORIG-FUN is the underlying filter to enhance.
PROCESS is the terminal process being optimized.
INPUT contains the terminal output stream."
  (if (or (not claude-code-ide-vterm-anti-flicker)
          (not (claude-code-ide--session-buffer-p (process-buffer process))))
      ;; Feature disabled or not a Claude buffer, pass through normally
      (funcall orig-fun process input)
    (with-current-buffer (process-buffer process)
      ;; Fast path: plain text with no active queue skips all pattern detection
      ;; This optimizes the common case of typing in the prompt
      (if (and (not claude-code-ide--vterm-render-queue)
               (not (string-search "\033" input)))
          (funcall orig-fun process input)
        ;; Detect rapid terminal redraw sequences
        ;; Pattern analysis for complex terminal updates:
        ;; - Vertical cursor movements (ESC[<n>A)
        ;; - Line clearing operations (ESC[K)
        ;; - High escape sequence density
        (let* ((complex-redraw-detected
                ;; Pattern: vertical movement + clear, repeated
                (string-match-p "\033\\[[0-9]*A.*\033\\[K.*\033\\[[0-9]*A.*\033\\[K" input))
               (clear-count (claude-code-ide--count-escape-sequence "\033[K" input))
               (escape-count (cl-count ?\033 input))
               (input-length (length input))
               ;; High escape density indicates redrawing, not normal output
               (escape-density (if (> input-length 0)
                                   (/ (float escape-count) input-length)
                                 0)))
          ;; Optimize rendering for detected patterns:
          ;; 1. Complex redraw sequence detected, OR
          ;; 2. Escape sequence density exceeds threshold with line operations
          ;; 3. OR already queuing (to complete the sequence)
          (if (or complex-redraw-detected
                  (and (> escape-density 0.3)
                       (>= clear-count 2))
                  claude-code-ide--vterm-render-queue)
              (progn
                ;; Add to queue (list for O(1) push, joined at flush time)
                (push input claude-code-ide--vterm-render-queue)
                ;; Reset existing render timer
                (when claude-code-ide--vterm-render-timer
                  (cancel-timer claude-code-ide--vterm-render-timer))
                ;; Schedule optimized rendering
                ;; Timing calibrated for visual quality
                (setq claude-code-ide--vterm-render-timer
                      (run-at-time claude-code-ide-vterm-render-delay nil
                                   (lambda (buf)
                                     (when (buffer-live-p buf)
                                       (with-current-buffer buf
                                         (when claude-code-ide--vterm-render-queue
                                           (let* ((inhibit-redisplay t)
                                                  (queue claude-code-ide--vterm-render-queue)
                                                  ;; Join list in correct order
                                                  (data (apply #'concat (nreverse queue))))
                                             ;; Clear queue first to prevent recursion
                                             (setq claude-code-ide--vterm-render-queue nil
                                                   claude-code-ide--vterm-render-timer nil)
                                             ;; Execute queued rendering
                                             (funcall orig-fun
                                                      (get-buffer-process buf)
                                                      data))))))
                                   (current-buffer))))
            ;; Standard processing for regular output
            (funcall orig-fun process input)))))))

(defvar-local claude-code-ide--saved-cursor-type nil
  "Saved cursor-type before entering vterm-copy-mode.")

(defun claude-code-ide--vterm-copy-mode-hook ()
  "Make sure cursor is visible in `vterm-copy-mode'.
Saves the current cursor-type when entering copy mode and restores it
when exiting, ensuring compatibility with evil-mode and other packages
that manage cursor appearance."
  (if vterm-copy-mode
      ;; Entering copy mode: save current cursor-type and make cursor visible
      (progn
        (setq claude-code-ide--saved-cursor-type cursor-type)
        (when (null cursor-type)
          (setq cursor-type t)))
    ;; Exiting copy mode: restore previous cursor-type
    (setq cursor-type claude-code-ide--saved-cursor-type)))

(defun claude-code-ide--configure-vterm-buffer ()
  "Configure vterm for enhanced performance and visual quality.
Establishes optimal terminal settings including rendering optimizations,
cursor management, and process buffering for superior user experience."
  ;; Disable automatic scrolling to bottom on output to prevent flickering
  (setq-local vterm-scroll-to-bottom-on-output nil)
  ;; Disable immediate redraw to batch updates and reduce flickering
  (when (boundp 'vterm--redraw-immididately)
    (setq-local vterm--redraw-immididately nil))
  ;; Try to prevent cursor flickering by disabling Emacs' own cursor management
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local blink-cursor-mode nil)
  (setq-local cursor-type nil)  ; Let vterm handle the cursor entirely
  ;; disable hl-line-mode, eliminates another source of flicker
  (setq-local global-hl-line-mode nil)
  (when (featurep 'hl-line)
    (hl-line-mode -1))
  ;; make sure the non-breaking space in the prompt isn't themed
  (face-remap-add-relative 'nobreak-space :inherit 'default)
  ;; Register hook for copy-mode cursor visibility
  (add-hook 'vterm-copy-mode-hook #'claude-code-ide--vterm-copy-mode-hook nil t)
  ;; Increase process read buffering to batch more updates together
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)
    ;; Try to make vterm read larger chunks at once
    (when (fboundp 'process-put)
      (process-put proc 'read-output-max 4096)))
  ;; Set up rendering optimization
  (when claude-code-ide-vterm-anti-flicker
    (advice-add 'vterm--filter :around #'claude-code-ide--vterm-smart-renderer)))


;;; Terminal Backend Abstraction

(defun claude-code-ide--terminal-ensure-backend ()
  "Ensure the selected terminal backend is available."
  (cond
   ((eq claude-code-ide-terminal-backend 'vterm)
    (unless (featurep 'vterm)
      (require 'vterm nil t))
    (unless (featurep 'vterm)
      (user-error "The package vterm is not installed.  Please install the vterm package or change `claude-code-ide-terminal-backend' to 'eat")))
   ((eq claude-code-ide-terminal-backend 'eat)
    (unless (featurep 'eat)
      (require 'eat nil t))
    (unless (featurep 'eat)
      (user-error "The package eat is not installed.  Please install the eat package or change `claude-code-ide-terminal-backend' to 'vterm")))
   ((eq claude-code-ide-terminal-backend 'ghostel)
    (unless (featurep 'ghostel)
      (require 'ghostel nil t))
    (unless (featurep 'ghostel)
      (user-error "The package ghostel is not installed.  Please install the ghostel package or change `claude-code-ide-terminal-backend' to `vterm' or `eat'"))
    (unless (fboundp 'ghostel-exec)
      (user-error "The installed ghostel package does not provide `ghostel-exec'.  Please update ghostel or change `claude-code-ide-terminal-backend' to `vterm' or `eat'")))
   (t
    (user-error "Invalid terminal backend: %s.  Valid options are 'vterm, 'eat, or 'ghostel" claude-code-ide-terminal-backend))))

(defun claude-code-ide--disable-ghostel-title-tracking ()
  "Disable Ghostel OSC title tracking in the current buffer."
  (cond
   ((boundp 'ghostel-set-title-function)
    (setq-local ghostel-set-title-function nil))
   ((boundp 'ghostel-enable-title-tracking)
    (setq-local ghostel-enable-title-tracking nil))))

(defun claude-code-ide--apply-ghostel-evil-escape ()
  "Set the buffer-local ESC routing for the current ghostel buffer.
Overrides `evil-ghostel--escape-mode' with
`claude-code-ide-ghostel-evil-escape' so ESC behaves as configured in
Claude Code sessions only, without touching the global
`evil-ghostel-escape' default.  A nil setting leaves the value that
`evil-ghostel-mode' derived from the global default in place.  No-op
unless `evil-ghostel-mode' is active in this buffer."
  (when (and claude-code-ide-ghostel-evil-escape
             (bound-and-true-p evil-ghostel-mode))
    (setq-local evil-ghostel--escape-mode claude-code-ide-ghostel-evil-escape)))

(defun claude-code-ide--terminal-send-string (string)
  "Send STRING to the terminal in the current buffer."
  (cond
   ((eq claude-code-ide-terminal-backend 'vterm)
    (vterm-send-string string))
   ((eq claude-code-ide-terminal-backend 'eat)
    (when eat-terminal
      (eat-term-send-string eat-terminal string)))
   ((eq claude-code-ide-terminal-backend 'ghostel)
    (if (fboundp 'ghostel-send-string)
        (ghostel-send-string string)
      (when-let ((process (get-buffer-process (current-buffer))))
        (process-send-string process string))))
   (t
    (error "Unknown terminal backend: %s" claude-code-ide-terminal-backend))))

(defun claude-code-ide--terminal-send-escape ()
  "Send escape key to the terminal in the current buffer."
  (cond
   ((eq claude-code-ide-terminal-backend 'vterm)
    (vterm-send-escape))
   ((eq claude-code-ide-terminal-backend 'eat)
    (when eat-terminal
      (eat-term-send-string eat-terminal "\e")))
   ((eq claude-code-ide-terminal-backend 'ghostel)
    (claude-code-ide--terminal-send-string "\e"))
   (t
    (error "Unknown terminal backend: %s" claude-code-ide-terminal-backend))))

(defun claude-code-ide--terminal-send-return ()
  "Send return key to the terminal in the current buffer."
  (cond
   ((eq claude-code-ide-terminal-backend 'vterm)
    (vterm-send-return))
   ((eq claude-code-ide-terminal-backend 'eat)
    (when eat-terminal
      (eat-term-send-string eat-terminal "\r")))
   ((eq claude-code-ide-terminal-backend 'ghostel)
    (claude-code-ide--terminal-send-string "\r"))
   (t
    (error "Unknown terminal backend: %s" claude-code-ide-terminal-backend))))

(defun claude-code-ide--sync-terminal-dimensions (buffer window)
  "Sync terminal dimensions in BUFFER to match WINDOW size.
This ensures the terminal process has the correct dimensions after
the buffer has been displayed in its final window, which may differ
from the window where it was initially created."
  (when (and buffer window (buffer-live-p buffer) (window-live-p window))
    (with-current-buffer buffer
      (when-let ((proc (get-buffer-process buffer)))
        (let ((height (window-body-height window))
              (width (window-body-width window)))
          (if (eq claude-code-ide-terminal-backend 'ghostel)
              (progn
                (when (fboundp 'ghostel--window-adjust-process-window-size)
                  (ghostel--window-adjust-process-window-size proc (list window)))
                (set-process-window-size proc height width))
            (set-process-window-size proc height width)))))))

(defun claude-code-ide--setup-terminal-keybindings ()
  "Set up keybindings for the Claude Code terminal buffer.
This function binds:
- M-RET (Alt-Return) to insert a newline
- C-<escape> to send escape"
  (cond
   ((eq claude-code-ide-terminal-backend 'vterm)
    ;; For vterm, we set up local keybindings in vterm-mode-map
    (local-set-key (kbd "S-<return>") #'claude-code-ide-insert-newline)
    (local-set-key (kbd "C-<escape>") #'claude-code-ide-send-escape))
   ((eq claude-code-ide-terminal-backend 'eat)
    ;; For eat, we need to modify the semi-char mode map which is the default
    ;; We use local-set-key to make it buffer-local
    (local-set-key (kbd "S-<return>") #'claude-code-ide-insert-newline)
    (local-set-key (kbd "C-<escape>") #'claude-code-ide-send-escape))
   ((eq claude-code-ide-terminal-backend 'ghostel)
    (local-set-key (kbd "S-<return>") #'claude-code-ide-insert-newline)
    (local-set-key (kbd "C-<escape>") #'claude-code-ide-send-escape))
   (t
    (error "Unknown terminal backend: %s" claude-code-ide-terminal-backend))))

;;; Terminal Reflow Glitch Prevention
;;
;; This section implements a workaround for Claude Code bug #1422
;; where terminal reflows during height-only changes can cause
;; uncontrollable scrolling. This code should be removed once
;; the upstream bug is fixed.
;; See: https://github.com/anthropics/claude-code/issues/1422

(defun claude-code-ide--terminal-resize-handler ()
  "Retrieve the terminal's resize handling function based on backend."
  (pcase claude-code-ide-terminal-backend
    ('vterm #'vterm--window-adjust-process-window-size)
    ('eat #'eat--adjust-process-window-size)
    ;; Ghostel manages resizing differently enough that the vterm/eat
    ;; reflow workaround should stay disabled for it.
    ('ghostel nil)
    (_ (error "Unsupported terminal backend: %s" claude-code-ide-terminal-backend))))

(defun claude-code-ide--terminal-scroll-mode-active-p ()
  "Determine if terminal is currently in scroll/copy mode."
  (pcase claude-code-ide-terminal-backend
    ('vterm (bound-and-true-p vterm-copy-mode))
    ('eat (not (bound-and-true-p eat--semi-char-mode)))
    (_ nil)))

(defun claude-code-ide--session-buffer-p (buffer)
  "Check if BUFFER belongs to a Claude Code session.
Recognition goes through the buffer's session backpointer rather than
its name, so custom naming schemes are recognized as well."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (buffer-local-value 'claude-code-ide--session buffer)))

(defun claude-code-ide--terminal-reflow-filter (original-fn &rest args)
  "Filter terminal reflows to prevent height-only resize triggers.
This wraps ORIGINAL-FN to suppress reflow signals unless the terminal
width has actually changed, working around the scrolling glitch."
  (let* ((base-result (apply original-fn args))
         (dimensions-stable t))
    ;; Examine each window showing a Claude session
    (dolist (win (window-list))
      (when-let* ((buf (window-buffer win))
                  ((claude-code-ide--session-buffer-p buf)))
        (let* ((new-width (window-width win))
               (cached-width (window-parameter win 'claude-code-ide-cached-width)))
          ;; Width change detected
          (unless (eql new-width cached-width)
            (setq dimensions-stable nil)
            (set-window-parameter win 'claude-code-ide-cached-width new-width)))))
    ;; Decide whether to allow reflow
    (cond
     ;; Not in a Claude buffer - pass through
     ((not (claude-code-ide--session-buffer-p (current-buffer)))
      base-result)
     ;; In scroll mode - suppress reflow
     ((claude-code-ide--terminal-scroll-mode-active-p)
      nil)
     ;; Dimensions changed - allow reflow
     ((not dimensions-stable)
      base-result)
     ;; No width change - suppress reflow
     (t nil))))


;;; Helper Functions

(defun claude-code-ide--default-buffer-name (directory)
  "Generate default buffer name for DIRECTORY."
  (format "*claude-code[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

(defun claude-code-ide--get-working-directory ()
  "Get the current working directory (project root or current directory)."
  (if-let ((project (project-current)))
      (expand-file-name (project-root project))
    (expand-file-name default-directory)))

(defun claude-code-ide--get-buffer-name (&optional directory)
  "Get the base buffer name for a Claude Code session in DIRECTORY.
If DIRECTORY is not provided, use the current working directory."
  (funcall claude-code-ide-buffer-name-function
           (or directory (claude-code-ide--get-working-directory))))

(defun claude-code-ide--instance-buffer-name (directory instance-name)
  "Compute the terminal buffer name for INSTANCE-NAME in DIRECTORY.
`claude-code-ide-buffer-name-function' is called with both arguments
when it accepts two; the documented single-argument form keeps working
and gets INSTANCE-NAME spliced into (or appended to) its result.  A nil
INSTANCE-NAME always yields the plain single-instance name."
  (let* ((fn claude-code-ide-buffer-name-function)
         (max-arity (cdr (func-arity fn))))
    (if (or (eq max-arity 'many)
            (and (numberp max-arity) (>= max-arity 2)))
        (funcall fn directory instance-name)
      (let ((base (funcall fn directory)))
        (cond
         ((null instance-name) base)
         ((string-suffix-p "]*" base)
          (concat (substring base 0 -2) ":" instance-name "]*"))
         (t (format "%s<%s>" base instance-name)))))))

(defun claude-code-ide--generate-session-id (working-dir)
  "Generate a unique session ID for a new instance in WORKING-DIR."
  (format "claude-%s-%s-%d"
          (file-name-nondirectory (directory-file-name working-dir))
          (format-time-string "%Y%m%d-%H%M%S")
          (cl-incf claude-code-ide--session-counter)))

(defun claude-code-ide--buffer-session (&optional buffer)
  "Return the session owning BUFFER (default: the current buffer), or nil."
  (buffer-local-value 'claude-code-ide--session (or buffer (current-buffer))))

(defun claude-code-ide--session-display-name (session)
  "Return SESSION's user-facing name, e.g. \"proj\" or \"proj:refactor\"."
  (let ((project (file-name-nondirectory
                  (directory-file-name
                   (claude-code-ide-mcp-session-project-dir session))))
        (name (claude-code-ide-mcp-session-instance-name session)))
    (if name (format "%s:%s" project name) project)))

(defun claude-code-ide--touch-session (session)
  "Record SESSION as the most recently used instance."
  (setf (claude-code-ide-mcp-session-last-used session) (float-time))
  (when-let ((buffer (claude-code-ide-mcp-session-buffer session)))
    (when (buffer-live-p buffer)
      (setq claude-code-ide--last-accessed-buffer buffer))))

(defun claude-code-ide--session-visible-p (session)
  "Return non-nil when SESSION's terminal has a window in the selected frame."
  (when-let ((buffer (claude-code-ide-mcp-session-buffer session)))
    (and (buffer-live-p buffer)
         (get-buffer-window buffer))))

(defun claude-code-ide--visible-project-sessions (project-dir)
  "Return the PROJECT-DIR sessions visible in the selected frame."
  (cl-remove-if-not #'claude-code-ide--session-visible-p
                    (claude-code-ide-mcp--sessions-for-project project-dir)))

(defun claude-code-ide--read-project-session (prompt project-dir)
  "Pick one of PROJECT-DIR's sessions with PROMPT via `completing-read'."
  (let* ((sessions (claude-code-ide-mcp--sessions-for-project project-dir))
         (candidates (mapcar (lambda (session)
                               (cons (claude-code-ide--session-display-name session)
                                     session))
                             sessions)))
    (cond
     ((null sessions) nil)
     ((null (cdr sessions)) (car sessions))
     (t (cdr (assoc (completing-read prompt candidates nil t) candidates))))))

(defun claude-code-ide--resolve-session (intent &optional prompt)
  "Resolve which instance a command targets; nil when the project has none.
Resolution: a prefix argument always asks via PROMPT; a Claude terminal
buffer targets itself; a project with a single instance, or a single
visible one, is unambiguous.  Beyond that, INTENT `auto' picks the most
recently used instance (echoing the choice), while INTENT `prompt' asks
— guessing is acceptable for sends, not for destructive commands."
  (let* ((project-dir (claude-code-ide--get-working-directory))
         (sessions (claude-code-ide-mcp--sessions-for-project project-dir)))
    (cond
     ((and current-prefix-arg sessions)
      (claude-code-ide--read-project-session (or prompt "Claude instance: ")
                                             project-dir))
     (claude-code-ide--session claude-code-ide--session)
     ((null sessions) nil)
     ((null (cdr sessions)) (car sessions))
     (t
      (let ((visible (claude-code-ide--visible-project-sessions project-dir)))
        (cond
         ((and visible (null (cdr visible))) (car visible))
         ((eq intent 'prompt)
          (claude-code-ide--read-project-session (or prompt "Claude instance: ")
                                                 project-dir))
         (t
          (let ((session (claude-code-ide-mcp--mru-session project-dir)))
            (when session
              (message "Claude instance: %s"
                       (claude-code-ide--session-display-name session)))
            session))))))))

(defun claude-code-ide--auto-instance-name (project-dir &optional exclude-session)
  "Return the lowest free auto name in PROJECT-DIR: nil (plain) or \"2\", \"3\"...
EXCLUDE-SESSION's own name is treated as free (used while renaming)."
  (let ((taken (mapcar #'claude-code-ide-mcp-session-instance-name
                       (cl-remove exclude-session
                                  (claude-code-ide-mcp--sessions-for-project project-dir))))
        (n 1))
    (while (if (= n 1)
               (memq nil taken)
             (member (number-to-string n) taken))
      (cl-incf n))
    (if (= n 1) nil (number-to-string n))))

(defun claude-code-ide--read-instance-name (project-dir &optional prompt exclude-session)
  "Read and validate an instance name for PROJECT-DIR.
Empty input auto-numbers (nil = the plain unnamed slot).  PROMPT
overrides the default prompt; EXCLUDE-SESSION is ignored in collision
checks (used while renaming).  Returns the name string or nil."
  (let (name done)
    (while (not done)
      (setq name (string-trim
                  (read-string (or prompt "Instance name (empty for auto): "))))
      (cond
       ((string-empty-p name)
        (setq name (claude-code-ide--auto-instance-name project-dir exclude-session)
              done t))
       ((string-match-p "\\`[0-9]+\\'" name)
        (message "Numeric names are reserved for auto-numbering")
        (sit-for 1))
       ((string-match-p "[][*[:cntrl:]]" name)
        (message "Name cannot contain [, ], or *")
        (sit-for 1))
       ((cl-some (lambda (session)
                   (and (not (eq session exclude-session))
                        (equal name (claude-code-ide-mcp-session-instance-name session))))
                 (claude-code-ide-mcp--sessions-for-project project-dir))
        (message "Name already used in this project: %s" name)
        (sit-for 1))
       (t (setq done t))))
    name))

(defconst claude-code-ide--window-slot-block 16
  "Side-window slots reserved per project.
Slots order windows along a frame side; reserving a contiguous block
per project keeps one project's instances grouped together instead of
interleaved by global creation order.")

(defun claude-code-ide--assign-window-slot (project-dir)
  "Return a side-window slot for a new instance of PROJECT-DIR.
Each project owns a block of `claude-code-ide--window-slot-block'
slots, so windows sort as emacs, emacs, src, src rather than by
creation order.  Within the block the smallest slot not used by any
live session is picked (the global check also keeps legacy
out-of-block slots collision-free)."
  (let* ((block claude-code-ide--window-slot-block)
         (siblings (cl-remove-if-not #'claude-code-ide-mcp-session-window-slot
                                     (claude-code-ide-mcp--sessions-for-project project-dir)))
         (all-slots (cl-remove nil
                               (mapcar #'claude-code-ide-mcp-session-window-slot
                                       (claude-code-ide-mcp--active-sessions)))))
    (cl-flet ((free-block-base ()
                (let ((used-bases (mapcar (lambda (slot) (floor slot block))
                                          all-slots))
                      (b 0))
                  (while (memq b used-bases)
                    (cl-incf b))
                  (* block b))))
      (let* ((base (if siblings
                       ;; Join the project's existing block
                       (* block (floor (claude-code-ide-mcp-session-window-slot
                                        (car siblings))
                                       block))
                     ;; New project: smallest block no live session occupies
                     (free-block-base)))
             (slot base))
        (while (and (< slot (+ base block))
                    (memq slot all-slots))
          (cl-incf slot))
        ;; Block exhausted: overflow into a fresh block rather than
        ;; spilling into a neighboring project's block
        (when (>= slot (+ base block))
          (setq slot (free-block-base)))
        slot))))

(defun claude-code-ide--strip-new-tab-claude-windows (&rest _)
  "Remove cloned Claude side windows from a freshly created tab.
With `tab-bar-new-tab-choice' t a new tab clones the previous tab's
layout, duplicating instance windows across tabs; each duplicate then
toggles independently, which reads as broken toggling.  New tabs start
Claude-free instead — summon instances there explicitly."
  (dolist (window (window-list))
    (when (and (window-live-p window)
               (claude-code-ide--buffer-session (window-buffer window)))
      (unless (ignore-errors (delete-window window) t)
        ;; The sole window of the tab cannot be deleted — show another
        ;; buffer in it instead
        (when (window-live-p window)
          (switch-to-prev-buffer window 'bury)
          (when (claude-code-ide--buffer-session (window-buffer window))
            (set-window-buffer window (get-buffer-create "*scratch*"))))))))

(defun claude-code-ide--note-window-selection (frame)
  "Stamp MRU state when a Claude terminal window gets selected in FRAME.
Without this, typing into a visible instance would not make it the
most recently used one — only displaying or hiding windows would."
  (when-let* ((window (frame-selected-window frame))
              (session (claude-code-ide--buffer-session (window-buffer window))))
    (unless (claude-code-ide-mcp-session-cleanup-done session)
      (setf (claude-code-ide-mcp-session-last-used session) (float-time))
      (setq claude-code-ide--last-accessed-buffer (window-buffer window)))))

(defun claude-code-ide--maybe-recommend-ghostel ()
  "Suggest the ghostel backend once when running on vterm or eat."
  (when (and claude-code-ide-show-backend-recommendation
             (not claude-code-ide--backend-recommendation-shown)
             (memq claude-code-ide-terminal-backend '(vterm eat)))
    (setq claude-code-ide--backend-recommendation-shown t)
    (message "Claude Code IDE: ghostel is the recommended terminal backend (currently using %s) — see the README; set claude-code-ide-show-backend-recommendation to nil to hide this"
             claude-code-ide-terminal-backend)))

(defun claude-code-ide--maybe-install-global-advice ()
  "Install the global terminal advice when the first instance starts."
  (when (= (hash-table-count claude-code-ide-mcp--sessions) 1)
    (add-hook 'window-selection-change-functions
              #'claude-code-ide--note-window-selection)
    (when (boundp 'tab-bar-tab-post-open-functions)
      (add-hook 'tab-bar-tab-post-open-functions
                #'claude-code-ide--strip-new-tab-claude-windows))
    (when-let ((resize-handler
                (and claude-code-ide-prevent-reflow-glitch
                     (claude-code-ide--terminal-resize-handler))))
      (advice-add resize-handler
                  :around #'claude-code-ide--terminal-reflow-filter))))

(defun claude-code-ide--cleanup-dead-sessions ()
  "Fully clean up sessions whose terminal process has died."
  (dolist (session (claude-code-ide-mcp--active-sessions))
    (let ((process (claude-code-ide-mcp-session-process session)))
      (when (and process (not (process-live-p process)))
        (claude-code-ide--cleanup-session session)))))

(defun claude-code-ide--cleanup-all-sessions ()
  "Clean up all active Claude Code sessions."
  (dolist (session (claude-code-ide-mcp--active-sessions))
    (claude-code-ide--cleanup-session session)))

;; Ensure cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-code-ide--cleanup-all-sessions)

(defun claude-code-ide--display-buffer-in-side-window (buffer)
  "Display BUFFER in a side window according to customization.
The window is displayed on the side specified by
`claude-code-ide-window-side' with dimensions from
`claude-code-ide-window-width' or `claude-code-ide-window-height'.
If `claude-code-ide-focus-on-open' is non-nil, the window is selected."
  (let ((window
         (if claude-code-ide-use-side-window
             ;; Use side window
             (let* ((side claude-code-ide-window-side)
                    ;; Each instance owns a slot, so several instances can
                    ;; be visible side by side instead of evicting each other
                    (slot (or (when-let ((session (claude-code-ide--buffer-session buffer)))
                                (claude-code-ide-mcp-session-window-slot session))
                              0))
                    (window-parameters '((no-delete-other-windows . t)))
                    (display-buffer-alist
                     `((,(regexp-quote (buffer-name buffer))
                        (display-buffer-in-side-window)
                        (side . ,side)
                        (slot . ,slot)
                        ,@(when (memq side '(left right))
                            `((window-width
                               . ,(lambda (win)
                                    (let ((delta (- claude-code-ide-window-width
                                                    (window-body-width win))))
                                      (unless (zerop delta)
                                        (window-resize win delta t)))))))
                        ,@(when (memq side '(top bottom))
                            `((window-height . ,claude-code-ide-window-height)))
                        (window-parameters . ,window-parameters)))))
               (display-buffer buffer))
           ;; Use regular buffer
           (display-buffer buffer))))
    ;; Update last accessed buffer whenever we display a Claude buffer
    (setq claude-code-ide--last-accessed-buffer buffer)
    (when-let ((session (claude-code-ide--buffer-session buffer)))
      (setf (claude-code-ide-mcp-session-last-used session) (float-time)))
    ;; Select the window to give it focus if configured to do so
    (when (and window claude-code-ide-focus-on-open)
      (select-window window))
    ;; For bottom/top windows, explicitly set and preserve the height
    (when (and window
               claude-code-ide-use-side-window
               (memq claude-code-ide-window-side '(top bottom)))
      (set-window-text-height window claude-code-ide-window-height))
    ;; Dedicate every Claude side window so unrelated display-buffer calls
    ;; don't reuse a visible instance's window
    (when (and window claude-code-ide-use-side-window)
      (set-window-dedicated-p window t))
    ;; Sync terminal dimensions with the actual window size
    ;; This is necessary because vterm/eat may have been created with
    ;; different dimensions before being displayed in this window
    (when window
      (claude-code-ide--sync-terminal-dimensions buffer window))
    window))

(defun claude-code-ide--cleanup-session (session &optional buffer-dying)
  "Clean up all state of SESSION when its Claude instance exits.
Idempotent per session: the sentinel, the terminal's kill-buffer hook
and the kill-emacs sweep can all arrive here, and overlapping teardowns
of SIBLING sessions must still proceed — a global in-progress flag
would silently skip them.

BUFFER-DYING non-nil means we were called from the terminal buffer's
own `kill-buffer-hook', so the buffer must NOT be killed again: a
nested kill would destroy it before the backend's remaining kill hooks
run, leaving them to fire on a dead buffer (ghostel's native-process
hook signals wrong-type-argument that way)."
  (when (and session
             (not (claude-code-ide-mcp-session-cleanup-done session)))
    (setf (claude-code-ide-mcp-session-cleanup-done session) t)
    ;; Close this session's open diffs first; their deferred verdicts can
    ;; never be delivered once the instance is gone, and the ediff
    ;; buffers/window configs would otherwise be orphaned
    (when-let ((active-diffs (claude-code-ide-mcp-session-active-diffs session)))
      (let (tab-names)
        (maphash (lambda (tab-name _diff-info) (push tab-name tab-names))
                 active-diffs)
        (dolist (tab-name tab-names)
          (condition-case err
              (claude-code-ide-mcp--cleanup-diff tab-name session)
            (error
             (claude-code-ide-debug "Error cleaning up diff %s: %s" tab-name err))))))
    ;; Stop this instance's MCP server (closes the websocket server,
    ;; removes its lockfile, deregisters it).  Best effort: a failure
    ;; here must not wedge the session half-cleaned.
    (condition-case err
        (claude-code-ide-mcp--stop-session session)
      (error
       (claude-code-ide-debug "Error stopping MCP session during cleanup: %s" err)))
    ;; Remove global advice and hooks when the last instance is gone.
    ;; Advice removal ignores the current customization values — they may
    ;; have changed since installation, and removing uninstalled advice is
    ;; a harmless no-op.
    (when (= (hash-table-count claude-code-ide-mcp--sessions) 0)
      (remove-hook 'window-selection-change-functions
                   #'claude-code-ide--note-window-selection)
      (when (boundp 'tab-bar-tab-post-open-functions)
        (remove-hook 'tab-bar-tab-post-open-functions
                     #'claude-code-ide--strip-new-tab-claude-windows))
      (dolist (fn '(vterm--window-adjust-process-window-size
                    eat--adjust-process-window-size))
        (when (fboundp fn)
          (advice-remove fn #'claude-code-ide--terminal-reflow-filter)))
      (when (fboundp 'vterm--filter)
        (advice-remove 'vterm--filter #'claude-code-ide--vterm-smart-renderer)))
    ;; Notify MCP tools server about session end
    (claude-code-ide-mcp-server-session-ended
     (claude-code-ide-mcp-session-session-id session))
    ;; Kill this session's own terminal buffer.  Never a name lookup: the
    ;; canonical name could resolve to a sibling instance's buffer.
    (let ((buffer (claude-code-ide-mcp-session-buffer session)))
      (when (and buffer (buffer-live-p buffer) (not buffer-dying))
        (let ((kill-buffer-hook nil) ; Disable hooks to prevent recursion
              (kill-buffer-query-functions nil)) ; Don't ask for confirmation
          (kill-buffer buffer))
        ;; The kill above ran with hooks disabled, so the backend never got
        ;; a chance to terminate the CLI process — do it explicitly.  When
        ;; BUFFER-DYING, the backend's own kill hooks handle the process.
        (let ((process (claude-code-ide-mcp-session-process session)))
          (when (and process (process-live-p process))
            (ignore-errors (delete-process process)))))
      ;; Repoint the global MRU when it pointed at the killed buffer
      (when (eq claude-code-ide--last-accessed-buffer buffer)
        (setq claude-code-ide--last-accessed-buffer
              (cl-loop for s in (claude-code-ide-mcp--active-sessions)
                       for b = (claude-code-ide-mcp-session-buffer s)
                       when (and b (buffer-live-p b)) return b))))
    (claude-code-ide-debug "Cleaned up Claude Code session %s"
                           (claude-code-ide--session-display-name session))))

;;; CLI Detection

(defun claude-code-ide--detect-cli ()
  "Detect if Claude Code CLI is available."
  (let ((available (condition-case nil
                       (eq (call-process claude-code-ide-cli-path nil nil nil "--version") 0)
                     (error nil))))
    (setq claude-code-ide--cli-available available)))

(defun claude-code-ide--ensure-cli ()
  "Ensure Claude Code CLI is available, detect if needed."
  (unless claude-code-ide--cli-available
    (claude-code-ide--detect-cli))
  claude-code-ide--cli-available)

;;; Commands

(defun claude-code-ide--toggle-existing-window (session)
  "Toggle visibility of SESSION's terminal window.
If the window is visible, it will be hidden.
If the window is not visible, it will be shown in a side window."
  (let* ((buffer (claude-code-ide-mcp-session-buffer session))
         (window (and buffer (get-buffer-window buffer))))
    (if window
        ;; Window is visible, hide it
        (progn
          ;; Track this instance as last accessed when closing
          (claude-code-ide--touch-session session)
          (delete-window window)
          (claude-code-ide-debug "Claude Code window hidden"))
      ;; Window is not visible, show it
      (progn
        (claude-code-ide--display-buffer-in-side-window buffer)
        ;; Update the original tab when showing the window
        (when (fboundp 'tab-bar--current-tab)
          (setf (claude-code-ide-mcp-session-original-tab session) (tab-bar--current-tab)))
        (claude-code-ide-debug "Claude Code window shown")))))

(defun claude-code-ide--build-claude-command (&optional continue resume session-id)
  "Build the Claude command with optional flags.
If CONTINUE is non-nil, add the -c flag.
If RESUME is non-nil, add the -r flag.
If SESSION-ID is provided, it's included in the MCP server URL path.
If `claude-code-ide-cli-debug' is non-nil, add the -d flag.
If `claude-code-ide-system-prompt' is non-nil, add the --append-system-prompt flag.
Additional flags from `claude-code-ide-cli-extra-flags' are also included."
  (let ((claude-cmd claude-code-ide-cli-path))
    ;; Add debug flag if enabled
    (when claude-code-ide-cli-debug
      (setq claude-cmd (concat claude-cmd " -d")))
    ;; Add resume flag if requested
    (when resume
      (setq claude-cmd (concat claude-cmd " -r")))
    ;; Add continue flag if requested
    (when continue
      (setq claude-cmd (concat claude-cmd " -c")))
    ;; Add append-system-prompt flag with Emacs context
    (let ((emacs-prompt "IMPORTANT: Connected to Emacs via claude-code-ide.el integration. Emacs uses mixed coordinates: Lines: 1-based (line 1 = first line), Columns: 0-based (column 0 = first column). Example: First character in file is at line 1, column 0. Available: xref (LSP), tree-sitter, imenu, project.el, flycheck/flymake diagnostics. Context-aware with automatic project/file/selection tracking.")
          (combined-prompt nil))
      ;; Always include the Emacs-specific prompt
      (setq combined-prompt emacs-prompt)
      ;; Append user's custom prompt if set
      (when claude-code-ide-system-prompt
        (setq combined-prompt (concat combined-prompt "\n\n" claude-code-ide-system-prompt)))
      ;; Add the combined prompt to the command
      (setq claude-cmd (concat claude-cmd " --append-system-prompt "
                               (shell-quote-argument combined-prompt))))
    ;; Add any extra flags
    (when (and claude-code-ide-cli-extra-flags
               (not (string-empty-p claude-code-ide-cli-extra-flags)))
      (setq claude-cmd (concat claude-cmd " " claude-code-ide-cli-extra-flags)))
    ;; Add MCP tools config if enabled
    (when (claude-code-ide-mcp-server-ensure-server)
      (when-let ((config (claude-code-ide-mcp-server-get-config session-id)))
        (let ((json-str (json-encode config)))
          (claude-code-ide-debug "MCP tools config JSON: %s" json-str)
          ;; For vterm, we need to escape for sh -c context
          ;; First escape backslashes, then quotes
          (setq json-str (replace-regexp-in-string "\\\\" "\\\\\\\\" json-str))
          (setq json-str (replace-regexp-in-string "\"" "\\\\\"" json-str))
          (setq claude-cmd (concat claude-cmd " --mcp-config \"" json-str "\""))
          ;; Add allowedTools flag if configured
          (let ((allowed-tools
                 (cond
                  ;; Auto mode: get all emacs-tools names
                  ((eq claude-code-ide-mcp-allowed-tools 'auto)
                   (mapconcat 'identity (claude-code-ide-mcp-server-get-tool-names "mcp__emacs-tools__") " "))
                  ;; List of specific tools
                  ((listp claude-code-ide-mcp-allowed-tools)
                   (mapconcat 'identity claude-code-ide-mcp-allowed-tools " "))
                  ;; String pattern or nil
                  (t claude-code-ide-mcp-allowed-tools))))
            (when allowed-tools
              (setq claude-cmd (concat claude-cmd " --allowedTools " allowed-tools)))))))
    claude-cmd))

(defun claude-code-ide--terminal-position-keeper (window-list)
  "Maintain stable terminal view position across window switches.
WINDOW-LIST contains windows requiring position synchronization.
Implements intelligent scroll management to preserve user context
when navigating between terminal and other buffers."
  (dolist (win window-list)
    (if (eq win 'buffer)
        ;; Direct buffer point update
        (goto-char (eat-term-display-cursor eat-terminal))
      ;; Window-specific position management
      (unless buffer-read-only  ; Skip when terminal is in navigation mode
        (let ((terminal-point (eat-term-display-cursor eat-terminal)))
          ;; Update window point to match terminal state
          (set-window-point win terminal-point)
          ;; Apply smart positioning strategy
          (cond
           ;; Terminal at bottom: maintain bottom alignment for active prompts
           ((>= terminal-point (- (point-max) 2))
            (with-selected-window win
              (goto-char terminal-point)
              (recenter -1)))  ; Pin to bottom
           ;; Terminal out of view: restore visibility
           ((not (pos-visible-in-window-p terminal-point win))
            (with-selected-window win
              (goto-char terminal-point)
              (recenter)))))))))

(defun claude-code-ide--parse-command-string (command-string)
  "Parse a command string into (program . args) for terminal exec APIs.
COMMAND-STRING is a shell command line to parse.
Returns a cons cell (program . args) where program is the executable
and args is a list of arguments."
  (let ((parts (split-string-shell-command command-string)))
    (cons (car parts) (cdr parts))))

(defun claude-code-ide--resolve-program (program)
  "Return an absolute path for PROGRAM when it resolves locally.
A bare name is looked up via `exec-path'; a name with a directory
component is expanded relative to `default-directory'.  Returns
PROGRAM unchanged when the lookup fails, leaving the missing
executable for the terminal backend to report.

Ghostel's native PTY path (ghostel 0.35.x) execs PROGRAM against the
raw process environment, whose PATH can be missing `exec-path'
entries that the rest of the package resolves against; passing an
absolute path sidesteps that lookup."
  (or (if (file-name-directory program)
          (expand-file-name program)
        (executable-find program))
      program))


(defun claude-code-ide--create-terminal-session (buffer-name working-dir port continue resume session-id)
  "Create a new terminal session for Claude Code.
BUFFER-NAME is the name for the terminal buffer.
WORKING-DIR is the working directory.
PORT is the MCP server port.
CONTINUE is whether to continue the most recent conversation.
RESUME is whether to resume a previous conversation.
SESSION-ID is the unique identifier for this session.

Returns a cons cell of (buffer . process) on success.
Signals an error if terminal fails to initialize."
  ;; Ensure terminal backend is available before proceeding
  (claude-code-ide--terminal-ensure-backend)
  (let* ((claude-cmd (claude-code-ide--build-claude-command continue resume session-id))
         (default-directory working-dir)
         (env-vars (append (list (format "CLAUDE_CODE_SSE_PORT=%d" port)
                                 "TERM_PROGRAM=emacs"
                                 "FORCE_CODE_TERMINAL=true")
                           (when claude-code-ide-no-flicker
                             (list "CLAUDE_CODE_NO_FLICKER=1")))))
    ;; Log the command for debugging
    (claude-code-ide-debug "Starting Claude with command: %s" claude-cmd)
    (claude-code-ide-debug "Working directory: %s" working-dir)
    (claude-code-ide-debug "Environment: CLAUDE_CODE_SSE_PORT=%d" port)
    (claude-code-ide-debug "Session ID: %s" session-id)
    (claude-code-ide-debug "Terminal backend: %s" claude-code-ide-terminal-backend)

    ;; A buffer created here is unowned until this function returns: the
    ;; session's buffer slot is still nil, so the caller's error path could
    ;; never reap it.  Track it and destroy it if construction fails.
    (let ((created-buffer nil))
      (condition-case err
          (cond
           ;; vterm backend
           ((eq claude-code-ide-terminal-backend 'vterm)
            (let* ((vterm-buffer-name buffer-name)
                   ;; Set vterm-shell to run Claude directly
                   (vterm-shell claude-cmd)
                   ;; vterm uses vterm-environment for passing env vars
                   (vterm-environment (append env-vars vterm-environment)))
              ;; Create vterm buffer without switching to it
              (let ((buffer (save-window-excursion
                              (vterm vterm-buffer-name))))
                (setq created-buffer buffer)
                ;; Check if vterm successfully created a buffer
                (unless buffer
                  (error "Failed to create vterm buffer.  Please ensure vterm is properly installed and compiled"))
                ;; Configure vterm buffer for optimal performance
                (with-current-buffer buffer
                  (claude-code-ide--configure-vterm-buffer))
                ;; Get the process that vterm created
                (let ((process (get-buffer-process buffer)))
                  (unless process
                    (error "Failed to get vterm process.  The vterm module may not be compiled correctly"))
                  ;; Check if buffer is still alive
                  (unless (buffer-live-p buffer)
                    (error "Vterm buffer was killed during initialization"))
                  (cons buffer process)))))

           ;; eat backend
           ((eq claude-code-ide-terminal-backend 'eat)
            (let* ((buffer (get-buffer-create buffer-name))
                   (eat-term-name "xterm-256color")
                   ;; Parse command string into program and args
                   (cmd-parts (claude-code-ide--parse-command-string claude-cmd))
                   (program (car cmd-parts))
                   (args (cdr cmd-parts)))
              (setq created-buffer buffer)
              (with-current-buffer buffer
                ;; Set up eat mode
                (unless (eq major-mode 'eat-mode)
                  (eat-mode))
                ;; Configure position preservation if enabled
                (when claude-code-ide-eat-preserve-position
                  (setq-local eat--synchronize-scroll-function
                              #'claude-code-ide--terminal-position-keeper))
                ;; Prepend our env vars to the buffer-local process-environment
                (setq-local process-environment
                            (append env-vars process-environment))
                (eat-exec buffer buffer-name program nil args)
                ;; Get the process
                (let ((process (get-buffer-process buffer)))
                  (unless process
                    (error "Failed to create eat process.  Please ensure eat is properly installed"))
                  (cons buffer process)))))

           ;; ghostel backend
           ((eq claude-code-ide-terminal-backend 'ghostel)
            (let* ((cmd-parts (claude-code-ide--parse-command-string claude-cmd))
                   (program (claude-code-ide--resolve-program (car cmd-parts)))
                   (args (cdr cmd-parts))
                   (buffer nil))
              (when-let ((stale-buffer (get-buffer buffer-name)))
                ;; A buffer with a live session backpointer is a running sibling
                ;; instance, not a stale leftover — never kill it.
                (when (claude-code-ide--buffer-session stale-buffer)
                  (error "Buffer %s already belongs to a running Claude Code instance"
                         buffer-name))
                (kill-buffer stale-buffer))
              (setq buffer (get-buffer-create buffer-name))
              (setq created-buffer buffer)
              (unless (buffer-live-p buffer)
                (error "Failed to create ghostel buffer.  Please ensure ghostel is properly installed"))
              (with-current-buffer buffer
                (setq default-directory working-dir)
                (claude-code-ide--disable-ghostel-title-tracking)
                ;; Let `claude-code-ide--cleanup-session' be the single place that
                ;; kills the buffer.  Otherwise ghostel's sentinel kills the buffer
                ;; first, firing `kill-buffer-hook' → cleanup-session, and then our
                ;; wrapping sentinel runs cleanup-session a second time.
                (setq-local ghostel-kill-buffer-on-exit nil)
                (let* ((process-environment (append env-vars process-environment))
                       (process (ghostel-exec buffer program args)))
                  ;; `ghostel-exec' switches the buffer into `ghostel-mode', which
                  ;; resets buffer-local variables.
                  (claude-code-ide--disable-ghostel-title-tracking)
                  (setq-local ghostel-kill-buffer-on-exit nil)
                  ;; `ghostel-mode-hook' has run by now, so if `evil-ghostel-mode'
                  ;; is enabled it has already seeded `evil-ghostel--escape-mode'
                  ;; from the global default; override it for this buffer only.
                  (claude-code-ide--apply-ghostel-evil-escape)
                  (unless process
                    (error "Failed to create ghostel process.  Please ensure ghostel is properly installed"))
                  ;; Chain ghostel's own sentinel so its buffer-local timers,
                  ;; focus-change hook, and `ghostel-exit-functions' still run
                  ;; when `claude-code-ide--start-session' installs its own
                  ;; sentinel on top.  Without this, ghostel teardown is skipped.
                  (process-put process 'claude-code-ide--ghostel-sentinel
                               (process-sentinel process))
                  (cons buffer process)))))

           (t
            (error "Unknown terminal backend: %s" claude-code-ide-terminal-backend)))
        ((error quit)
         (when (and created-buffer (buffer-live-p created-buffer))
           (when-let ((process (get-buffer-process created-buffer)))
             (ignore-errors (delete-process process)))
           (let ((kill-buffer-hook nil)
                 (kill-buffer-query-functions nil))
             (ignore-errors (kill-buffer created-buffer))))
         (signal (car err) (cdr err)))))))

(defun claude-code-ide--start-session (&optional continue resume)
  "Start a new Claude Code instance for the current project.
If CONTINUE is non-nil, start Claude with the -c (continue) flag.
If RESUME is non-nil, start Claude with the -r (resume) flag.

Always creates a new instance; a project may run any number of them
concurrently.  When the project already has instances (or with a
prefix argument), prompts for an optional instance name.

This function handles:
- CLI availability checking
- Dead session cleanup
- New session creation with a per-instance MCP server
- Process and buffer lifecycle management"
  (unless (claude-code-ide--ensure-cli)
    (user-error "Claude Code CLI not available.  Please install it and ensure it's in PATH"))

  ;; Clean up any dead sessions first
  (claude-code-ide--cleanup-dead-sessions)

  ;; Ensure the selected terminal backend is available before starting MCP
  (claude-code-ide--terminal-ensure-backend)

  (let* ((working-dir (claude-code-ide--get-working-directory))
         ;; Additional instances get an optional name; a prefix argument
         ;; offers the prompt for the first instance too
         (instance-name (when (or (claude-code-ide-mcp--sessions-for-project working-dir)
                                  current-prefix-arg)
                          (claude-code-ide--read-instance-name working-dir)))
         ;; A live instance may already own the base name — e.g. two
         ;; projects sharing a basename both render as *claude-code[proj]*.
         ;; Uniquify instead of clobbering or refusing.
         (buffer-name (let* ((base (claude-code-ide--instance-buffer-name working-dir instance-name))
                             (existing (get-buffer base)))
                        (if (and existing (claude-code-ide--buffer-session existing))
                            (generate-new-buffer-name base)
                          base)))
         (session-id (claude-code-ide--generate-session-id working-dir))
         (session nil)
         (registered nil))
    (condition-case err
        (progn
          ;; Start this instance's MCP server
          (setq session (claude-code-ide-mcp-create-session working-dir session-id instance-name))
          (setf (claude-code-ide-mcp-session-window-slot session)
                (claude-code-ide--assign-window-slot working-dir))
          ;; Register with the MCP tools server BEFORE spawning the CLI so
          ;; an early /mcp/<session-id> request finds its context
          (claude-code-ide-mcp-server-session-started session-id working-dir nil)
          (setq registered t)
          (let* ((port (claude-code-ide-mcp-session-port session))
                 (buffer-and-process (claude-code-ide--create-terminal-session
                                      buffer-name working-dir port continue resume session-id))
                 (buffer (car buffer-and-process))
                 (process (cdr buffer-and-process)))
            (setf (claude-code-ide-mcp-session-buffer session) buffer
                  (claude-code-ide-mcp-session-process session) process)
            (claude-code-ide-mcp-server-update-session-buffer session-id buffer)
            (with-current-buffer buffer
              (setq-local claude-code-ide--session session))
            ;; Install global terminal advice for the first live instance
            (claude-code-ide--maybe-install-global-advice)
            ;; Set up process sentinel to clean up when Claude exits.
            ;; The ghostel backend stashes its native sentinel on the
            ;; process so we can chain it here — otherwise ghostel's
            ;; buffer-local timers and focus-change hook never tear down.
            (let ((prev-sentinel (process-get process 'claude-code-ide--ghostel-sentinel)))
              (set-process-sentinel process
                                    (lambda (proc event)
                                      (when prev-sentinel
                                        (ignore-errors (funcall prev-sentinel proc event)))
                                      ;; Check for abnormal exit with error code
                                      (when (string-match "exited abnormally with code \\([0-9]+\\)" event)
                                        (let ((exit-code (match-string 1 event)))
                                          (claude-code-ide-debug "Claude process exited with code %s, event: %s"
                                                                 exit-code event)
                                          (message "Claude exited with error code %s" exit-code)))
                                      (when (or (string-match "finished" event)
                                                (string-match "exited" event)
                                                (string-match "killed" event)
                                                (string-match "terminated" event))
                                        (claude-code-ide--cleanup-session session)))))
            ;; Also add buffer kill hook as a backup.  The buffer-dying
            ;; flag stops cleanup from nested-killing the buffer while
            ;; the backend's own kill hooks still have to run on it.
            (with-current-buffer buffer
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (claude-code-ide--cleanup-session session 'buffer-dying))
                        nil t)
              ;; Set up terminal keybindings
              (claude-code-ide--setup-terminal-keybindings)
              ;; Add terminal-specific exit hooks
              (cond
               ((eq claude-code-ide-terminal-backend 'vterm)
                ;; Add vterm exit hook to ensure buffer is killed when process exits
                ;; vterm runs Claude directly, no shell involved
                (add-hook 'vterm-exit-functions
                          (lambda (&rest _)
                            (when (buffer-live-p buffer)
                              (kill-buffer buffer)))
                          nil t))
               ((eq claude-code-ide-terminal-backend 'eat)
                ;; eat uses kill-buffer-on-exit variable
                (setq-local eat-kill-buffer-on-exit t))))
            ;; Stabilization period for terminal layout initialization
            (sleep-for claude-code-ide-terminal-initialization-delay)
            ;; The CLI can die within the stabilization delay (e.g. it
            ;; failed to exec in the terminal backend's environment), in
            ;; which case the exit sentinel has already killed the buffer.
            ;; Displaying the dead buffer would surface only as a cryptic
            ;; wrong-type-argument, so fail with a real explanation.
            (unless (and (buffer-live-p buffer) (process-live-p process))
              (error "Claude Code exited immediately after startup.  Verify that `claude-code-ide-cli-path' (%s) is executable in the %s backend's environment"
                     claude-code-ide-cli-path claude-code-ide-terminal-backend))
            ;; Display the buffer in a side window
            (claude-code-ide--display-buffer-in-side-window buffer)
            (claude-code-ide-log "Claude Code %sstarted in %s with MCP on port %d%s"
                                 (cond (continue "continued and ")
                                       (resume "resumed and ")
                                       (t ""))
                                 (claude-code-ide--session-display-name session)
                                 port
                                 (if claude-code-ide-cli-debug " (debug mode enabled)" ""))
            ;; Delayed so the startup message above stays readable first
            (run-with-timer 2 nil #'claude-code-ide--maybe-recommend-ghostel)))
      ((error quit)
       ;; Session creation failed (or was quit) - tear down only THIS
       ;; instance; a directory-wide stop would kill sibling instances'
       ;; servers.  Once a terminal buffer exists, the full per-session
       ;; cleanup also kills it and its process — leaving it around would
       ;; orphan a running CLI nothing tracks anymore.
       (unless (and session (claude-code-ide-mcp-session-cleanup-done session))
         (if (and session (claude-code-ide-mcp-session-buffer session))
             (claude-code-ide--cleanup-session session)
           (when registered
             (claude-code-ide-mcp-server-session-ended session-id))
           (when session
             (claude-code-ide-mcp--stop-session session))))
       ;; Re-signal the error with improved message
       (signal (car err) (cdr err))))))

;;;###autoload
(defun claude-code-ide ()
  "Start a new Claude Code instance for the current project or directory.
Always creates a new instance; a project may run several concurrently.
When the project already has instances (or with a prefix argument),
prompts for an optional instance name.  Use `claude-code-ide-toggle',
`claude-code-ide-switch-to-buffer' or `claude-code-ide-list-sessions'
to reach running instances."
  (interactive)
  (claude-code-ide--start-session))

;;;###autoload
(defun claude-code-ide-resume ()
  "Resume Claude Code in a new instance for the current project or directory.
This starts Claude with the -r (resume) flag to continue a previous
conversation.  Always creates a new instance."
  (interactive)
  (claude-code-ide--start-session nil t))

;;;###autoload
(defun claude-code-ide-continue ()
  "Continue the most recent Claude Code conversation in a new instance.
This starts Claude with the -c (continue) flag to continue the most
recent conversation in the current directory.  Always creates a new
instance; a second continued instance forks the same conversation."
  (interactive)
  (claude-code-ide--start-session t))

;;;###autoload
(defun claude-code-ide-check-status ()
  "Check Claude Code CLI status."
  (interactive)
  (claude-code-ide--detect-cli)
  (if claude-code-ide--cli-available
      (let ((version-output
             (with-temp-buffer
               (call-process claude-code-ide-cli-path nil t nil "--version")
               (buffer-string))))
        (claude-code-ide-log "Claude Code CLI version: %s" (string-trim version-output)))
    (claude-code-ide-log "Claude Code is not installed.")))

;;;###autoload
(defun claude-code-ide-stop ()
  "Stop a Claude Code instance of the current project or directory.
Stopping is destructive, so the target is never guessed: the current
terminal buffer, the sole instance, or the sole visible one is
unambiguous; otherwise (and with a prefix argument) a completing-read
picks the instance."
  (interactive)
  (let ((session (claude-code-ide--resolve-session 'prompt "Stop Claude instance: ")))
    (if (not session)
        (claude-code-ide-log "No Claude Code session is running in this directory")
      (let ((display-name (claude-code-ide--session-display-name session))
            (buffer (claude-code-ide-mcp-session-buffer session)))
        (if (and buffer (buffer-live-p buffer))
            ;; Kill the buffer (cleanup will be handled by hooks)
            ;; The process sentinel will handle cleanup when the process dies
            (kill-buffer buffer)
          (claude-code-ide--cleanup-session session))
        (claude-code-ide-log "Stopping Claude Code %s..." display-name)))))

;;;###autoload
(defun claude-code-ide-stop-all (&optional all-projects)
  "Stop every Claude Code instance in the current project.
With prefix argument ALL-PROJECTS, stop the instances of all projects."
  (interactive "P")
  (let ((sessions (if all-projects
                      (claude-code-ide-mcp--active-sessions)
                    (claude-code-ide-mcp--sessions-for-project
                     (claude-code-ide--get-working-directory)))))
    (if (null sessions)
        (claude-code-ide-log "No Claude Code sessions to stop")
      (when (y-or-n-p (if all-projects
                          (format "Stop all %d Claude Code instance%s? "
                                  (length sessions)
                                  (if (cdr sessions) "s" ""))
                        (format "Stop %d Claude Code instance%s in %s? "
                                (length sessions)
                                (if (cdr sessions) "s" "")
                                (file-name-nondirectory
                                 (directory-file-name
                                  (claude-code-ide--get-working-directory))))))
        ;; One instance's teardown error must not strand the rest
        (dolist (session sessions)
          (condition-case err
              (let ((buffer (claude-code-ide-mcp-session-buffer session)))
                (if (and buffer (buffer-live-p buffer))
                    (kill-buffer buffer)
                  (claude-code-ide--cleanup-session session)))
            (error
             (claude-code-ide-log "Error stopping %s: %s"
                                  (claude-code-ide--session-display-name session)
                                  (error-message-string err)))))
        (claude-code-ide-log "Stopped %d Claude Code instance%s"
                             (length sessions)
                             (if (cdr sessions) "s" ""))))))

;;;###autoload
(defun claude-code-ide-rename-session ()
  "Rename a Claude Code instance of the current project.
The name shows up in the buffer name, session lists and prompts.
Empty input converts the instance to the lowest free auto number."
  (interactive)
  (let ((session (claude-code-ide--resolve-session 'prompt "Rename Claude instance: ")))
    (unless session
      (user-error "No Claude Code session for this project"))
    (let* ((project-dir (claude-code-ide-mcp-session-project-dir session))
           (old-name (claude-code-ide--session-display-name session))
           (new-name (claude-code-ide--read-instance-name
                      project-dir
                      (format "Rename %s to (empty for auto number): " old-name)
                      session)))
      (setf (claude-code-ide-mcp-session-instance-name session) new-name)
      (when-let ((buffer (claude-code-ide-mcp-session-buffer session)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (rename-buffer (claude-code-ide--instance-buffer-name project-dir new-name) t))))
      (claude-code-ide-log "Renamed Claude session %s to %s"
                           old-name
                           (claude-code-ide--session-display-name session)))))

;;;###autoload
(defun claude-code-ide-switch-to-buffer ()
  "Switch to a Claude Code buffer of the current project.
With several instances the most recently used one is chosen (a prefix
argument picks explicitly).  The window is always focused: explicit
navigation overrides `claude-code-ide-focus-on-open'."
  (interactive)
  (let ((session (claude-code-ide--resolve-session 'auto "Switch to Claude instance: ")))
    (unless session
      (user-error "No Claude Code session for this project.  Use M-x claude-code-ide to start one"))
    (let ((buffer (claude-code-ide-mcp-session-buffer session)))
      (if-let ((window (and buffer (get-buffer-window buffer))))
          ;; Buffer is visible, just focus it
          (select-window window)
        ;; Buffer exists but not visible, display it and focus it
        (when-let ((window (claude-code-ide--display-buffer-in-side-window buffer)))
          (select-window window))))))

;;;###autoload
(defun claude-code-ide-list-sessions ()
  "List all active Claude Code instances and switch to the selected one."
  (interactive)
  (claude-code-ide--cleanup-dead-sessions)
  (let* ((sessions (sort (claude-code-ide-mcp--active-sessions)
                         (lambda (a b)
                           (> (or (claude-code-ide-mcp-session-last-used a) 0)
                              (or (claude-code-ide-mcp-session-last-used b) 0)))))
         (candidates
          (mapcar (lambda (session)
                    (cons (format "%s — %s (%s)"
                                  (claude-code-ide--session-display-name session)
                                  (abbreviate-file-name
                                   (claude-code-ide-mcp-session-project-dir session))
                                  (if (claude-code-ide-mcp-session-client session)
                                      "connected" "waiting"))
                          session))
                  sessions)))
    (if candidates
        (let* ((choice (completing-read "Switch to Claude Code session: "
                                        candidates nil t))
               (session (cdr (assoc choice candidates))))
          (when session
            (let ((buffer (claude-code-ide-mcp-session-buffer session)))
              (if (and buffer (buffer-live-p buffer))
                  (when-let ((window (claude-code-ide--display-buffer-in-side-window buffer)))
                    (select-window window))
                (user-error "Buffer for session %s no longer exists" choice)))))
      (claude-code-ide-log "No active Claude Code sessions"))))

;;;###autoload
(defun claude-code-ide-insert-at-mentioned ()
  "Insert selected text into a Claude prompt.
Targets one instance: the current terminal, the sole or sole-visible
instance, else the most recently used one (prefix argument picks)."
  (interactive)
  (let ((session (claude-code-ide--resolve-session 'auto "Send selection to Claude instance: ")))
    (if (and session (claude-code-ide-mcp-session-client session))
        (progn
          (claude-code-ide-mcp-send-at-mentioned session)
          (claude-code-ide-debug "Sent selection to Claude Code"))
      (user-error "Claude Code is not connected.  Please start Claude Code first"))))

;;;###autoload
(defun claude-code-ide-send-escape ()
  "Send escape key to a Claude Code terminal of the current project.
Inside a Claude terminal buffer it always targets that instance."
  (interactive)
  (let ((session (claude-code-ide--resolve-session 'auto "Send ESC to Claude instance: ")))
    (if-let ((buffer (and session (claude-code-ide-mcp-session-buffer session))))
        (with-current-buffer buffer
          (claude-code-ide--terminal-send-escape))
      (user-error "No Claude Code session for this project"))))

;;;###autoload
(defun claude-code-ide-insert-newline ()
  "Send newline (backslash + return) to a Claude Code terminal.
This simulates typing backslash followed by Enter, which Claude Code
interprets as a newline.  Inside a Claude terminal buffer it always
targets that instance."
  (interactive)
  (let ((session (claude-code-ide--resolve-session 'auto "Send newline to Claude instance: ")))
    (if-let ((buffer (and session (claude-code-ide-mcp-session-buffer session))))
        (with-current-buffer buffer
          (claude-code-ide--terminal-send-string "\\")
          ;; Small delay to ensure prompt text is processed before sending return
          (sit-for 0.1)
          (claude-code-ide--terminal-send-return))
      (user-error "No Claude Code session for this project"))))

;;;###autoload
(defun claude-code-ide-toggle-vterm-optimization ()
  "Toggle vterm rendering optimization.
This command switches the advanced rendering optimization on or off.
Use this to balance between visual smoothness and raw responsiveness."
  (interactive)
  (setq claude-code-ide-vterm-anti-flicker
        (not claude-code-ide-vterm-anti-flicker))
  (message "Vterm rendering optimization %s"
           (if claude-code-ide-vterm-anti-flicker
               "enabled (smoother display with minimal latency)"
             "disabled (direct rendering, maximum responsiveness)")))

;;;###autoload
(defun claude-code-ide-send-prompt (&optional prompt session)
  "Send a prompt to a Claude Code terminal.
When called interactively, reads a prompt from the minibuffer.
When called programmatically, sends the given PROMPT string; SESSION
overrides the automatic instance resolution."
  (interactive)
  (let ((session (or session
                     (claude-code-ide--resolve-session 'auto "Send prompt to Claude instance: "))))
    (if-let ((buffer (and session (claude-code-ide-mcp-session-buffer session))))
        (let ((prompt-to-send (or prompt (read-string "Claude prompt: "))))
          (when (not (string-empty-p prompt-to-send))
            (with-current-buffer buffer
              (claude-code-ide--terminal-send-string prompt-to-send)
              ;; Small delay to ensure prompt text is processed before sending return
              (sit-for 0.1)
              (claude-code-ide--terminal-send-return))
            (claude-code-ide-debug "Sent prompt to Claude Code: %s" prompt-to-send)))
      (user-error "No Claude Code session for this project"))))

(defun claude-code-ide--current-tab-key ()
  "Return a key identifying the current tab-bar tab, or \"none\".
Tab-bar tabs keep independent window layouts, so everything that
remembers window sets must be scoped per tab."
  (or (and (fboundp 'tab-bar--current-tab)
           (alist-get 'name (tab-bar--current-tab)))
      "none"))

(defun claude-code-ide--hidden-panel-get (scope)
  "Return the remembered hidden window set for SCOPE in this frame's tab.
SCOPE is a project directory for the project panel, or `:all' for the
whole-tab panel of `claude-code-ide-toggle-recent'."
  (cdr (assoc (cons (claude-code-ide--current-tab-key) scope)
              (frame-parameter nil 'claude-code-ide-hidden-panel))))

(defun claude-code-ide--hidden-panel-set (scope sessions)
  "Remember SESSIONS as SCOPE's hidden window set in this frame's tab.
A nil SESSIONS drops the entry, so consumed or stale sets don't keep
dead session structs alive in the frame parameter.  Entries belonging
to tabs that no longer exist are pruned on the way."
  (let* ((key (cons (claude-code-ide--current-tab-key) scope))
         (live-tabs (and (fboundp 'tab-bar-tabs)
                         (mapcar (lambda (tab) (alist-get 'name (cdr tab)))
                                 (tab-bar-tabs))))
         (rest (cl-remove-if (lambda (entry)
                               (or (equal (car entry) key)
                                   (and live-tabs
                                        (not (member (caar entry) live-tabs)))))
                             (frame-parameter nil 'claude-code-ide-hidden-panel))))
    (set-frame-parameter nil 'claude-code-ide-hidden-panel
                         (if sessions
                             (cons (cons key sessions) rest)
                           rest))))

;;;###autoload
(defun claude-code-ide-toggle (&optional pick)
  "Toggle the current project's Claude Code windows as a panel.
When any of the project's instances is visible, hide them all and
remember the set; otherwise restore the remembered set, falling back
to the most recently used instance.  With prefix argument PICK,
choose a single instance and toggle only its window.
Tab-bar tabs keep independent window layouts, so hiding and restoring
apply to the current tab only."
  (interactive "P")
  (let* ((project-dir (claude-code-ide--get-working-directory))
         (sessions (claude-code-ide-mcp--sessions-for-project project-dir)))
    (unless sessions
      (user-error "No Claude Code session for this project"))
    (if pick
        (when-let ((session (claude-code-ide--read-project-session
                             "Toggle Claude instance: " project-dir)))
          (claude-code-ide--toggle-existing-window session))
      (let ((visible (claude-code-ide--visible-project-sessions project-dir)))
        (if visible
            (progn
              (claude-code-ide--hidden-panel-set project-dir visible)
              (dolist (session visible)
                (claude-code-ide--toggle-existing-window session))
              ;; Stay silent for a single window, matching the
              ;; single-instance behavior before panels existed
              (when (cdr visible)
                (message "Hid %d Claude Code windows" (length visible))))
          ;; Restore the remembered set (skipping stopped instances),
          ;; else show the most recently used instance
          (let ((restore (or (cl-remove-if-not (lambda (s) (memq s sessions))
                                               (claude-code-ide--hidden-panel-get project-dir))
                             (list (claude-code-ide-mcp--mru-session project-dir)))))
            (claude-code-ide--hidden-panel-set project-dir nil)
            (dolist (session restore)
              (claude-code-ide--toggle-existing-window session))))))))

;;;###autoload
(defun claude-code-ide-show-all (&optional all-projects)
  "Show every Claude Code instance of the current project.
Each hidden instance's window opens in its own side-window slot;
already visible ones are left in place.  Unlike `claude-code-ide-toggle',
this ignores the remembered panel set, so it also recovers instances
whose windows were deleted manually.  With prefix argument
ALL-PROJECTS, show the instances of all projects."
  (interactive "P")
  (let ((sessions (if all-projects
                      (claude-code-ide-mcp--active-sessions)
                    (claude-code-ide-mcp--sessions-for-project
                     (claude-code-ide--get-working-directory))))
        (shown 0))
    (unless sessions
      (user-error "No Claude Code session%s"
                  (if all-projects "s" " for this project")))
    (dolist (session sessions)
      (let ((buffer (claude-code-ide-mcp-session-buffer session)))
        (when (and buffer
                   (buffer-live-p buffer)
                   (not (claude-code-ide--session-visible-p session)))
          (claude-code-ide--display-buffer-in-side-window buffer)
          (cl-incf shown))))
    (message (if (zerop shown)
                 "All Claude Code instances already visible"
               (format "Showing %d Claude Code instance%s"
                       shown (if (> shown 1) "s" ""))))))

(define-obsolete-function-alias 'claude-code-ide-toggle-window
  #'claude-code-ide-toggle "0.2.0")

;;;###autoload
(defun claude-code-ide-toggle-recent ()
  "Toggle visibility of all Claude Code windows in the current tab.
If any Claude window is visible, hide all of them and remember the
set.  If none is visible, restore the remembered set, falling back to
the most recently accessed instance.  Tab-bar tabs keep independent
window layouts, so each tab hides and restores its own set."
  (interactive)
  (let ((visible (cl-remove-if-not #'claude-code-ide--session-visible-p
                                   (claude-code-ide-mcp--active-sessions))))
    (cond
     ;; Close all visible windows, remembering the set
     (visible
      (claude-code-ide--hidden-panel-set :all visible)
      (dolist (session visible)
        (claude-code-ide--toggle-existing-window session))
      (message "Closed all Claude Code windows%s"
               (if (and (fboundp 'tab-bar-tabs)
                        (cdr (tab-bar-tabs)))
                   " in this tab" "")))

     ;; Restore the remembered set (skipping stopped instances)
     ((cl-remove-if #'claude-code-ide-mcp-session-cleanup-done
                    (claude-code-ide--hidden-panel-get :all))
      (let ((restore (cl-remove-if #'claude-code-ide-mcp-session-cleanup-done
                                   (claude-code-ide--hidden-panel-get :all))))
        (claude-code-ide--hidden-panel-set :all nil)
        (dolist (session restore)
          (claude-code-ide--display-buffer-in-side-window
           (claude-code-ide-mcp-session-buffer session)))
        (message "Restored %d Claude Code window%s"
                 (length restore) (if (cdr restore) "s" ""))))

     ;; No remembered set, show the most recent one
     ((and claude-code-ide--last-accessed-buffer
           (buffer-live-p claude-code-ide--last-accessed-buffer))
      (claude-code-ide--display-buffer-in-side-window claude-code-ide--last-accessed-buffer)
      (message "Opened most recent Claude Code session"))

     ;; No recent session available
     (t
      (user-error "No recent Claude Code session to toggle")))))

(provide 'claude-code-ide)

;;; claude-code-ide.el ends here
