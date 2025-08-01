#!/bin/bash

# Read input from stdin (Claude passes hook input via stdin)
HOOK_INPUT=$(cat)

# Extract the file path from the Write tool input
FILE_PATH=$(echo "$HOOK_INPUT" | jq -r '.tool_input.file_path')

# Check if file exists
if [ -z "$FILE_PATH" ] || [ ! -f "$FILE_PATH" ]; then
    exit 0
fi

# Check if it's an Elisp file
case "$FILE_PATH" in
    *.el)
        # Change to project directory
        cd /home/yoav/projects/claude-code-ide.el

        # Format the individual Elisp file
        echo "Formatting: $FILE_PATH" >&2
        emacs -batch -L . --eval "(progn (require 'transient nil t) (require 'claude-code-ide-mcp-server nil t) (find-file \"$FILE_PATH\") (emacs-lisp-mode) (setq-local indent-tabs-mode nil) (setq-local lisp-indent-function 'lisp-indent-function) (indent-region (point-min) (point-max)) (save-buffer))" >&2

        # Check exit code
        if [ $? -eq 0 ]; then
            echo "✓ Formatted: $FILE_PATH" >&2
        else
            echo "✗ Failed to format: $FILE_PATH" >&2
            exit 1
        fi
        ;;
    *)
        # Not an Elisp file, skip formatting
        exit 0
        ;;
esac

exit 0