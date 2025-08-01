#!/bin/bash

# Read input from stdin (Claude passes hook input via stdin)
HOOK_INPUT=$(cat)

# Extract the file path from the Write tool input
FILE_PATH=$(echo "$HOOK_INPUT" | jq -r '.tool_input.file_path')

# Check if file exists and is a text file we should process
if [ -z "$FILE_PATH" ] || [ ! -f "$FILE_PATH" ]; then
    exit 0
fi

# Skip binary files and certain file types
case "$FILE_PATH" in
    *.png|*.jpg|*.jpeg|*.gif|*.bmp|*.ico|*.svg|*.bin|*.exe|*.dll|*.so|*.dylib|*.elc)
        exit 0
        ;;
esac

# For Elisp files, use Emacs to clean whitespace
case "$FILE_PATH" in
    *.el)
        # Change to project directory
        cd /home/yoav/projects/claude-code-ide.el

        # Use Emacs to clean whitespace
        emacs -batch --eval "(progn (find-file \"$FILE_PATH\") (delete-trailing-whitespace) (save-buffer) (kill-buffer))" 2>/dev/null

        echo "Cleaned whitespace in: $FILE_PATH" >&2
        ;;
    *)
        # For other text files, use sed
        # Clean whitespace in-place:
        # 1. Remove trailing whitespace from each line
        # 2. Ensure file ends with exactly one newline
        sed -i -e 's/[[:space:]]*$//' -e '$a\' "$FILE_PATH" 2>/dev/null

        echo "Cleaned whitespace in: $FILE_PATH" >&2
        ;;
esac

exit 0
