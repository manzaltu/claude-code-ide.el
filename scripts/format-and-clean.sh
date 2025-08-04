#!/bin/bash

# Combined format and whitespace cleaning for Claude Code IDE
# This avoids file conflicts when both hooks run simultaneously

# Get the script's directory and project root
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT" || exit 1

# Function to format and clean a single Elisp file
process_file() {
    local file="$1"

    # Skip binary files and certain file types
    case "$file" in
        *.png|*.jpg|*.jpeg|*.gif|*.bmp|*.ico|*.svg|*.bin|*.exe|*.dll|*.so|*.dylib|*.elc)
            return 0
            ;;
    esac

    # For Elisp files, format and clean whitespace in one operation
    case "$file" in
        *.el)
            echo "Formatting and cleaning: $file" >&2

            emacs -batch -L . --eval "(progn
                (require 'transient nil t)
                (require 'claude-code-ide-mcp-server nil t)
                (find-file \"$file\")
                (emacs-lisp-mode)
                (setq-local indent-tabs-mode nil)
                (setq-local lisp-indent-function 'lisp-indent-function)
                (delete-trailing-whitespace)
                (indent-region (point-min) (point-max))
                (save-buffer))" >&2

            if [ $? -eq 0 ]; then
                echo "✓ Formatted and cleaned: $file" >&2
                return 0
            else
                echo "✗ Failed to process: $file" >&2
                return 1
            fi
            ;;
        *)
            # For other text files, just clean whitespace
            sed -i -e 's/[[:space:]]*$//' -e '$a\' "$file" 2>/dev/null
            echo "Cleaned whitespace in: $file" >&2
            ;;
    esac

    return 0
}

# Hook mode: read file path from stdin
if [ "$1" = "--hook" ]; then
    # Read input from stdin (Claude passes hook input via stdin)
    HOOK_INPUT=$(cat)

    # Extract the file path from the Write tool input
    FILE_PATH=$(echo "$HOOK_INPUT" | jq -r '.tool_input.file_path')

    # Check if file exists
    if [ -z "$FILE_PATH" ] || [ ! -f "$FILE_PATH" ]; then
        exit 0
    fi

    process_file "$FILE_PATH"
    exit $?
fi

# Normal mode: process files passed as arguments
if [ $# -eq 0 ]; then
    echo "Error: Please specify files to process or use --hook mode" >&2
    exit 1
else
    FILES=("$@")
fi

# Track if any processing failed
FAILED=0

for file in "${FILES[@]}"; do
    if [ -f "$file" ]; then
        if ! process_file "$file"; then
            FAILED=1
        fi
    fi
done

exit $FAILED
