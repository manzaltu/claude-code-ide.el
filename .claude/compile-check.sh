#!/bin/bash

# Byte-compilation check for Claude Code IDE
# This script is used by both GitHub Actions and Claude stop hooks

cd "$(dirname "$0")/.." || exit 1

echo "Running byte-compilation check..." >&2

# Function to check if Emacs package directory exists
find_emacs_package() {
    local package="$1"
    # Check common locations
    for dir in \
        ~/.emacs.d/.cache/straight/repos/"$package" \
        ~/.emacs.d/elpa/"$package"* \
        ~/.config/emacs/.cache/straight/repos/"$package" \
        ~/.config/emacs/elpa/"$package"*; do
        if [ -d "$dir" ]; then
            echo "$dir"
            return 0
        fi
    done
    return 1
}

# Build load path for dependencies
LOAD_PATH="-L ."

# Try to find optional dependencies
if WEBSOCKET_DIR=$(find_emacs_package "emacs-websocket"); then
    LOAD_PATH="$LOAD_PATH -L $WEBSOCKET_DIR"
fi

if TRANSIENT_DIR=$(find_emacs_package "transient"); then
    LOAD_PATH="$LOAD_PATH -L $TRANSIENT_DIR"
fi

if VTERM_DIR=$(find_emacs_package "emacs-libvterm"); then
    LOAD_PATH="$LOAD_PATH -L $VTERM_DIR"
fi

# Compile all elisp files and capture output
COMPILE_OUTPUT=$(emacs -batch $LOAD_PATH \
    --eval "(setq byte-compile-warnings '(not free-vars unresolved))" \
    -f batch-byte-compile *.el 2>&1)

# Filter out known non-issues
FILTERED_OUTPUT=$(echo "$COMPILE_OUTPUT" | grep -E "(Warning|Error)" | \
    grep -v "clang-include-fixer.el" | \
    grep -v "Unknown type: buffer-live" | \
    grep -v "reference to free variable.*eat-terminal" | \
    grep -v "reference to free variable.*vterm-" | \
    grep -v "reference to free variable.*flycheck-" | \
    grep -v "the function.*is not known to be defined" | \
    grep -v "Unused lexical" | \
    grep -v "docstring wider than")

# Check for real errors (not just warnings)
REAL_ERRORS=$(echo "$COMPILE_OUTPUT" | grep -E "Error:" | \
    grep -v "Cannot open load file" | \
    grep -v "No such file or directory")

# For Claude hook: output JSON format
if [ "$1" = "--json" ]; then
    if [ -n "$REAL_ERRORS" ]; then
        cat <<EOF
{
  "decision": "block",
  "reason": "✗ Compilation errors found! Fix these errors before stopping:\n$REAL_ERRORS"
}
EOF
        exit 0
    elif [ -n "$FILTERED_OUTPUT" ]; then
        # Only show warnings, don't block
        echo "$FILTERED_OUTPUT" | sed 's/^/[Warning] /' >&2
    fi
    echo "✓ Byte-compilation check passed!" >&2
    exit 0
fi

# For GitHub Actions: exit with error code
if [ -n "$REAL_ERRORS" ]; then
    echo "✗ Compilation errors found:" >&2
    echo "$REAL_ERRORS" >&2
    exit 1
fi

if [ -n "$FILTERED_OUTPUT" ]; then
    echo "⚠ Compilation warnings found:" >&2
    echo "$FILTERED_OUTPUT" >&2
    # Don't fail on warnings for now
fi

echo "✓ Byte-compilation check passed!" >&2

# Clean up .elc files to avoid stale bytecode
rm -f *.elc

exit 0
