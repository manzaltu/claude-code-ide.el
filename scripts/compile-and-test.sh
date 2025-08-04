#!/bin/bash

# Combined compile and test check for Claude Code IDE
# This script is used by both GitHub Actions and Claude stop hooks


# Check if we're in hook mode via --hook argument
HOOK_MODE=false
HOOK_INPUT=""
if [ "$1" = "--hook" ]; then
    HOOK_MODE=true

    # Read JSON input from stdin when in hook mode
    if [ ! -t 0 ]; then
        HOOK_INPUT=$(cat)
    fi
fi

# Get the script's directory and project root
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT" || exit 1
echo "Running compile and test checks..." >&2

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

# STEP 1: Compile all elisp files
echo "=== Running byte-compilation check ===" >&2
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

# Store compile result
COMPILE_FAILED=0
if [ -n "$REAL_ERRORS" ]; then
    COMPILE_FAILED=1
fi

# Show compile results
if [ -n "$REAL_ERRORS" ]; then
    echo "✗ Compilation errors found:" >&2
    echo "$REAL_ERRORS" >&2
elif [ -n "$FILTERED_OUTPUT" ]; then
    echo "⚠ Compilation warnings found:" >&2
    echo "$FILTERED_OUTPUT" | sed 's/^/[Warning] /' >&2
    echo "✓ Byte-compilation check passed (with warnings)" >&2
else
    echo "✓ Byte-compilation check passed!" >&2
fi

# STEP 2: Run tests (only if compilation succeeded or had warnings)
TEST_FAILED=0
if [ $COMPILE_FAILED -eq 0 ]; then
    echo "" >&2
    echo "=== Running tests ===" >&2
    emacs -batch -L . -l ert -l claude-code-ide-tests.el -f ert-run-tests-batch-and-exit >&2
    TEST_EXIT_CODE=$?

    if [ $TEST_EXIT_CODE -eq 0 ]; then
        echo "✓ All tests passed!" >&2
    else
        echo "✗ Tests failed!" >&2
        TEST_FAILED=1
    fi
else
    echo "" >&2
    echo "=== Skipping tests due to compilation errors ===" >&2
    TEST_FAILED=1
fi

# STEP 3: Clean up .elc files to avoid stale bytecode
echo "" >&2
echo "Cleaning up .elc files..." >&2
rm -f *.elc

# Handle hook mode output
if [ "$HOOK_MODE" = true ]; then
    if [ $COMPILE_FAILED -eq 1 ]; then
        # Escape newlines in error messages for JSON
        ESCAPED_ERRORS=$(echo "$REAL_ERRORS" | sed ':a;N;$!ba;s/\n/\\n/g' | sed 's/"/\\"/g')
        cat <<EOF
{
  "decision": "block",
  "reason": "✗ Compilation errors found! Fix these errors before stopping:\\n$ESCAPED_ERRORS"
}
EOF
        exit 0
    elif [ $TEST_FAILED -eq 1 ]; then
        cat <<EOF
{
  "decision": "block",
  "reason": "✗ Tests failed! Fix the failing tests above before stopping. Run './scripts/compile-and-test.sh' to see the failures again."
}
EOF
        exit 0
    else
        # Success - show summary
        echo "" >&2
        echo "=== Summary ===" >&2
        echo "✓ Compilation: PASSED" >&2
        echo "✓ Tests: PASSED" >&2
        echo "✓ All checks passed!" >&2
        exit 0
    fi
fi

# Normal mode: exit with appropriate code
if [ $COMPILE_FAILED -eq 1 ] || [ $TEST_FAILED -eq 1 ]; then
    exit 1
fi

exit 0
