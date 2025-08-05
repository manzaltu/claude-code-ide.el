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
emacs -batch $LOAD_PATH \
    --eval "(setq byte-compile-warnings '(not free-vars unresolved))" \
    -f batch-byte-compile *.el
COMPILE_EXIT_CODE=$?

if [ $COMPILE_EXIT_CODE -eq 0 ]; then
    echo "✓ Byte-compilation check passed!" >&2
else
    echo "✗ Compilation failed!" >&2
fi

# STEP 2: Run tests (only if compilation succeeded)
TEST_FAILED=0
if [ $COMPILE_EXIT_CODE -eq 0 ]; then
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
    if [ $COMPILE_EXIT_CODE -ne 0 ]; then
        cat <<EOF
{
  "decision": "block",
  "reason": "✗ Compilation failed! Fix the compilation errors before stopping. Run './scripts/compile-and-test.sh' to see the errors."
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
if [ $COMPILE_EXIT_CODE -ne 0 ] || [ $TEST_FAILED -eq 1 ]; then
    exit 1
fi

exit 0
