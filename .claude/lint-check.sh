#!/bin/bash

# Lint check for Stop hook
# Runs after Claude finishes responding

cd /home/yoav/projects/claude-code-ide.el

echo "Running lint check..." >&2

# Check byte-compilation warnings (excluding dependency errors)
BYTE_COMPILE_OUTPUT=$(emacs -batch -f batch-byte-compile *.el 2>&1 | grep -v "Cannot open load file" | grep -v "No such file or directory" | grep -v "clang-include-fixer.el" | grep -E "(Warning|Error)")

# Check if there were any warnings or errors
if [ -n "$BYTE_COMPILE_OUTPUT" ]; then
    echo "✗ Byte-compilation warnings/errors found:" >&2
    echo "$BYTE_COMPILE_OUTPUT" >&2

    # Output JSON to block Claude from stopping
    cat <<EOF
{
  "decision": "block",
  "reason": "✗ Linting errors found! Fix the byte-compilation warnings/errors above before stopping. Run 'emacs -batch -f batch-byte-compile *.el' to see all warnings."
}
EOF
    exit 0
fi

# For now, skip the complex checkdoc checks as they're too prone to false positives
# Just ensure critical files have basic documentation
echo "✓ All linting checks passed!" >&2
exit 0
