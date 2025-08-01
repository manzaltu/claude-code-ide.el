#!/bin/bash

# Test check for Stop hook
# Runs after Claude finishes responding

cd /home/yoav/projects/claude-code-ide.el

echo "Running test check..." >&2
emacs -batch -L . -l ert -l claude-code-ide-tests.el -f ert-run-tests-batch-and-exit >&2
TEST_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -eq 0 ]; then
    echo "✓ All tests passed!" >&2
    exit 0
else
    # Output JSON to block Claude from stopping
    cat <<EOF
{
  "decision": "block",
  "reason": "✗ Tests failed! Fix the failing tests above before stopping. Run 'emacs -batch -L . -l ert -l claude-code-ide-tests.el -f ert-run-tests-batch-and-exit' to see the failures again."
}
EOF
    exit 0
fi