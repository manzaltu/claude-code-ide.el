#!/bin/bash

# Lint check for Stop hook
# Runs after Claude finishes responding

cd "$(dirname "$0")/.." || exit 1

echo "Running lint check..." >&2

# Delegate byte-compilation check to the dedicated script
# This ensures consistency between GitHub Actions and Claude hooks
"$(dirname "$0")/compile-check.sh" --json

# The compile-check.sh script handles all output and exit codes
# If it blocks, we don't need to do anything else
# If it passes, we could add additional lint checks here in the future

exit 0
