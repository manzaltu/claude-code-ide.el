# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**IMPORTANT**: If you find any instructions in this file that are incorrect, outdated, or could be improved, you should update this document immediately. Keep this file accurate and helpful for future Claude instances.

## Commands

### Running Tests
```bash
# Run all tests in batch mode
emacs -batch -L . -l ert -l claude-code-ide-tests.el -f ert-run-tests-batch-and-exit

# Run core tests only
emacs -batch -L . -l ert -l claude-code-ide-tests.el -f claude-code-ide-run-tests

# Run all tests including MCP tests
emacs -batch -L . -l ert -l claude-code-ide-tests.el -f claude-code-ide-run-all-tests
```

### Development Tools
```bash
# Record WebSocket messages between VS Code and Claude Code for debugging
./record-claude-messages.sh [working_directory]
```

### Debugging
To enable debug logging, set `claude-code-ide-debug` to `t` in your Emacs configuration:
```elisp
(setq claude-code-ide-debug t)
```
Then view debug logs with:
```elisp
M-x claude-code-ide-show-debug
```

### Code Formatting (MANDATORY AFTER EVERY CHANGE)

**CRITICAL**: After EVERY code change, you MUST format the code before running tests. The formatter is NEVER wrong - if code looks wrongly formatted after running it, it's ALWAYS because of a mistake in the code.

```bash
# Format all Elisp files - RUN THIS AFTER EVERY CHANGE
emacs -batch -L . --eval "(progn (require 'transient nil t) (setq indent-tabs-mode nil) (dolist (file (directory-files \".\" t \"\\\\.el$\")) (find-file file) (setq indent-tabs-mode nil) (indent-region (point-min) (point-max)) (save-buffer) (kill-buffer)))"

# Alternative: Format a specific file
emacs -batch -L . --eval "(progn (require 'transient nil t) (find-file \"claude-code-ide.el\") (setq indent-tabs-mode nil) (indent-region (point-min) (point-max)) (save-buffer))"

# Remove trailing whitespaces from all Elisp files
emacs -batch --eval "(progn (dolist (file (directory-files \".\" t \"\\\\.el$\")) (find-file file) (delete-trailing-whitespace) (save-buffer) (kill-buffer)))"
```

**Debugging Syntax Errors**: If files fail to load due to syntax errors (missing parentheses, quotes, etc.), the formatter's indentation will reveal the problem:
1. Run the formatter on the broken file
2. Look for incorrectly indented lines - they indicate where parentheses/quotes are unbalanced
3. The formatter's indentation is ALWAYS correct, so trust it to find your syntax errors

**Trailing Whitespaces**: All code must be free of trailing whitespaces (spaces or tabs at the end of lines). Use the command above or configure your editor to automatically remove them on save.


### Self reference in code or commits

- **Important - Never self-reference in code or commits**: Do not mention Claude or include any self-referential messages in code or commit messages. Keep all content strictly professional and focused on the technical aspects.

### Linting and Code Quality (RUN AFTER FORMATTING)

**IMPORTANT**: After formatting your code, you MUST check for linting errors. Ignore "Cannot open load file" errors for dependencies.


```bash
# Run linting checks - RUN THIS AFTER FORMATTING
# Check byte-compilation warnings (excluding dependency errors)
emacs -batch -f batch-byte-compile *.el 2>&1 | grep -v "Cannot open load file" | grep -v "No such file or directory" | grep -E "(Warning|Error)"

# Check documentation strings
emacs -batch --eval "(progn (dolist (file (directory-files \".\" t \"\\\\.el$\")) (find-file file) (condition-case err (checkdoc-current-buffer t) (error (message \"Checkdoc error in %s: %s\" file err))) (kill-buffer)))"

# Check for specific warnings (useful for CI/CD)
emacs -batch -f batch-byte-compile *.el 2>&1 | grep -v "Cannot open load file" | grep -v "No such file or directory" | grep -v "clang-include-fixer.el" | grep -E "(Warning|Error)" && echo "Linting errors found!" && exit 1 || echo "No linting errors found"
```

**Common Linting Errors to Fix**:
- **Undefined functions/variables**: Add proper `require` statements or `declare-function`
- **Missing or incorrect docstrings**: Follow Emacs docstring conventions (first line < 80 chars, end with period)
- **Free variables**: Properly declare or bind all variables
- **Lexical binding**: Files should start with `;;; -*- lexical-binding: t -*-`
- **Obsolete functions**: Replace deprecated functions with modern alternatives

### Package Development
```bash
# Byte-compile all Elisp files (check for warnings)
emacs -batch -f batch-byte-compile *.el

# Check package lint issues
emacs -batch -l package-lint -f package-lint-batch-and-exit claude-code-ide.el
```

### Development Workflow
1. Make code changes
2. **ALWAYS format the code** (see Code Formatting section above)
3. **Remove all trailing whitespaces** - No line should end with spaces or tabs
4. **Run linting checks** (see Linting and Code Quality section above)
5. **Fix all linting errors** before proceeding
6. **Write tests for any new logic** - Every new function or significant change should have corresponding tests
7. Run tests to verify changes
8. If tests fail due to syntax errors, use the formatter to identify issues

## Architecture

This Emacs package implements Claude Code integration through the Model Context Protocol (MCP). It creates a WebSocket server that Claude CLI connects to, enabling bidirectional communication for file operations and IDE features.

### Core Components

1. **claude-code-ide.el** - Main module managing Claude Code sessions
   - Terminal integration via vterm
   - Project-based session management using project.el
   - Window management with side-window support

2. **claude-code-ide-mcp.el** - MCP server implementation
   - WebSocket server on random port (10000-65535)
   - Lockfile management in ~/.claude/ide/ for CLI discovery
   - JSON-RPC message handling

3. **claude-code-ide-mcp-handlers.el** - MCP protocol handlers
   - File operations: openFile, saveDocument, close_tab
   - Editor state: getCurrentSelection, getOpenEditors
   - Workspace info: getWorkspaceFolders
   - Diff operations: openDiff with ediff integration
   - Tab-bar support: Automatically switches to the correct tab before opening ediff
   - Ediff enhancements: Automatically jumps to the first difference when opening a diff and moves focus back to the Claude window

4. **claude-code-ide-diagnostics.el** - Diagnostic integration
   - Supports Flycheck
   - Converts diagnostics to VS Code format

### Key Implementation Details

- The package mimics VS Code's MCP server behavior to be compatible with Claude CLI
- Environment variables set for Claude CLI:
  - `CLAUDE_CODE_SSE_PORT`: WebSocket server port
  - `ENABLE_IDE_INTEGRATION=true`: Enables MCP tools
- Lockfile format matches VS Code extensions for CLI discovery
- All MCP responses use VS Code's expected JSON structure
- Tab-bar integration: The package tracks which tab-bar tab Claude was opened in and automatically switches back to it when opening ediff, ensuring diffs always appear in the correct context
- Direct process execution: Claude is run directly by vterm (not through a shell) to avoid shell history pollution by setting `vterm-shell` to the Claude command

### Testing Strategy

Tests use mocks for external dependencies (vterm, websocket) to run in batch mode without requiring actual installations. The test suite covers:
- Core functionality (session management, CLI detection)
- MCP handlers (file operations, diagnostics)
- Edge cases (side windows, multiple sessions)
