# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**IMPORTANT**: If you find any instructions in this file that are incorrect, outdated, or could be improved, you should update this document immediately. Keep this file accurate and helpful for future Claude instances.

## Architecture and File Structure

This package integrates Claude Code CLI with Emacs via WebSocket and the Model Context Protocol (MCP).

**Core Files:**
- `claude-code-ide.el` - Main entry: user commands, session management, terminal buffers
- `claude-code-ide-mcp.el` - WebSocket server, JSON-RPC handling, session state
- `claude-code-ide-mcp-handlers.el` - MCP tool implementations (file ops, ediff, diagnostics)

**Support Files:**
- `claude-code-ide-mcp-server.el` - HTTP-based MCP tools server framework
- `claude-code-ide-mcp-http-server.el` - HTTP transport implementation
- `claude-code-ide-emacs-tools.el` - Emacs tools: xref, project info, imenu, buffer listing, tree-sitter, goto-location
- `claude-code-ide-diagnostics.el` - Flycheck integration
- `claude-code-ide-transient.el` - Transient menu interface
- `claude-code-ide-debug.el` - Debug logging utilities
- `claude-code-ide-tests.el` - ERT test suite with mocks

**Modular MCP Tools (mcp-tools.d/):**
- `claude-code-ide-tool-buffer-management.el` - Buffer operations (list, read, goto, reload)
- `claude-code-ide-tool-eval.el` - Emacs Lisp evaluation (disabled by default, see Security section)

## Hooks

This project uses Claude Code hooks to automatically maintain code quality. The hooks are configured in `.claude/settings.json` and include:
- **PostToolUse hooks**: Automatically format code and remove trailing whitespace after edits
- **Stop hooks**: Run tests and linting checks before allowing Claude to stop, blocking if issues are found

These hooks help ensure consistent code formatting and catch issues early in the development process.

## Commands

### Running Tests

Tests run automatically as part of Claude Code hooks, but you can also run them manually:
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

## Debugging

The user has the ability to enable debug logging and to send you the produced log. Ask them for assistance if needed.

### Debugging Syntax Errors

**Important**: The formatter's indentation is ALWAYS correct, so do not try to reformat code yourself. If the code is not indented correctly , it **ALWAYS** means that there is an issue with the code and **not** with the formatter.

If files fail to load due to syntax errors (missing parentheses, quotes, etc.), the formatter's indentation will reveal the problem. Look for incorrectly indented lines - they indicate where parentheses/quotes are unbalanced.

## Self reference in code or commits

- **Important - Never self-reference in code or commits**: Do not mention Claude or include any self-referential messages in code or commit messages. Keep all content strictly professional and focused on the technical aspects.

## Testing

**Always write tests for any new logic** - Every new function or significant change should have corresponding tests.

Tests use mocks for external dependencies (vterm, websocket) to run in batch mode without requiring actual installations. The test suite covers:
- Core functionality (session management, CLI detection)
- MCP handlers (file operations, diagnostics)
- Edge cases (side windows, multiple sessions)

## Security

### Emacs Lisp Eval Tool

The `claude-code-ide-tool-eval` MCP tool allows Claude to evaluate arbitrary Emacs Lisp expressions. This is **disabled by default** for security.

**To enable:**
```elisp
(setq claude-code-ide-eval-enabled t)
```

Or interactively: `M-x claude-code-ide-eval-toggle`

**Security features:**
- Disabled by default (must be explicitly enabled)
- All evaluations are logged to `*claude-code-ide-eval-log*` buffer
- View log: `M-x claude-code-ide-eval-show-log`
- Clear log: `M-x claude-code-ide-eval-clear-log`
- Can be toggled on/off at any time

**Additional protection:**
Consider requiring user approval for the eval tool in `~/.claude/settings.json`:
```json
{
  "permissions": {
    "ask": [
      "mcp__emacs-tools__claude-code-ide-mcp-eval"
    ]
  }
}
```

## Committing code
Never commit changes unless the user explicitly asks you to.
- You can use the read-buffer tool to read *Messages* to help diagnose emacs runtime issues
