# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**IMPORTANT**: If you find any instructions in this file that are incorrect, outdated, or could be improved, you should update this document immediately. Keep this file accurate and helpful for future Claude instances.

## Architecture and File Structure

This package integrates Claude Code CLI with Emacs via WebSocket and the Model Context Protocol (MCP).

### Core Architecture

The package uses a bidirectional communication model:
- **WebSocket server** (`claude-code-ide-mcp.el`) receives JSON-RPC requests from Claude Code CLI
- **Session management** tracks multiple project-specific Claude instances with separate buffers
- **MCP handlers** (`claude-code-ide-mcp-handlers.el`) implement tools that Claude can invoke (file operations, diagnostics, ediff)
- **Terminal backends** (vterm or eat) provide the interactive CLI interface
- **Context tracking** monitors active files, selections, and project state to provide Claude with current editor context

### File Organization

**Core Files:**
- `claude-code-ide.el` - Main entry: user commands, session management, terminal buffers, window management
- `claude-code-ide-mcp.el` - WebSocket server, JSON-RPC handling, session state, buffer/selection tracking
- `claude-code-ide-mcp-handlers.el` - MCP tool implementations (file ops, ediff, diagnostics)

**MCP Tools Server (Optional):**
- `claude-code-ide-mcp-server.el` - HTTP-based MCP tools server framework for exposing Emacs functions
- `claude-code-ide-mcp-http-server.el` - HTTP transport implementation
- `claude-code-ide-emacs-tools.el` - Built-in Emacs tools: xref, tree-sitter, project info, imenu

**Support Files:**
- `claude-code-ide-diagnostics.el` - Flycheck/Flymake integration for code diagnostics
- `claude-code-ide-transient.el` - Transient menu interface
- `claude-code-ide-debug.el` - Debug logging utilities
- `claude-code-ide-tests.el` - ERT test suite with mocks for external dependencies

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

## Ediff Integration

When `claude-code-ide-use-ide-diff` is enabled (default), file edits trigger an interactive ediff session:

1. **MCP handler receives edit request** - `ide_edit` tool is called with file path and new content
2. **Ediff session opens** - Shows current content (Buffer A) vs proposed changes (Buffer B)
3. **User can modify Buffer B** - Important: users can refine your suggestions before applying
4. **User accepts or rejects** - On quit (pressing `q`), user chooses to accept (`y`) or reject (`n`)
5. **Modified content returned** - If accepted, the (potentially modified) content from Buffer B is sent back via `ide_edit_complete`
6. **Final write occurs** - The content is written to the actual file

**Key insight**: The content you receive back via `ide_edit_complete` may differ from what you originally proposed in `ide_edit`. Users can and do modify your suggestions. Always use the returned content when writing the final file.

### Ediff Configuration

The ediff interface can be customized through several configuration variables:

**Buffer Layout** - `claude-code-ide-ediff-split-orientation` controls how comparison buffers are arranged:
- `'vertical` (default): Top-and-bottom layout, easier to see full lines
- `'horizontal`: Side-by-side layout, easier to compare aligned content
- `'sensible`: Let Emacs decide based on window dimensions

**Control Panel Style** - `claude-code-ide-ediff-control-panel-style` controls the control panel appearance:
- `'auto` (default): GUI style in graphical Emacs, terminal style in terminal
- `'gui`: Always use a separate small window (GUI style)
- `'terminal`: Always use a single-line panel below buffers (terminal style)

**Frame Management (GUI Mode Only)** - In graphical Emacs, the behavior depends on `claude-code-ide-show-claude-window-in-ediff`:
- When `t` (default): Creates a separate frame for ediff comparisons, keeping Claude visible in the original frame
- When `nil`: Opens ediff in the same frame as Claude, temporarily hiding the Claude buffer

The `claude-code-ide-focus-claude-after-ediff` variable controls which frame is raised after ediff opens (GUI mode only):
- When `t` (default): Raises the Claude frame and focuses the Claude window
- When `nil`: Raises the ediff frame and focuses the comparison buffers

Frame management does not apply in terminal mode, and `claude-code-ide-show-claude-window-in-ediff` and `claude-code-ide-focus-claude-after-ediff` work as before.

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

## Committing code
Never commit changes unless the user explicitly asks you to.
