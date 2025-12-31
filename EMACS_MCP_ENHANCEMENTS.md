# Emacs MCP Server Enhancement Proposal

## Overview

Proposal to extend `claude-code-ide.el` MCP server with editor control capabilities. Currently the integration is read-only (code analysis). This adds write/control capabilities for a bidirectional integration.

## Motivation

**Current Limitation:** After Claude Code makes changes to files (via `Edit`, `Write`, or shell commands like `sed`), Emacs buffers become out-of-sync. The user must manually reload buffers.

**Desired Workflow:**
1. Claude Code refactors code using `Edit` or `sed`
2. Claude Code automatically calls `reload-buffer` to sync Emacs
3. Claude Code uses LSP diagnostics to find issues
4. Claude Code calls `goto-location` to jump to the error in Emacs
5. User sees the error highlighted in their editor immediately

## Proposed MCP Tools

### 1. `open-file` - Open File in Emacs

**Purpose:** Open a file in Emacs, optionally jumping to a specific location.

**Parameters:**
```json
{
  "file_path": "/absolute/path/to/file.py",
  "line": 42,           // optional, 1-based
  "column": 10,         // optional, 0-based (Emacs uses 0-based columns)
  "focus": true         // optional, switch to window/frame (default: true)
}
```

**Returns:**
```json
{
  "success": true,
  "buffer_name": "file.py",
  "window_id": "emacs-window-123"
}
```

**Emacs Implementation:**
```elisp
(defun claude-code-mcp-open-file (file-path &optional line column focus)
  "Open FILE-PATH in Emacs, optionally jumping to LINE and COLUMN."
  (let ((buffer (find-file-noselect (expand-file-name file-path))))
    (when focus
      (switch-to-buffer buffer))
    (when line
      (goto-line line)
      (when column
        (move-to-column column)))
    (list :success t
          :buffer-name (buffer-name buffer)
          :window-id (window-id (selected-window)))))
```

**Use Cases:**
- After finding references with xref, open the file to view them
- After grep search, jump to first match
- Navigate to files mentioned in error messages

---

### 2. `reload-buffer` - Reload Buffer from Disk

**Purpose:** Revert buffer contents from disk (sync after external modifications).

**Parameters:**
```json
{
  "file_path": "/absolute/path/to/file.py"  // optional, uses current buffer if not specified
}
```

**Returns:**
```json
{
  "success": true,
  "file_path": "/absolute/path/to/file.py",
  "message": "Buffer reloaded from disk"
}
```

**Emacs Implementation:**
```elisp
(defun claude-code-mcp-reload-buffer (&optional file-path)
  "Reload buffer from disk. Uses current buffer if FILE-PATH not specified."
  (let* ((buffer (if file-path
                     (find-buffer-visiting (expand-file-name file-path))
                   (current-buffer)))
         (file (buffer-file-name buffer)))
    (if (not buffer)
        (list :success nil :error "Buffer not found")
      (with-current-buffer buffer
        (revert-buffer t t t)
        (list :success t
              :file-path file
              :message "Buffer reloaded from disk")))))
```

**Use Cases:**
- After `Edit` or `Write` tool modifies a file
- After `sed`, `awk`, or other shell commands modify files
- After git operations (checkout, pull, merge)

---

### 3. `goto-location` - Jump to Specific Location

**Purpose:** Jump to a specific line/column in a file.

**Parameters:**
```json
{
  "file_path": "/absolute/path/to/file.py",
  "line": 42,           // required, 1-based
  "column": 10,         // optional, 0-based
  "highlight": true     // optional, temporarily highlight the line (default: false)
}
```

**Returns:**
```json
{
  "success": true,
  "file_path": "/absolute/path/to/file.py",
  "line": 42,
  "column": 10
}
```

**Emacs Implementation:**
```elisp
(defun claude-code-mcp-goto-location (file-path line &optional column highlight)
  "Jump to LINE and COLUMN in FILE-PATH. Optionally HIGHLIGHT the line."
  (find-file (expand-file-name file-path))
  (goto-line line)
  (when column
    (move-to-column column))
  (when highlight
    (pulse-momentary-highlight-one-line (point)))
  (recenter)
  (list :success t
        :file-path file-path
        :line line
        :column (or column 0)))
```

**Use Cases:**
- Jump to LSP diagnostic errors/warnings
- Jump to grep/ripgrep search results
- Navigate to stack trace locations
- Jump to specific code locations mentioned in logs

---

### 4. `show-definition` - Jump to Symbol Definition

**Purpose:** Jump to the definition of a symbol using xref/LSP.

**Parameters:**
```json
{
  "symbol": "ShippingExtractor",
  "file_path": "/absolute/path/to/file.py"  // optional, provides context
}
```

**Returns:**
```json
{
  "success": true,
  "symbol": "ShippingExtractor",
  "definition_file": "/path/to/main.py",
  "definition_line": 67,
  "definition_column": 6
}
```

**Emacs Implementation:**
```elisp
(defun claude-code-mcp-show-definition (symbol &optional file-path)
  "Jump to definition of SYMBOL, optionally starting from FILE-PATH context."
  (when file-path
    (find-file (expand-file-name file-path)))
  (let ((definitions (xref-backend-definitions 'etags symbol)))
    (if (not definitions)
        (list :success nil :error (format "No definition found for %s" symbol))
      (xref-pop-to-location (car definitions))
      (list :success t
            :symbol symbol
            :definition-file (buffer-file-name)
            :definition-line (line-number-at-pos)
            :definition-column (current-column)))))
```

**Use Cases:**
- Navigate to class/function definitions when analyzing code
- Jump to implementation after reading documentation
- Explore code structure

---

### 5. `run-elisp` - Execute Arbitrary Elisp (Advanced)

**Purpose:** Execute arbitrary Elisp code in Emacs. Most flexible but requires care.

**Parameters:**
```json
{
  "code": "(progn (save-some-buffers t) (recompile))",
  "capture_output": true  // optional, capture output (default: false)
}
```

**Returns:**
```json
{
  "success": true,
  "result": "Compilation started",
  "output": "... compilation output ..."  // if capture_output=true
}
```

**Emacs Implementation:**
```elisp
(defun claude-code-mcp-run-elisp (code &optional capture-output)
  "Execute Elisp CODE. Optionally CAPTURE-OUTPUT."
  (condition-case err
      (let ((result (if capture-output
                        (with-output-to-string
                          (eval (read code)))
                      (eval (read code)))))
        (list :success t
              :result (format "%S" result)
              :output (when capture-output result)))
    (error
     (list :success nil
           :error (error-message-string err)))))
```

**Use Cases:**
- Trigger compilation: `(recompile)`
- Run tests: `(projectile-test-project)`
- Format code: `(python-black-buffer)`
- Save all buffers: `(save-some-buffers t)`
- Custom workflows

**Security Note:** This is powerful but potentially dangerous. Consider:
- Restricting to allowlist of safe commands
- Requiring user confirmation for certain operations
- Sandboxing the evaluation context

---

## Integration with Existing Tools

### Enhanced Workflow Examples

#### Example 1: Refactor with Auto-Reload
```
1. Claude: Uses imenu to find ShippingExtractor class (line 67-2377)
2. Claude: Uses sed to replace logger. with self.logger. in that range
3. Claude: Calls reload-buffer("main.py") to sync Emacs
4. Claude: Uses getDiagnostics to check for errors
5. Claude: If errors found, calls goto-location to jump to first error
```

#### Example 2: Fix LSP Diagnostic
```
1. Claude: Calls getDiagnostics
2. Claude: Finds error at main.py:304
3. Claude: Calls goto-location(file="main.py", line=304, highlight=true)
4. User sees error highlighted in Emacs
5. Claude: Makes fix with Edit tool
6. Claude: Calls reload-buffer("main.py")
7. Claude: Calls getDiagnostics again to verify fix
```

#### Example 3: Navigate References
```
1. Claude: Uses xref-find-references to find all uses of logger
2. Claude: Calls goto-location for each reference
3. Claude: Shows user the context at each location
4. User decides which to refactor
```

---

## MCP Server Registration

Add to `claude-code-ide.el`:

```elisp
(defun claude-code-ide-mcp-register-tools ()
  "Register all MCP tools."
  (claude-code-ide-mcp-register-tool
   "open-file"
   "Open a file in Emacs, optionally jumping to a specific location"
   '((file_path . string)
     (line . number)
     (column . number)
     (focus . boolean))
   #'claude-code-mcp-open-file)

  (claude-code-ide-mcp-register-tool
   "reload-buffer"
   "Reload buffer from disk (sync after external modifications)"
   '((file_path . string))
   #'claude-code-mcp-reload-buffer)

  (claude-code-ide-mcp-register-tool
   "goto-location"
   "Jump to a specific line/column in a file"
   '((file_path . string)
     (line . number)
     (column . number)
     (highlight . boolean))
   #'claude-code-mcp-goto-location)

  (claude-code-ide-mcp-register-tool
   "show-definition"
   "Jump to the definition of a symbol using xref/LSP"
   '((symbol . string)
     (file_path . string))
   #'claude-code-mcp-show-definition)

  (claude-code-ide-mcp-register-tool
   "run-elisp"
   "Execute arbitrary Elisp code in Emacs"
   '((code . string)
     (capture_output . boolean))
   #'claude-code-mcp-run-elisp))
```

---

## Testing Plan

### Unit Tests
```elisp
(ert-deftest test-claude-code-mcp-open-file ()
  "Test opening a file."
  (let ((result (claude-code-mcp-open-file "/tmp/test.py" 10 5)))
    (should (plist-get result :success))
    (should (equal (plist-get result :buffer-name) "test.py"))))

(ert-deftest test-claude-code-mcp-reload-buffer ()
  "Test reloading a buffer."
  (with-temp-buffer
    (insert "original content")
    (write-file "/tmp/test-reload.txt")
    ;; Modify file externally
    (with-temp-file "/tmp/test-reload.txt"
      (insert "modified content"))
    ;; Reload
    (let ((result (claude-code-mcp-reload-buffer "/tmp/test-reload.txt")))
      (should (plist-get result :success))
      (should (string= (buffer-string) "modified content")))))
```

### Integration Tests
1. Test reload-buffer after Edit tool modifies file
2. Test goto-location with LSP diagnostics
3. Test open-file with xref references
4. Test run-elisp with common commands (compile, test, format)

---

## Security Considerations

### `run-elisp` Safety

**Risks:**
- Arbitrary code execution in Emacs process
- Could modify files, delete data, access network
- Could hang or crash Emacs

**Mitigations:**
1. **Allowlist Mode (Recommended for v1):**
   ```elisp
   (defvar claude-code-safe-commands
     '(recompile
       save-some-buffers
       projectile-test-project
       python-black-buffer
       format-all-buffer))

   (defun claude-code-mcp-run-elisp-safe (code)
     "Execute CODE only if it's in the allowlist."
     (let ((form (read code)))
       (if (member (car form) claude-code-safe-commands)
           (claude-code-mcp-run-elisp code)
         (list :success nil
               :error "Command not in safe allowlist"))))
   ```

2. **User Confirmation:**
   ```elisp
   (defun claude-code-mcp-run-elisp-with-confirmation (code)
     "Execute CODE after user confirmation."
     (when (y-or-n-p (format "Execute elisp: %s?" code))
       (claude-code-mcp-run-elisp code)))
   ```

3. **Dry-Run Mode:**
   - Add `dry_run` parameter
   - Returns what would be executed without executing

### File Access Safety

All tools should:
- Use `expand-file-name` to normalize paths
- Check file exists before operations
- Respect Emacs file permissions
- Stay within project boundaries (optional: check `projectile-project-root`)

---

## Implementation Status

### ✅ Implemented
1. **goto-location** - Jump to specific line/column in a file with optional highlighting
2. **reload-buffer** - Reload buffer from disk to sync with external file modifications

### Phase 1 (Essential - Implement Next)
1. **open-file** - Basic navigation (may be redundant with goto-location)

### Phase 2 (Nice to Have)
4. **show-definition** - Useful but overlaps with goto-location + xref

### Phase 3 (Advanced)
5. **run-elisp** - Powerful but requires careful security design

---

## Documentation Updates

### For AI Assistants (README.md)

Add to "Code navigation and refactoring best practices":

```markdown
- **Sync Emacs after edits:** After using Edit, Write, or shell commands to modify files,
  call `mcp__emacs-tools__reload-buffer` to sync the buffer in Emacs
- **Navigate to errors:** After getting LSP diagnostics, use `mcp__emacs-tools__goto-location`
  to jump to errors/warnings in the editor
- **Show code to user:** Use `mcp__emacs-tools__open-file` to open relevant files when
  explaining code or showing examples
```

### For Users

Add to docs/EMACS_INTEGRATION.md (new file):

```markdown
# Emacs Integration

claude-code-ide.el provides bidirectional integration between Claude Code and Emacs.

## Available Commands

### Navigation
- `open-file` - Open a file in Emacs
- `goto-location` - Jump to specific line/column
- `show-definition` - Jump to symbol definition

### Synchronization
- `reload-buffer` - Reload buffer from disk after external modifications

### Automation (Advanced)
- `run-elisp` - Execute Emacs commands (requires configuration)

## Configuration

### Enable Auto-Reload
Automatically reload buffers when Claude Code modifies files:

```elisp
(setq claude-code-auto-reload-buffers t)
```

### Configure Safe Commands (for run-elisp)
```elisp
(setq claude-code-safe-commands
      '(recompile
        save-some-buffers
        projectile-test-project))
```
```

---

## Future Enhancements

### Potential Additions
1. **Multiple cursor/region selection** - Highlight multiple locations
2. **Diff preview** - Show diffs before applying changes
3. **Search/replace UI** - Interactive search with preview
4. **Workspace management** - Save/restore window configurations
5. **Terminal integration** - Send commands to inferior shell/REPL

### LSP Integration
- `trigger-code-action` - Apply LSP code actions
- `format-buffer` - Trigger LSP formatting
- `organize-imports` - Trigger organize imports
- `get-diagnostics-filtered` - Enhanced diagnostics with filtering (see below)

---

## 6. `get-diagnostics-filtered` - Enhanced LSP Diagnostics with Filtering

**Purpose:** Get LSP diagnostics with filtering options to reduce context window usage and focus on relevant issues.

**Problem with Current `getDiagnostics`:**
- Returns all diagnostics (errors, warnings, info, hints) for entire file
- Large files can have 100+ diagnostics consuming significant context
- No way to paginate or filter results
- AI assistants waste tokens processing irrelevant warnings

**Parameters:**
```json
{
  "file_path": "/absolute/path/to/file.py",  // required
  "severity_filter": ["Error", "Warning"],    // optional, default: all severities
  "line_range": {                             // optional, only diagnostics in range
    "start": 100,
    "end": 200
  },
  "limit": 20,                                // optional, max diagnostics to return
  "offset": 0,                                // optional, for pagination
  "source_filter": ["pylint", "mypy3"]       // optional, filter by diagnostic source
}
```

**Returns:**
```json
{
  "diagnostics": [
    {
      "line": 42,
      "column": 10,
      "severity": "Error",
      "source": "mypy3",
      "message": "Incompatible types...",
      "range": {...}
    }
  ],
  "total_count": 156,        // Total diagnostics before filtering
  "filtered_count": 20,      // Count after filtering
  "has_more": true           // Whether there are more results
}
```

**Emacs Implementation:**
```elisp
(defun claude-code-mcp-get-diagnostics-filtered (file-path &optional severity-filter line-range limit offset source-filter)
  "Get LSP diagnostics with filtering options.
FILE-PATH: Absolute path to file
SEVERITY-FILTER: List of severity levels to include (Error, Warning, Information, Hint)
LINE-RANGE: Plist with :start and :end line numbers
LIMIT: Maximum number of diagnostics to return
OFFSET: Number of diagnostics to skip (for pagination)
SOURCE-FILTER: List of diagnostic sources to include (pylint, mypy3, etc.)"

  (let* ((buffer (find-buffer-visiting (expand-file-name file-path)))
         (all-diagnostics (if buffer
                              (with-current-buffer buffer
                                (lsp-diagnostics))
                            nil))
         (filtered (seq-filter
                    (lambda (diag)
                      (let ((severity (lsp:diagnostic-severity diag))
                            (line (lsp:position-line (lsp:range-start (lsp:diagnostic-range diag))))
                            (source (lsp:diagnostic-source diag)))
                        (and
                         ;; Severity filter
                         (or (not severity-filter)
                             (member (lsp-diagnostic-severity-to-string severity) severity-filter))
                         ;; Line range filter
                         (or (not line-range)
                             (and (>= line (plist-get line-range :start))
                                  (<= line (plist-get line-range :end))))
                         ;; Source filter
                         (or (not source-filter)
                             (member source source-filter)))))
                    all-diagnostics))
         (total-count (length all-diagnostics))
         (filtered-count (length filtered))
         (paginated (if (and limit offset)
                        (seq-subseq filtered offset (min (+ offset limit) filtered-count))
                      filtered))
         (has-more (and limit offset (< (+ offset (length paginated)) filtered-count))))

    (list :diagnostics (mapcar #'claude-code-mcp-diagnostic-to-json paginated)
          :total-count total-count
          :filtered-count filtered-count
          :has-more has-more)))

(defun claude-code-mcp-diagnostic-to-json (diag)
  "Convert LSP diagnostic to JSON-friendly format."
  (let* ((range (lsp:diagnostic-range diag))
         (start (lsp:range-start range))
         (line (lsp:position-line start))
         (column (lsp:position-character start)))
    (list :line line
          :column column
          :severity (lsp-diagnostic-severity-to-string (lsp:diagnostic-severity diag))
          :source (lsp:diagnostic-source diag)
          :message (lsp:diagnostic-message diag)
          :range range)))

(defun lsp-diagnostic-severity-to-string (severity)
  "Convert LSP severity number to string."
  (pcase severity
    (1 "Error")
    (2 "Warning")
    (3 "Information")
    (4 "Hint")
    (_ "Unknown")))
```

**Use Cases:**

1. **Errors only (ignore warnings):**
   ```json
   {
     "file_path": "/path/to/file.py",
     "severity_filter": ["Error"]
   }
   ```

2. **Diagnostics in specific function:**
   ```json
   {
     "file_path": "/path/to/file.py",
     "line_range": {"start": 100, "end": 150},
     "severity_filter": ["Error", "Warning"]
   }
   ```

3. **Paginated results:**
   ```json
   {
     "file_path": "/path/to/file.py",
     "limit": 10,
     "offset": 0
   }
   ```
   Then call again with `offset: 10` for next page.

4. **Only mypy errors (ignore pylint style warnings):**
   ```json
   {
     "file_path": "/path/to/file.py",
     "severity_filter": ["Error"],
     "source_filter": ["mypy3"]
   }
   ```

**Benefits:**
- **Reduced token usage** - Only get diagnostics you care about
- **Focus on critical issues** - Filter out style warnings during refactoring
- **Better UX** - Paginate through large diagnostic lists
- **Context-aware** - Get diagnostics for specific code regions

**Temporary AI Assistant Workaround (until tool is implemented):**

When calling `mcp__ide__getDiagnostics` and receiving large results, AI assistants should:

1. **Prioritize errors** - Scan results for `"severity": "Error"` entries first
2. **Ignore style warnings** - Skip pylint warnings like "logging-fstring-interpolation", "too-many-return-statements"
3. **Focus on type errors** - Pay attention to mypy3 errors about incompatible types
4. **Report summary** - Tell user "Found X errors, Y warnings" instead of listing all
5. **Example response:**
   ```
   LSP diagnostics show:
   - 3 mypy type errors (lines 42, 108, 201)
   - 45 pylint style warnings (can be ignored for now)

   The critical issues are:
   1. Line 42: Incompatible types in function call
   2. Line 108: Undefined variable 'foo'
   3. Line 201: Missing return type annotation
   ```

**MCP Registration:**
```elisp
(claude-code-ide-mcp-register-tool
 "get-diagnostics-filtered"
 "Get LSP diagnostics with filtering options to reduce context window usage"
 '((file_path . string)
   (severity_filter . array)
   (line_range . object)
   (limit . number)
   (offset . number)
   (source_filter . array))
 #'claude-code-mcp-get-diagnostics-filtered)
```

---

## Questions for Review

1. Should `reload-buffer` auto-reload after every Edit/Write, or require explicit call?
2. Should `run-elisp` be allowlist-only, or support confirmation dialog?
3. Should we add a `project_root` parameter to restrict operations to project?
4. Should navigation commands respect user's window/frame preferences?
5. Should we add undo/redo support (revert changes via Emacs)?

---

## References

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/elisp.html)
- [xref API](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
- [LSP Mode](https://emacs-lsp.github.io/lsp-mode/)
