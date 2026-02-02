# Design Notes

This file contains design decisions and feature considerations for claude-code-ide.el.

## Buffer Management - Window Behavior Option

**Date:** 2025-11-23
**Status:** Not Implemented
**Decision:** Deferred

### Problem

When Claude uses `goto-location` or `reload-buffer` MCP tools, they currently always open/display files in another window and return focus to the Claude terminal. This prevents the Claude terminal from being replaced, which is generally desirable.

However, there may be cases where Claude should open a file in the current window (replacing the terminal). The user originally requested the ability to control this behavior.

### Proposed Solution

Add an optional `use_other_window` parameter to both tools:
- **Default:** `true` - Opens in another window, keeps focus on Claude terminal (current behavior)
- **When `false`:** Opens in current window, replacing Claude terminal

### Implementation Details

Would require:

1. Add `use-other-window` parameter to function signatures:
   - `claude-code-ide-mcp-goto-location`
   - `claude-code-ide-mcp-reload-buffer`

2. Add conditional logic to check parameter value:
   ```elisp
   (let ((use-other (if (eq use-other-window :json-false) nil
                      (if use-other-window use-other-window t))))
     (if use-other
         (find-file-other-window file-path)
       (find-file file-path)))
   ```

3. Add parameter to tool registration schemas

4. Handle JSON boolean: `json-parse-string` with `:object-type 'alist` represents `false` as `:json-false` keyword

### Decision Rationale

**Deferred for now** because:
- Current behavior (always other window) works well for the primary use case
- Unclear when Claude would need to replace its own terminal window
- Can be added later if a clear use case emerges
- Simpler code is easier to maintain

### Related Files

- `mcp-tools.d/claude-code-ide-tool-buffer-management.el` - Would contain implementation
- `DEBUGGING_NOTES.md` - Contains extensive exploration of JSON boolean handling from debugging sessions

### Notes

Previous debugging sessions (Sessions 3-5 in DEBUGGING_NOTES.md) explored implementing this feature extensively, including discovering the correct JSON boolean representation (`:json-false`). However, the actual implementation was never completed, and testing revealed it's not currently needed.
