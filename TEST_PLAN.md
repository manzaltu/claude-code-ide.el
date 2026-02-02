# Test Plan: Buffer Management `use_other_window` Feature

**Date:** 2025-11-23
**Branch:** jdb-new-mcp-commands
**Commit:** 2e8bbcb

---

## 🚨 RESUME HERE - SESSION 6 🚨

### Status: Fix Applied, Needs Verification

**What happened in Session 5:**
- ✅ Re-ran all 8 tests, Test 2 failed (use_other_window: false ignored)
- ✅ Found root cause: Lines 131 & 171 checked for `:false` instead of `:json-false`
- ✅ Applied correct fix: Changed both to `:json-false` (verified by grepping codebase)
- ✅ Deployed: Copied to installed package and recompiled
- ✅ User restarted MCP server
- ⏳ **Session ended before verification**

**NEXT STEPS - Verify these 3 tests:**

1. **Test 2** - goto-location with use_other_window: false
   - Should open in CURRENT window (no "in other window" in output)

2. **Test 6** - reload-buffer with use_other_window: false
   - User visually confirm: opens in current window

3. **Test 4** - goto-location with highlight
   - User visually confirm: line is highlighted

**If all pass:** Commit the fix
**If fail:** Check DEBUGGING_NOTES.md Session 5 for troubleshooting steps

---

## Overview

Testing the new `use_other_window` parameter added to buffer management tools:
- `claude-code-ide-mcp-goto-location`
- `claude-code-ide-mcp-reload-buffer`

## Prerequisites
- Claude Code session is active in a terminal buffer
- At least one other window is visible in Emacs frame
- Working directory: `/home/jdblair/src/claude-code-ide.el`

---

## Test Cases

### Test 1: goto-location with Default Behavior
**Command:** Ask Claude to navigate to a specific location without specifying `use_other_window`

**Expected Behavior:**
- File opens in a **different window** (not the Claude terminal)
- Cursor jumps to the specified line and column
- **Focus remains on Claude terminal window**
- Claude terminal buffer is not replaced

**Example:** "Navigate to line 50 in claude-code-ide.el"

**Status:** ✅ PASS

---

### Test 2: goto-location with use_other_window: false
**Command:** Explicitly request to open in the current window

**Expected Behavior:**
- File opens in the **current window** (Claude terminal)
- Cursor jumps to the specified line
- **Focus moves to the file buffer** (old behavior)
- Claude terminal is replaced/hidden

**Example:** "Navigate to line 100 in claude-code-ide.el, opening it in the current window"

**Status:** ❌ FAIL

---

### Test 3: goto-location with use_other_window: true
**Command:** Explicitly request to open in another window

**Expected Behavior:**
- Same as Test 1 (should be identical to default)
- File opens in a different window
- Focus remains on Claude terminal

**Status:** ⏳ Pending

---

### Test 4: goto-location with Highlight
**Command:** Navigate to a location with highlighting enabled

**Expected Behavior:**
- File opens in another window (default behavior)
- Target line is **temporarily highlighted**
- **Focus returns to Claude terminal** after highlighting
- Highlight fades after a moment

**Example:** "Show me line 150 in claude-code-ide-mcp.el and highlight it"

**Status:** ⏳ Pending

---

### Test 5: reload-buffer with Default Behavior
**Setup:** Make a change to a file outside Emacs, or ask Claude to edit a file first

**Expected Behavior:**
- Buffer reloads with fresh content from disk
- Reloaded buffer is **displayed in another window**
- **Focus remains on Claude terminal**
- No "file changed on disk" warnings

**Example:** "Reload claude-code-ide.el"

**Status:** ⏳ Pending

---

### Test 6: reload-buffer with use_other_window: false
**Expected Behavior:**
- Buffer reloads with fresh content
- Buffer is **displayed in current window** (Claude terminal)
- **Focus moves to the reloaded buffer**

**Status:** ⏳ Pending

---

### Test 7: reload-buffer with use_other_window: true
**Expected Behavior:**
- Same as Test 5 (identical to default)
- Buffer displayed in another window
- Focus remains on Claude terminal

**Status:** ⏳ Pending

---

### Test 8: Tool Schema Verification
**Command:** List MCP tools to verify parameter is registered

**Expected Behavior:**
- Both `goto-location` and `reload-buffer` tools show `use_other_window` parameter
- Parameter is marked as optional
- Parameter type is `boolean`
- Description mentions: "Open in another window and keep focus on current window (default: true)"

**Verification:** Check `claude mcp list-tools` or ask Claude to describe the tools

**Status:** ⏳ Pending

---

## Edge Cases to Test

1. **Single window layout:** What happens when there's only one window?
2. **File already open:** Navigate to a file that's already visible in another window
3. **Multiple windows:** Verify behavior with 3+ windows visible
4. **Invalid file path:** Error handling when file doesn't exist

---

## Test Results

Results will be documented here after each test execution.

### Test 1 Results
- [ ] PASS / [ ] FAIL
- Notes:

### Test 2 Results
- [ ] PASS / [ ] FAIL
- Notes:

### Test 3 Results
- [ ] PASS / [ ] FAIL
- Notes:

### Test 4 Results
- [ ] PASS / [ ] FAIL
- Notes:

### Test 5 Results
- [ ] PASS / [ ] FAIL
- Notes:

### Test 6 Results
- [ ] PASS / [ ] FAIL
- Notes:

### Test 7 Results
- [ ] PASS / [ ] FAIL
- Notes:

### Test 8 Results
- [ ] PASS / [ ] FAIL
- Notes:

---

## Summary

- **Total Tests:** 8
- **Passed:** 0
- **Failed:** 0
- **Pending:** 8

## Issues Found

(Document any issues discovered during testing)

## Conclusion

(Final assessment after all tests complete)
