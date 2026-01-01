# Project instructions for Claude Code

# Default behavior of ediff in emacs

In a GUI environment, when I do ediff of two files, emacs will partition the
window into top and bottom portions, and open a small ediff window (the ediff
control panel) near the top right corner of the main window. This small window
can expand to show help if I press "?"

In a terminal environment, the ediff panel appears as a single-line buffer
below the second buffer and above the messages buffer, and expands into a
multiline buffer if I press "?"

# How Claude Code IDE is doing it

The emacs IDE for Claude Code seems to assume that it's working in a
full-screen terminal, and opens the ediff control window accordingly. Also, it
opens the two buffers - current content and proposed changes - in side-to-side
windows.

# What is problematic with the IDE approach

The side-to-side windows make it difficult to see the full line even if the
window is full-screen. Additionally, setting the ediff control window to be
terminal-like changes its size settings even if I want to do regular ediff
outside of Claude Code IDE

# What I would like to happen

1. Have a settings variable that allows the user to choose between
   top-and-bottom placement of the two buffers to be compared (vertical
   partitioning), vs side-to-side comparison of the two buffers (horizontal
   partitioning). Have this set to top-bottom (vertical) by default
   
2. Have another settings variable to control whether the ediff control window
   opens below the buffers, terminal-style, or as a small separate window,
   GUI-style. If emacs is run from a GUI, have this set to GUI-style by
   default, but set it to terminal-style if emacs is run from a terminal.
   
# Window opening behavior of ediff

Currently, when the IDE wants to show a diff between the current version and
the proposed changes, it hides the claude-code vterm buffer while the changes
are being shown. This prevents me from seeing the thought process from Claude
while I am reviewing the changes.

# Desired behavior

If emacs is being run from a terminal, keep the behavior that was there
before, as controlled by claude-code-ide-show-claude-window-in-ediff and
claude-code-ide-switch-tab-on-ediff etc.

If emacs is being run from a GUI, take the following approach:

  If claude-code-ide-show-claude-window-in-ediff is nil, then show the
  comparison buffers in the same frame where the claude-code buffer is, and hide
  that buffer while ediff is active. Once the ediff session is finished, restore
  the frame to its previous state, including displaying the claude-code buffer
  as before

  If claude-code-ide-show-claude-window-in-ediff is t, act according to the
  following two cases - 

    a. If there is only one frame open for the current emacs session, then open
    a new frame and show the comparison buffers there, so that the claude-code
    buffer in the current frame stays visible
    
    b. If there is more than one frame open, the show the comparison buffers
    in a different frame than the one showing the claude-code buffer
    
# Update CLAUDE.md and README.org

Update these two files to reflect the current version of the code, including
the changes in 83f18f3 and f5c1ec8. Document the effect of
claude-code-ide-show-claude-window-in-ediff and
claude-code-ide-focus-claude-after-ediff in GUI mode.
