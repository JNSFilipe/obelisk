# Gloom Sheprd navigation

Work ID: meow-emacs
Origin: independent apply
Owner: none
Work state: complete

## Scope and behavior

Explicit C-h/j/k/l navigation reaches editor windows, Ghostel terminal windows,
and the Sheprd sidebar. Ghostel starts in input state; Escape enters normal
scrollback navigation and i resumes terminal input. Native shell controls remain
available in input state. Sidebar buttons and j/k retain native behavior, and its
side-window layout protections remain intact. No shared Doom source changes.

## Diagnosis

The sidebar sets no-other-window, so windmove skips it. Gloom disables Meow in
Ghostel, so normal navigation bindings do not apply there. Previous navigation
evidence covered ordinary editor splits only and did not establish Sheprd support.
User confirmed both terminal and sidebar are affected.

## Tasks

- [x] Reproduce navigation failures in mixed windows.
- [x] Explicit directional focus includes protected sidebar windows.
- [x] Synchronize Meow normal/input with Ghostel native read-only/input modes.
- [x] Check bidirectional navigation, shell input, clipboard copy, and layout safety.
- [x] Run standalone/shared regressions and graphical real-terminal checks.
- [x] Reconcile documentation and archive.

## Evidence

Source inspection: configs/gloom/editing.el, shared configs/doom/sheprd.el,
installed Meow 1.5.0 and Ghostel libraries.

Before fix: window-in-direction left returned nil despite the real protected
side window; IGNORE=t found it. Ghostel had Meow=nil, C-h=help-command,
Escape=ghostel--send-event. This reproduced both barriers.

After fix: 25/25 Gloom ERT checks passed in isolated state on Emacs Plus 31.1
(log /tmp/gloom-nav-all.log). Mixed 2x2 editor/native terminal + actual Sheprd
sidebar test exercises every direction, direct return from sidebar, terminal
Escape/i, continuing native output, clipboard dispatch, actual keyboard PTY
input, retained sidebar protections and no-neighbour error. Native Copy/Emacs/
char/semi-char transitions and temporary Meow keypad use also checked.
Shared unchanged Sheprd regression suite: 36/36 passed.

First mixed test found native M-w exits read-only after copying; fixed the
terminal-local Meow copy command to call kill-ring-save directly, retaining
Ghostel's native text filter and normal state. Test then passed without an
extra literal i being sent to the PTY.

Graphical isolated startup with three focused regressions and shared Sheprd GUI integration passed through macOS Launch Services on 2026-10-06. Final cursor refinement preserves a steady Meow-owned cursor by ignoring terminal cursor requests. On 2026-10-07, final batch suite passed 25/25 (/tmp/gloom-nav-final-batch.log); final focused GUI checks and shared integration passed after loading Gloom in an initialized graphical frame (/tmp/gloom-navigation-gui-final-result).

Validation limits: a broader GUI run of the batch-oriented suite timed out with no result and is not counted as passed. A subsequent fresh-state normal startup aborted in Emacs native font_find_for_lface/char_displayable_p while loading completion code; exact cause unestablished. Delaying the harness config load until a graphical frame existed allowed all focused GUI checks to pass. The earlier direct subprocess launch failed during macOS application registration; Launch Services resolved that separate launch issue. No isolated timed-out verification process remained when inspected on 2026-10-07. No production workaround or global native-compilation disablement was added.

## Next

none for the requested navigation repair. Separate debt: investigate fresh-state Emacs native font startup crash if it occurs in normal use; navigation behavior is verified independently.

Documentation reconciled: configs/gloom/README.md and PARITY.md. Shared Doom implementation and unrelated working-tree edits preserved. No packages added.
