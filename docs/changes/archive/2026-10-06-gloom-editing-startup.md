# Gloom startup warnings and modal editing
Work ID: meow-emacs | Origin: independent apply | State: complete
Source: 2026-10-06-gloom-feature-parity.md

## Scope and scenarios
Resolve the reported native compiler warnings without editing the shared Doom
package store. Reproduce Meow startup/key availability, configure Vim-familiar
keys while retaining Meow's selection-first operation, and make y copy a selected
or ordinarily marked region to the system clipboard. Preserve Ghostel/Sheprd
native keys, Dired navigation and normal Meta command handling.

## Tasks
- [x] Reproduce native compilation warnings and initial Meow state/key behavior.
- [x] Correct scoped compilation setup and modal startup behavior.
- [x] Configure Vim-familiar selection-first keys and clipboard copying.
- [x] Verify actual editing, fresh startup/compilation and graphical clipboard.
- [x] Reconcile README/parity notes.
- [x] Archive and finalize the receipt.

## Evidence / Next
Installed doom-snippets calls yas-load-directory without a compile-time require.
The fname warning occurs in persp-mode's optional Workgroups macro branch;
Gloom uses native window-state functions instead. Both reported warnings were
reproduced by native-compiling temporary copies of the shared sources. Clean
graphical startup confirms Meow is active; the existing map lacks gg/line
boundary keys and assigns I/A/o/v/G to unfamiliar operations. No startup error
was reproduced. The initial editing test also exposed the C-k windmove collision
in Meow's delete-without-selection fallback, now corrected by calling kill-line
directly. Batch Emacs defaults transient-mark-mode off, so actual selection-first
editing tests exposed that assumption; Gloom now explicitly enables marked
regions for consistent selection handling. Buffer jumps use Meow's native
commands and retain its selection cleanup. Copy tests pass for Meow selections
and ordinary marks, including read-only motion buffers.
Current ERT result: all 23 tests pass, including actual insertion/movement,
selection-first deletion, delete fallback, undo/redo and clipboard dispatch.
A fresh-state GUI experiment using an EMACSLOADPATH override aborted in native
macOS face_for_font/face_for_char code before a result was written. It is not a
Lisp startup error. That nonstandard test harness was discarded; no production
configuration change was made to address the native crash. Normal startup and
a fresh-state launch using the existing ELPA directory both subsequently passed
after running the interaction test on a displayed frame. The exact cause of the
nonstandard harness crash was not established.

Both graphical runs verified actual macOS clipboard text via /usr/bin/pbpaste
for normal Meow word selection and an ordinary mark in a read-only motion buffer,
then restored prior clipboard text and closed their own test instance. No Lisp
startup error occurred. Native JIT compilation remains enabled globally; the
fresh cache generated 33 other .eln files, no persp-mode/doom-snippets .eln, and
neither reported compiler warning recurred. Those libraries and the Yas startup
dependency path use existing bytecode to avoid unsupported compiler declarations;
shared sources were not edited and other warnings remain enabled. Cache output
lives in Gloom's state rather than its source directory. Diff hunk commands now
have explicit autoloads, so they are callable from the initial scratch buffer.

Final checks: all 23 ERT tests passed, covering actual Vim-style movements and
insertion, selection-first editing, undo/redo, deletion without a selection,
copy dispatch and preserved terminal/sidebar keys. All Gloom Elisp passed
check-parens and git diff --check passed. README and PARITY describe the resulting
keybindings, bytecode scope and evidence. No packages were added. A live terminal
frame clipboard test was not run; actual macOS graphical clipboard was verified.
Repair complete. Restart Gloom to use the changed startup and editing configuration.
