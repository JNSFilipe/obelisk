# Fresh terminals and persistent scratch

Status: complete

## Why / scope
Fix leader t creating independent Ghostel shells and leader comma preserving a
shared scratch shell and restoring the editing layout. Preserve unrelated edits.

## Behavior
Target: configs/doom/sheprd.md
- Every leader t invocation creates a fresh terminal in the editing window.
- Scratch show/hide reuses one shell; repeated show preserves the original layout.
- Layout restoration is frame-local; workspace switches first dismiss scratch.
- Scratch remains free of workspace ownership across repeated display hooks.
- Spawn failure restores the layout; hidden command input preserves it.

## Approach
Use Ghostel's nonnumeric prefix for fresh shells. Use frame parameters for layout,
persp's permanent buffer filter, and restore before workspace switch.

## Tasks
- [x] Implement and run regressions for fresh creation and scratch lifecycle.
- [x] Verify actual Ghostel/Doom behavior and reconcile documentation.

## Evidence
Original helper passed nil to Ghostel (documented slot reuse). Scratch stored a
single global configuration and overwrote it on every show. Doom registers buffers
on every switch, disproving the one-time idle-detach assumption.

- 2026-09-10: 26/26 ERT tests pass using Homebrew Emacs batch -Q,
  loading scratch-term-test.el and sheprd-test.el. Seven new regressions cover
  fresh creation, repeat show, failure rollback, sidebar toggle, workspace switch,
  filtering, and hidden spawn layout preservation.
- Strict scratch-term byte compilation (warnings as errors) and scoped diff
  whitespace checks pass.
- Saved scratch-term-integration-check returned `passed` in the running Doom
  daemon with temporary macOS GUI frames and actual native Ghostel processes.
  Verified distinct live shells, Escape binding, repeated-show restoration,
  scratch reuse, per-frame layouts, workspace switch dismissal, permanent
  ownership filtering, and shell survival after temporary workspace removal.
- Test harness corrections: initial GUI creation needed window-system ns;
  batch window geometry needed initialization; GUI test needed correct frame
  scope and persp-kill rather than Doom saved-workspace deletion. Final native
  test passed after these corrections; temporary resources cleaned up.
- Updated configs/doom/sheprd.md. Helper reloaded into the running daemon.
  Existing unrelated edits preserved; no commit or system activation needed.
