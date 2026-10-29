# Gloom: Evil instead of Meow

Status: implemented (awaiting the user's GUI trial)

## Scope
Replace Meow with Evil in Gloom, mirroring the Doom config's Evil setup.
Gloom only; Sheprd already configures its sidebar/board for Evil.

## Approach
- editing.el: Evil (undo-redo, C-u scroll, Y to eol, keep visual on shift),
  evil-collection for special modes, evil-surround, evil-nerd-commenter (gc).
- Leader: the C-c prefix map on SPC (normal/visual/motion) and M-SPC (all
  states) through an Evil intercept map (gloom-override-mode), which also
  carries normal/motion C-h/j/k/l window focus.
- Doom config tweaks mirrored: visual-line j/k, $ = last non-blank, w/W =
  viw/viW, C-q in insert/replace, ç/Ç hunks.
- Terminals: evil-ghostel (upstream) replaces the Meow↔Ghostel bridge; Gloom
  keeps C-s consult-line and C-k kill-line in insert state. Pilish uses
  pilish-evil. Dired h/l kept over evil-collection.
- packages/install: Meow removed; evil, evil-collection, evil-surround,
  evil-nerd-commenter, evil-ghostel (from Ghostel's repo) added; all present
  in Doom's store.

## Tasks
- [x] Config, packages, installer.
- [x] Tests rewritten for Evil; docs (README, PARITY, docs/emacs.md).
- [ ] GUI trial by the user; closeout.

## Evidence
- 2026-10-29: Gloom suite 31/31 in batch with no extra load path (Meow no
  longer needed). Fixes found by tests: evil-ghostel passthrough overrode
  Gloom's insert-state C-s/C-k; visual selection checks use evil-visual-*.
- Not run: graphical launch under Evil (cursor shapes, clipboard via pbpaste,
  Claude Code Escape routing in evil-ghostel).
