# vjump.el — Design Spec

**Date:** 2026-04-06
**Status:** Approved

## Overview

`vjump.el` is a single-file Emacs package that maintains a session-scoped jump history as a **tree** and visualises it with the same horizontal UI as `vundo`. Navigating backward and then jumping elsewhere creates a branch — the discarded "future" is preserved, never lost.

---

## 1. Architecture

A global minor mode (`vjump-mode`) hooks into Emacs's jump-recording machinery. Three detection layers feed into a single internal `vjump--push` function. A visualization command (`vjump-visualize`) opens a `*vjump-tree*` side-window with live navigation identical in feel to `vundo`.

**Detection layers:**

| Layer | Mechanism | Covers |
|-------|-----------|--------|
| 1 | Advice on `push-mark` | `xref`, `isearch`, `goto-line`, `C-SPC`, and any command that already calls `push-mark` |
| 2 | `post-command-hook` distance check | Raw cursor movement > threshold lines |
| 3 | `post-command-hook` buffer-change check | Any command that switches the current buffer |

A boolean flag `vjump--push-mark-called` prevents double-recording when layers 1 and 2 fire for the same command.

**No persistence.** The tree exists only for the current Emacs session. On quit, it is gone.

---

## 2. Data Model

```elisp
(cl-defstruct vjump-node
  marker      ; Emacs marker — encodes both buffer and position
  parent      ; parent vjump-node, or nil for the sentinel root
  children    ; ordered list of child vjump-nodes (newest last)
  point       ; integer — text position of this node's glyph in *vjump-tree*
  timestamp)  ; float-time, for display purposes
```

**Global state:**

```elisp
(defvar vjump--root    nil) ; sentinel root node (no marker)
(defvar vjump--current nil) ; pointer to current position in tree
```

**Branching rule:** when `vjump--push` fires and `vjump--current` already has children, the new node is appended as a new child (sibling to existing children). Old children are never discarded. This is what makes the history a tree rather than a list.

**Back-navigation:** `pop-to-mark-command` (bound to `C-u C-SPC`) is advised to move `vjump--current` to its parent, keeping the tree intact. `vjump-go-back` and `vjump-go-forward` are public commands for tree-aware C-o / C-i navigation without opening the visualizer.

---

## 3. Visualization

Opens as a small side window (bottom by default, same as vundo). The tree is drawn horizontally with the same algorithm and glyphs as vundo.

**Example:**

```
○──○──○──○
│     └──●  ← current position (bar.py:93)
└──○──○
```

- `●` = current node, `○` = other nodes
- Each node shows `buffer-name:line` as an overlay (not in the tree text itself, to keep alignment clean)

**Draw engine:** direct port of `vundo--draw-tree`, with `vundo-m-*` accessors renamed to `vjump-node-*`. Glyph customization reuses `vundo-glyph-alist` if vundo is loaded; otherwise falls back to our own `vjump-glyph-alist` (same structure).

**Live jumping:** on every navigation keypress, the originating window is updated immediately:

```elisp
(with-selected-window vjump--orig-window
  (switch-to-buffer (marker-buffer (vjump-node-marker node)))
  (goto-char (marker-position (vjump-node-marker node))))
```

**Keymap** (identical to vundo):

| Key | Action |
|-----|--------|
| `f` / `→` | Forward to first child |
| `b` / `←` | Backward to parent |
| `n` / `↓` | Next sibling |
| `p` / `↑` | Previous sibling |
| `a` | Back to nearest branching point |
| `e` | Forward to tip of current branch |
| `q` / `C-g` | Quit, stay at current location |
| `RET` | Confirm and close |

`vundo-roll-back-on-quit` equivalent: `vjump-roll-back-on-quit` (default `t`). On `q`, point returns to the node that was current when `vjump-visualize` was called. On `RET`, it stays at wherever you navigated to.

---

## 4. Jump Detection Details

### push-mark advice

```elisp
(defun vjump--push-mark-advice (&rest _)
  (setq vjump--push-mark-called t)
  (vjump--push (point) (current-buffer)))
```

### post-command-hook

```elisp
(defun vjump--post-command ()
  (cond
   (vjump--push-mark-called
    ;; Already recorded by push-mark advice, just reset flag.
    (setq vjump--push-mark-called nil))
   ((not (eq (current-buffer) vjump--pre-command-buffer))
    ;; Buffer changed.
    (vjump--push (point) (current-buffer)))
   ((> (abs (- (line-number-at-pos (point))
               (line-number-at-pos vjump--pre-command-point)))
       vjump-distance-threshold)
    ;; Large distance movement.
    (vjump--push (point) (current-buffer)))))
```

`vjump--pre-command-point` and `vjump--pre-command-buffer` are captured in a `pre-command-hook`.

### pop-to-mark advice

```elisp
(defun vjump--pop-mark-advice (&rest _)
  (when (vjump-node-parent vjump--current)
    (setq vjump--current (vjump-node-parent vjump--current))))
```

---

## 5. File Structure

Single file: `vjump.el` (~400 lines)

```
vjump.el
├── Package header + Commentary       (GPL-3+, Requires: emacs 28.1)
├── Customization variables
│   ├── vjump-distance-threshold      (default 10)
│   ├── vjump-window-max-height       (default 3)
│   ├── vjump-window-side             (default 'bottom)
│   ├── vjump-roll-back-on-quit       (default t)
│   └── vjump-glyph-alist             (fallback, mirrors vundo-glyph-alist)
├── Faces
│   └── vjump-default/node/stem/branch-stem/highlight
├── Node struct (cl-defstruct vjump-node)
├── Global state vars
├── Core tree operations
│   ├── vjump--push
│   ├── vjump--go-back
│   └── vjump--go-forward
├── Jump detection
│   ├── vjump--pre-command-hook
│   ├── vjump--post-command-hook
│   └── vjump--push-mark-advice / vjump--pop-mark-advice
├── Draw engine (ported from vundo--draw-tree)
│   ├── vjump--draw-tree
│   ├── vjump--put-node-at-point
│   ├── vjump--get-node-at-point
│   ├── vjump--translate
│   └── vjump--next-line-at-column
├── Visualization buffer
│   ├── vjump-tree-mode (derived-mode special-mode, for *vjump-tree* buffer)
│   ├── vjump-tree-mode-map
│   ├── vjump--buffer
│   └── vjump--refresh-buffer
├── Navigation commands
│   ├── vjump-forward / vjump-backward
│   ├── vjump-next / vjump-previous
│   ├── vjump-stem-root / vjump-stem-end
│   ├── vjump-quit / vjump-confirm
│   └── vjump--jump-to-node
├── Public API
│   ├── vjump-visualize               (main entry point)
│   ├── vjump-go-back                 (C-o equivalent, tree-aware)
│   └── vjump-go-forward              (C-i equivalent, tree-aware)
├── vjump-mode (global minor mode)
└── (provide 'vjump)
```

---

## 6. Public API Summary

```elisp
;; Enable tracking globally
(vjump-mode 1)

;; Suggested bindings (mirrors vundo's C-x u)
(global-set-key (kbd "C-x j") #'vjump-visualize)
(global-set-key (kbd "C-o")   #'vjump-go-back)
(global-set-key (kbd "C-i")   #'vjump-go-forward)
```

---

## 7. Non-goals

- Persistence across sessions (session-only by design)
- Diff between nodes (vundo has this for undo states; jumps have no meaningful diff)
- Per-project or per-perspective scoping (global tree only)
- Integration with evil-mode jump list (separate concern)
