# Design Spec: God-Mode and Which-Key Integration

Enable native support in `which-key` to correctly display keybindings when using `god-mode` translations (e.g., `x` -> `C-x`).

## Problem
In `god-mode`, typing `x` or `c` translates to the `C-x` and `C-c` prefixes. However, `which-key` does not automatically recognize these translated prefixes to trigger its completion menu, leading to a "blind" experience for the user.

## Proposed Changes
### `init.el`
1. Update the `which-key` `use-package` block.
2. Add `(which-key-enable-god-mode-support)` within the `:config` section.
3. Ensure the call happens after `which-key-mode` is initialized.

```elisp
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  (which-key-enable-god-mode-support))
```

## Verification Plan
### Automated Tests
- None (standard Emacs package configuration).

### Manual Verification
1. Reload `init.el` or restart Emacs.
2. Activate `god-mode` (press `ESC`).
3. Press `x`.
4. **Expected Result:** After 0.5 seconds, the `which-key` popup should appear showing `C-x` bindings (e.g., `f` for `find-file`, `s` for `save-buffer`).
5. Press `c`.
6. **Expected Result:** After 0.5 seconds, the `which-key` popup should appear showing `C-c` bindings.
7. Verify that standard `C-x` and `C-c` (outside of God Mode) still trigger the menu as expected.
