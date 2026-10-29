;;; editing.el --- Meow and native Emacs commands -*- lexical-binding: t; -*-
(gloom-require 'meow)
(transient-mark-mode 1)
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
      meow-use-clipboard t
      ;; C-k is window-up in normal state; Meow's kill fallback must call the
      ;; editing command itself instead of resolving that rebound key.
      meow--kbd-kill-line #'kill-line)

(defun gloom-focus-window (direction)
  "Focus the adjacent window in DIRECTION, including Sheprd's sidebar.
Ignore no-other-window only for this explicit command; retain sidebar layout
protections and exclude the minibuffer."
  (let ((window (window-in-direction direction nil t nil nil 'never)))
    (unless window (user-error "No window %s" direction))
    (select-window window)
    (when (fboundp 'sheprd--seen-here) (sheprd--seen-here))))
(defun gloom-window-left () (interactive) (gloom-focus-window 'left))
(defun gloom-window-down () (interactive) (gloom-focus-window 'below))
(defun gloom-window-up () (interactive) (gloom-focus-window 'above))
(defun gloom-window-right () (interactive) (gloom-focus-window 'right))
(defconst gloom-window-keys
  '(("C-h" . gloom-window-left) ("C-j" . gloom-window-down)
    ("C-k" . gloom-window-up) ("C-l" . gloom-window-right)))
(dolist (binding gloom-window-keys)
  (keymap-set meow-normal-state-keymap (car binding) (cdr binding))
  (keymap-set meow-motion-state-keymap (car binding) (cdr binding)))

(defun gloom-meow-insert-line-start ()
  "Insert at the first non-whitespace character, like Vim's I."
  (interactive)
  (when (region-active-p) (meow-cancel-selection))
  (back-to-indentation)
  (meow-insert))
(defun gloom-meow-append-line-end ()
  "Insert at the end of the line, like Vim's A."
  (interactive)
  (when (region-active-p) (meow-cancel-selection))
  (end-of-line)
  (meow-insert))
(defun gloom-meow-visual ()
  "Toggle a character selection; Meow motions extend it without a new state."
  (interactive)
  (if (region-active-p) (meow-cancel-selection) (meow-right-expand)))
(defun gloom-meow-search-backward ()
  (interactive)
  (meow-search -1))
(defun gloom-meow-buffer-start ()
  (interactive)
  (meow-prev '(4)))
(defun gloom-meow-buffer-end ()
  (interactive)
  (meow-next '(4)))

;; Read-only/motion buffers also support copying an ordinary marked region.
(meow-motion-define-key '("j" . meow-next) '("k" . meow-prev)
                       '("y" . meow-save))
(defvar gloom-meow-g-map (make-sparse-keymap))
(keymap-set meow-normal-state-keymap "g" gloom-meow-g-map)
(meow-normal-define-key
 '("h" . meow-left) '("j" . meow-next) '("k" . meow-prev) '("l" . meow-right)
 '("H" . meow-left-expand) '("J" . meow-next-expand)
 '("K" . meow-prev-expand) '("L" . meow-right-expand)
 '("i" . meow-insert) '("a" . meow-append)
 '("I" . gloom-meow-insert-line-start) '("A" . gloom-meow-append-line-end)
 '("o" . meow-open-below) '("O" . meow-open-above)
 '("w" . meow-mark-word) '("W" . meow-mark-symbol)
 '("b" . meow-back-word) '("e" . meow-next-word)
 '("v" . gloom-meow-visual) '("V" . meow-line) '("x" . meow-delete)
 '("c" . meow-change) '("s" . meow-change) '("d" . meow-kill)
 '("y" . meow-save) '("p" . meow-yank) '("r" . meow-replace)
 '("u" . meow-undo) '("C-r" . undo-redo) '("G" . gloom-meow-buffer-end)
 '("gg" . gloom-meow-buffer-start) '("gr" . meow-grab) '("gb" . meow-block)
 '("/" . meow-visit) '("n" . meow-search) '("N" . gloom-meow-search-backward)
 '("f" . meow-find) '("t" . meow-till)
 '("0" . beginning-of-line) '("^" . back-to-indentation) '("$" . end-of-line)
 '(";" . meow-reverse) '("z" . meow-pop-selection)
 '("[" . meow-beginning-of-thing) '("]" . meow-end-of-thing)
 '("," . meow-inner-of-thing) '("." . meow-bounds-of-thing)
 '("'" . repeat) '("<escape>" . meow-cancel-selection)
 '("ç" . diff-hl-next-hunk) '("Ç" . diff-hl-previous-hunk))
(dotimes (digit 9)
  (define-key meow-normal-state-keymap (number-to-string (1+ digit))
              (intern (format "meow-expand-%d" (1+ digit)))))

(defun gloom-new-terminal ()
  "Open a new Ghostel terminal, even when one already exists."
  (interactive)
  (ghostel t))
(defun gloom-terminal-split (direction)
  "Split the selected window toward DIRECTION and open a new terminal there."
  (select-window (split-window nil nil direction))
  (gloom-new-terminal))
(defun gloom-terminal-split-right () (interactive) (gloom-terminal-split 'right))
(defun gloom-terminal-split-down () (interactive) (gloom-terminal-split 'below))

;; Meow's leader is C-c: the same commands work in insert state and terminals.
;; Keep C-c s reserved for Sheprd; TAB is its additional familiar leader key.
;; Without sessions (`gloom-sessions' nil) the same keys open plain Ghostel
;; terminals and switch among all buffers.
(if gloom-sessions
    (meow-leader-define-key
     '("t" . sheprd-new-terminal) '("," . sheprd-split-right)
     '("<" . sheprd-split-down) '("b" . persp-switch-to-buffer))
  (meow-leader-define-key
   '("t" . gloom-new-terminal) '("," . gloom-terminal-split-right)
   '("<" . gloom-terminal-split-down) '("b" . consult-buffer)))
(meow-leader-define-key
 '("SPC" . execute-extended-command) '("X" . eval-expression)
 '("v" . consult-line) '("g" . consult-ripgrep) '("G" . magit-status)
 '("w" . save-buffer) '("o" . dired-jump) '("f" . projectile-find-file)
 '("c" . comment-line)
 '("D" . gloom-diagnostics) '("r" . async-shell-command)
 '("m" . makefile-executor-execute-project-target) '("M" . compile)
 '("u" . vundo) '("y" . consult-yank-pop) '("p" . projectile-switch-project)
 '("." . ibuffer) '(";" . scratch-buffer) '("?" . meow-cheatsheet)
 '("/" . meow-keypad-describe-key))
(when gloom-sessions
  (define-key mode-specific-map (kbd "TAB") sheprd-command-map)
  (define-key mode-specific-map (kbd "<tab>") sheprd-command-map)
  (defun gloom-sidebar-keys ()
    (keymap-set sheprd-sidebar-mode-map "SPC" mode-specific-map)
    (dolist (binding gloom-window-keys)
      (keymap-set sheprd-sidebar-mode-map (car binding) (cdr binding))))
  (add-hook 'sheprd-sidebar-mode-hook #'gloom-sidebar-keys)
  (gloom-sidebar-keys))
(keymap-set global-map "C-x b"
            (if gloom-sessions #'persp-switch-to-buffer #'consult-buffer))
(keymap-set global-map "C-q" #'kill-current-buffer)
(keymap-set global-map "C-x u" #'vundo)
(keymap-set global-map "M-." #'xref-find-definitions)
(keymap-set global-map "M-?" #'xref-find-references)
(keymap-set global-map "M-/" #'completion-at-point)
(keymap-set global-map "C-." #'embark-act)
(keymap-set global-map "M-a" #'avy-goto-char-timer)
(keymap-set global-map "C-c - s" #'flyspell-mode)
(keymap-set global-map "C-c - h" #'hs-toggle-hiding)
(keymap-set global-map "C-c - l" #'display-line-numbers-mode)
(keymap-set global-map "C-c S" #'split-window-below)
(keymap-set global-map "C-c |" #'split-window-right)

;; Ghostel owns PTY input and scrollback. Meow normal state uses Ghostel's live
;; read-only mode; insert state returns to its native semi-char input map.
(defvar gloom-ghostel-syncing nil)
(defun gloom-ghostel-meow-state (state)
  "Keep Ghostel's native input mode consistent with Meow STATE."
  (when (and (derived-mode-p 'ghostel-mode) meow-mode
             (not gloom-ghostel-syncing))
    (let ((gloom-ghostel-syncing t))
      (pcase state
        ('normal
         (unless (memq ghostel--input-mode '(copy emacs))
           (ghostel-emacs-mode)))
        ('insert
         (when (memq ghostel--input-mode '(copy emacs))
           (ghostel-semi-char-mode)))))))
(defun gloom-ghostel-native-state (&rest _)
  "Follow native Ghostel mode switches, including mouse selection and exits."
  (when (and (derived-mode-p 'ghostel-mode) meow-mode
             (not gloom-ghostel-syncing))
    (let ((gloom-ghostel-syncing t))
      (meow--switch-state
       (if (memq ghostel--input-mode '(copy emacs)) 'normal 'insert)))))
(add-hook 'meow-switch-state-hook #'gloom-ghostel-meow-state)
(with-eval-after-load 'ghostel
  ;; Ghostel has no input-mode change hook. Observe its existing transition
  ;; functions rather than replacing its PTY, redraw or selection machinery.
  (dolist (command '(ghostel--enter-readonly ghostel-semi-char-mode
                    ghostel-char-mode ghostel-line-mode ghostel-readonly-exit))
    (advice-add command :after #'gloom-ghostel-native-state)))

;; Preserve native sidebar buttons and special-mode letter commands.
(setq meow-mode-state-list
      (append '((ghostel-mode . insert) (sheprd-sidebar-mode . motion)
                (dired-mode . motion)) meow-mode-state-list))
(defvar gloom-meow-native-modes
  '(sheprd-sidebar-mode
    ;; M-x Machina's views bind their own single keys (j/k, RET, n, z, ...),
    ;; and its EAT terminals switch EAT's input modes themselves.
    mx-machina-mode mx-machina-sidebar-mode mx-machina-board-mode
    mx-machina-actions-mode mx-machina-diagnostics-mode
    mx-machina-transcript-mode eat-mode)
  "Major modes whose buffers run without Meow, keeping their native keys.")
(defun gloom-meow-buffer-policy ()
  (when meow-mode
    (cond ((apply #'derived-mode-p gloom-meow-native-modes) (meow-mode -1))
          ((derived-mode-p 'ghostel-mode)
           ;; Native M-w copies and exits to the shell. Meow y should keep
           ;; normal state so the next navigation key cannot become PTY input.
           (setq-local meow--kbd-kill-ring-save #'kill-ring-save
                       ;; Keep the requested steady, Meow-owned cursor even
                       ;; when a terminal application requests its own blink.
                       ghostel-ignore-cursor-change t)
           (gloom-ghostel-native-state)))))
(add-hook 'meow-mode-hook #'gloom-meow-buffer-policy)
(meow-global-mode 1)
(provide 'gloom-editing)
