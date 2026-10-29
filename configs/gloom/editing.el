;;; editing.el --- Evil, its leader and native Emacs commands -*- lexical-binding: t; -*-
;; Evil reads these while loading.  evil-collection supplies the bindings of
;; special modes (Dired, Magit, Help...), so Evil's own are turned off.
(setq evil-want-keybinding nil
      evil-want-integration t
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol t
      evil-undo-system 'undo-redo
      evil-symbol-word-search t
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-keep-visual-state-on-shift t
      evil-esc-delay 0.01)
(gloom-require 'evil)
(evil-mode 1)
(gloom-require 'evil-collection)
(evil-collection-init)
(gloom-require 'evil-surround)
(global-evil-surround-mode 1)
(gloom-require 'evil-nerd-commenter)

(defun gloom-focus-window (direction)
  "Focus the adjacent window in DIRECTION, including Sheprd's sidebar.
Ignore no-other-window only for this explicit command; retain sidebar layout
protections and exclude the minibuffer."
  (let ((window (window-in-direction direction nil t nil nil 'never)))
    (unless window (user-error "No window %s" direction))
    (select-window window)
    (sheprd--seen-here)))
(defun gloom-window-left () (interactive) (gloom-focus-window 'left))
(defun gloom-window-down () (interactive) (gloom-focus-window 'below))
(defun gloom-window-up () (interactive) (gloom-focus-window 'above))
(defun gloom-window-right () (interactive) (gloom-focus-window 'right))
(defconst gloom-window-keys
  '(("C-h" . gloom-window-left) ("C-j" . gloom-window-down)
    ("C-k" . gloom-window-up) ("C-l" . gloom-window-right)))

;; The leader is the C-c prefix map, so every C-c command is also SPC in
;; normal, visual and motion states and M-SPC everywhere, as in Doom.  An
;; intercept map wins over evil-collection's per-mode bindings, so SPC and
;; the window keys reach the leader in Dired, Magit and the rest alike.
(defvar gloom-override-mode-map (make-sparse-keymap)
  "Keys that take precedence over every other Evil binding.")
(define-minor-mode gloom-override-mode
  "Gloom's leader and window keys, above every other Evil binding."
  :global t
  :keymap gloom-override-mode-map)
(evil-make-intercept-map gloom-override-mode-map)
(evil-define-key* '(normal visual motion) gloom-override-mode-map
  (kbd "SPC") mode-specific-map)
(evil-define-key* '(normal visual motion insert emacs) gloom-override-mode-map
  (kbd "M-SPC") mode-specific-map)
(dolist (binding gloom-window-keys)
  (evil-define-key* '(normal motion) gloom-override-mode-map
    (kbd (car binding)) (cdr binding)))
(gloom-override-mode 1)

;; The same Evil tweaks as the Doom config: visual-line j/k, $ before trailing
;; blanks, w/W select the word, C-q kills the buffer even while typing.
(evil-define-key* 'normal 'global
  "j" #'evil-next-visual-line
  "k" #'evil-previous-visual-line
  "$" #'evil-last-non-blank
  "w" "viw"
  "W" "viW"
  "gc" #'evilnc-comment-operator
  "ç" #'diff-hl-next-hunk
  "Ç" #'diff-hl-previous-hunk)
(evil-define-key* 'visual 'global "gc" #'evilnc-comment-operator)
(evil-define-key* '(insert replace) 'global (kbd "C-q") #'kill-current-buffer)

;; Leader keys (SPC or C-c).  Keep C-c s reserved for Sheprd; TAB is its
;; additional familiar leader key.
(defun gloom-leader-help ()
  "Show every leader key."
  (interactive)
  (which-key-show-keymap 'mode-specific-map))
(pcase-dolist (`(,key . ,command)
               '(("SPC" . execute-extended-command) ("X" . eval-expression)
                 ("t" . sheprd-new-terminal) ("," . sheprd-split-right)
                 ("<" . sheprd-split-down) ("v" . consult-line)
                 ("g" . consult-ripgrep) ("G" . magit-status) ("w" . save-buffer)
                 ("o" . dired-jump) ("f" . projectile-find-file)
                 ("b" . persp-switch-to-buffer) ("c" . comment-line)
                 ("D" . gloom-diagnostics) ("r" . async-shell-command)
                 ("m" . makefile-executor-execute-project-target) ("M" . compile)
                 ("u" . vundo) ("y" . consult-yank-pop)
                 ("p" . projectile-switch-project) ("." . ibuffer)
                 (";" . scratch-buffer) ("?" . gloom-leader-help)
                 ("/" . describe-key)))
  (keymap-set mode-specific-map key command))
;; Sheprd's agents on C-c a (SPC a), with M-x Machina's keys.
(keymap-set mode-specific-map "a" sheprd-agent-map)
(define-key mode-specific-map (kbd "TAB") sheprd-command-map)
(define-key mode-specific-map (kbd "<tab>") sheprd-command-map)
(defun gloom-sidebar-keys ()
  (keymap-set sheprd-sidebar-mode-map "SPC" mode-specific-map)
  (dolist (binding gloom-window-keys)
    (keymap-set sheprd-sidebar-mode-map (car binding) (cdr binding))))
(add-hook 'sheprd-sidebar-mode-hook #'gloom-sidebar-keys)
(gloom-sidebar-keys)
(keymap-set global-map "C-x b" #'persp-switch-to-buffer)
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

;; Terminals: evil-ghostel drives the shell's line editor from normal state
;; (operators, i/a/I/A at the prompt, p/P, u) and leaves full-screen apps such
;; as Claude Code their own Escape; C-c <escape> enters normal state there.
(gloom-require 'evil-ghostel)
(defun gloom-ghostel-evil ()
  "Enable Evil's terminal integration with a steady, Evil-owned cursor."
  (evil-ghostel-mode 1)
  (setq-local ghostel-ignore-cursor-change t))
(add-hook 'ghostel-mode-hook #'gloom-ghostel-evil)
;; Keep Gloom's typing-state choices over evil-ghostel's passthrough: C-s
;; searches the scrollback and C-k kills to the kill ring (init.el).
(evil-define-key* 'insert evil-ghostel-mode-map
  (kbd "C-s") #'consult-line
  (kbd "C-k") #'gloom-ghostel-kill-line)

;; Native keys where Gloom chose them over evil-collection's.
(with-eval-after-load 'dired
  (evil-define-key* 'normal dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file))
;; Sheprd's sidebar and board, and Pilish, configure Evil themselves.

(provide 'gloom-editing)
