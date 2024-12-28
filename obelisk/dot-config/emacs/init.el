;; Early init performance optimizations
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Initialize package system
(setq package-enable-at-startup nil)

;; Initialize elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                             :ref nil
                             :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                             :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-compile-file \"elpaca.el\")"))))
            (progn (require 'elpaca)
                   (elpaca-generate-autoloads "elpaca" repo)
                   (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo t)))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Wait for elpaca to be ready
(elpaca-wait)

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

;; #############################################################################
;; Helper functions
;; #############################################################################
(defun vemacs/marker-is-point-p (marker)
  "Test if MARKER is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun vemacs/push-mark-maybe ()
  "Push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (vemacs/marker-is-point-p (car global-mark-ring))
                (vemacs/marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

(defun vemacs/backward-global-mark ()
  "Use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (vemacs/push-mark-maybe)
  (when (vemacs/marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun vemacs/forward-global-mark ()
  "Hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (vemacs/push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (vemacs/marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

(defun vemacs/find-file ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'find-file)))

(defun vemacs/dired ()
  (interactive)
  (if (projectile-project-p)
      (dired (projectile-project-root))
    (dired "~/")))

(defun vemacs/xy-window-pixel-ratio ()
  "Return the ratio of the window's width to its height in pixels."
  (interactive)
  (let* ((edges (window-pixel-edges))
         (width (- (nth 2 edges) (nth 0 edges)))
         (height (- (nth 3 edges) (nth 1 edges)))
         (ratio (/ (float width) height)))
    (if (called-interactively-p 'interactive)
        (message "Width/Height Ratio: %f" ratio)
      ratio)))

(defun vemacs/auto-split-window ()
  "Split the current window along its biggest dimension and run `projectile-find-file`."
  (interactive)
  (if (> (vemacs/xy-window-pixel-ratio) 1.0)
      (split-window-horizontally)       ; Wider window, split horizontally
    (split-window-vertically))          ; Taller window, split vertically
  (other-window 1)
  (projectile-find-file))

(defun vemacs/indent-region (num-spaces)
  "Indent or unindent the selected region by NUM-SPACES."
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (indent-rigidly (point) (min (1+ end) (line-end-position)) num-spaces)
            (forward-line)))
        (setq deactivate-mark nil))
    (if (< num-spaces 0) (vemacs/forward-global-mark) (vemacs/backward-global-mark))))

(defun vemacs/meow-append ()
  ;; https://github.com/meow-edit/meow/issues/43
  (interactive)
  (unless (region-active-p) (forward-char 1))
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--direction-forward)
    (when (bound-and-true-p delete-selection-mode)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun vemacs/meow-left ()
  (interactive)
  (if (region-active-p)
      (meow-left-expand)
    (meow-left)))

(defun vemacs/meow-next ()
  (interactive)
  (if (region-active-p)
      (meow-next-expand 1)
    (meow-next 1)))

(defun vemacs/meow-prev ()
  (interactive)
  (if (region-active-p)
      (meow-prev-expand 1)
    (meow-prev 1)))

(defun vemacs/meow-right ()
  (interactive)
  (if (region-active-p)
      (meow-right-expand)
    (meow-right)))

(defun vemacs/meow-grab-or-go-to-bottom ()
  (interactive)
  (if (region-active-p)
      (meow-grab)
    (meow-end-of-thing 'buffer)))

(defun vemacs/meow-kill-or-line ()
  (interactive)
  (if (region-active-p)
      (meow-kill)
    (meow-line 1)))

;; Basic settings
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq default-frame-alist '((undecorated . t)))
(global-display-line-numbers-mode 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(global-auto-revert-mode 1)
(winner-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

;; Package configurations
(use-package ido
  :ensure nil
  :config
  (ido-mode 1)
  (ido-everywhere 1))

(use-package ido-completing-read+
  :after ido
  :config
  (icu-mode 1))

(use-package smex
  :after ido
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

;; MEOW
(use-package meow
  :demand t
  :bind
  ;; Define CapsLock as a Nabla and use it as a modifier (https://www.emacswiki.org/emacs/CapsKey#toc5)
  ;; Use nabla (aka CapsLock) as leader (https://www.emacswiki.org/emacs/CapsKey#toc5)
  ;; ("∇" . meow-keypad)
  :config
  (setq meow-use-clipboard t) ;;
  (defun meow-setup ()
    ;; Enable modeline indicator
    (meow-setup-indicator)
    ;; Define prefixes that are baypassed to keypad mode
    (setq meow-keypad-start-keys '((?x . ?x)))
    ;; Define the layout
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     '("?" . meow-cheatsheet)
     '(":" . execute-extended-command)
     '(";" . eval-expression)
     '("." . ibuffer)
     '("," . scratch-buffer)
     '("*" . project-search)
     '("'" . text-scale-adjust)
     '("b" . consult-buffer)
     '("c" . comment-line)
     '("w" . save-buffer)
     '("a" . lsp-execute-code-action)
     '("o" . vemacs/dired)
     '("f" . vemacs/find-file)
     '("h" . replace-string)
     '("d" . consult-flymake)
     '("t" . eat)
     '("T" . eat-project)
     '("r" . async-shell-command)
     '("m" . compile)
     ;; '("u" . undo-tee-visualize)
     '("s" . vemacs/auto-split-window))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("/" . meow-visit)
     '("I" . meow-inner-of-thing) ;; '("," . meow-inner-of-thing)
     '("A" . meow-bounds-of-thing) ;; '("." . meow-bounds-of-thing)
     '("," . meow-beginning-of-thing) ;; '("[" . meow-beginning-of-thing)
     '("." . meow-end-of-thing) ;; '("]" . meow-end-of-thing)
     '("a" . vemacs/meow-append)
     '("o" . meow-open-below) ;; '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("x" . meow-delete) ;; '("d" . meow-delete)
     '("X" . meow-backward-delete) ;; '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . vemacs/meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("O" . meow-open-above) ;; '("I" . meow-open-above)
     '("j" . vemacs/meow-next)
     '("J" . meow-next-expand)
     '("k" . vemacs/meow-prev)
     '("K" . meow-prev-expand)
     '("l" . vemacs/meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("Q" . meow-block) ;; '("o" . meow-block)
     ;; '("C-B" . meow-to-block) ;; '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . meow-yank-pop) ;; This presents a paste menu
     '("q" . meow-quit)
     ;; '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("d" . vemacs/meow-kill-or-line)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("V" . meow-line) ;; '("x" . meow-line)
     '(":" . meow-goto-line) ;; '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("ç" . git-gutter:next-hunk)
     '("Ç" . git-gutter:previous-hunk)
     '("<tab>" .  (lambda () (interactive) (vemacs/indent-region 2)))
     '("<backtab>" .  (lambda () (interactive) (vemacs/indent-region -2)))
     '("<escape>" . meow-cancel-selection) ;; '("<escape>" . ignore)
     '("∇" . consult-global-mark)
     '("C-h" . windmove-left)
     '("C-l" . windmove-right)
     '("C-k" . windmove-up)
     '("C-j" . windmove-down)
     '("C-q" . delete-window)))
  (meow-setup)
  (meow-global-mode 1))

(use-package eglot
  :hook ((prog-mode . eglot-ensure)))

(use-package rust-mode
  :hook (rust-mode . eglot-ensure))

(use-package go-mode
  :hook (go-mode . eglot-ensure))

(use-package zig-mode
  :hook (zig-mode . eglot-ensure))

(use-package lua-mode
  :hook (lua-mode . eglot-ensure))

(use-package markdown-mode)

(use-package magit)

(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))

;; Language specific settings
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)))

;; Font settings
(set-frame-font "Source Code Pro-12" nil t)

;; Save customizations to separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Restore gc-cons-threshold
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))
