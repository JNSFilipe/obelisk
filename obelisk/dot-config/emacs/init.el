;;; init.el -*- lexical-binding: t; -*-

;; Early init performance optimizations
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Initialize package system
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package via straight.el
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (use-package-always-defer t)
  (straight-use-package-by-default t))

;; #############################################################################
;; Helper functions
;; #############################################################################
(load-file (expand-file-name "oblsk.el" user-emacs-directory))

;; #############################################################################
;; Select completion framework
;; #############################################################################
;; (defvar completion-framework 'ido
(defvar completion-framework 'vertico
;; (defvar completion-framework 'helm
  "Completion framework to use. Options are 'vertico, 'helm, or 'ido.")
;; #############################################################################

;; Basic UI settings
(use-package emacs
  :defer nil
  :straight nil
  :custom
  (inhibit-startup-message t)
  (scroll-conservatively 101)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  (scroll-step 1)
  (scroll-margin 3)
  (scroll-conservatively most-positive-fixnum)
  (maximum-scroll-margin 0.5)
  (auto-window-vscroll nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil)
  (pixel-scroll-precision-mode t) ;; For Emacs 29+
  (load-prefer-newer t)
  (history-length 1000)
  (create-lockfiles nil)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-display-line-numbers-mode 1)
  (global-auto-revert-mode 1)
  (winner-mode 1)
  (show-paren-mode 1)
  (global-hl-line-mode 1)
  (savehist-mode t)
  (xterm-mouse-mode t) ;; Enable mouse support in terminal
  ;; Show both line and column numbers in the mode line
  (line-number-mode 1)
  (column-number-mode 1)
  ;; Set default indentation to 2 spaces
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  ;; Enable relative line numbers
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  ;; Restore gc-cons-threshold after startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 800000)))
  ;; Font settings
  (set-frame-font "FiraCode Nerd Font Mono-12" nil t))

;; Update transient, required by other packages (magit, gptel)
(use-package transient)

;;; COMPILATION
(use-package compile
  :straight nil
  :custom
  ;; (setq-default compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)
  (ansi-color-for-compilation-mode t))

;; PROCED
(use-package proced
  :straight nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user) ;; We can change interactively with `s'
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

;;; DIRED
(use-package dired
  :straight nil
  :config
  ;; https://emacs.stackexchange.com/a/5604
  (setq dired-dwim-target t))

;; Files and backup settings
(use-package files
  :defer nil
  :straight nil
  :custom
  (backup-by-copying t)
  (version-control t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  (delete-old-versions t)
  (auto-save-default t)
  (delete-by-moving-to-trash t)
  :config
  (let ((backup-dir (expand-file-name "backups" user-emacs-directory)))
    (unless (file-exists-p backup-dir)
      (make-directory backup-dir t))
    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,backup-dir t)))))

;; Add completion backends
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex))

;; Built-in completion configuration
(use-package minibuffer
  :defer nil
  :straight nil
  :custom
  ;; Complete if the candidate is unique.
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  :config
  (setq completion-styles '(basic partial-completion substring initials flex emacs22)))

;; Language-specific settings
(use-package prog-mode
  :defer nil
  :straight nil
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (setq indent-tabs-mode nil
                              tab-width 2)))))

;; Custom file settings
(use-package cus-edit
  :defer nil
  :straight nil
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Completion framework imports
(pcase completion-framework
  ('vertico
   (load-file (expand-file-name "vertico-framework.el" user-emacs-directory)))
  ('helm
   (load-file (expand-file-name "helm-framework.el" user-emacs-directory)))
  ('ido
   (load-file (expand-file-name "ido-framework.el" user-emacs-directory)))
  (_
   (message "Unknown completion framework: %s, defaulting to ido" completion-framework)
   (load-file (expand-file-name "ido-framework.el" user-emacs-directory))))

;; Recentf configuration
(use-package recentf
  ;; :defer nil
  :straight nil  ; built-in package
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never))

;; Enable cursor change in terminal
(use-package evil-terminal-cursor-changer
  :defer nil
  :config
  (unless (display-graphic-p)
    (etcc-on)))

;; Evil and related packages
(use-package evil
  :defer nil
  :init
  (setq evil-want-C-i-jump nil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.
  (setq evil-respect-visual-line-mode t)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-normal-state-cursor 'box)
  (blink-cursor-mode -1)

  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-define-key 'normal 'global
    "j" "gj"
    "k" "gk"
    "w" "viw"
    "W" "viW"
    "ç" 'diff-hl-next-hunk
    "Ç" 'diff-hl-previous-hunk
    (kbd "$") "g_" ;; https://stackoverflow.com/questions/20165596/select-entire-line-in-vim-without-the-new-line-character
    (kbd "<tab>") 'evil-jump-forward
    (kbd "<backtab>") 'evil-jump-backward)

  (evil-define-key 'visual 'global
    (kbd "$") "g_" ;; https://stackoverflow.com/questions/20165596/select-entire-line-in-vim-without-the-new-line-character
    (kbd "<") '(lambda () (interactive) (oblsk/indent-region 2))
    (kbd ">") '(lambda () (interactive) (oblsk/indent-region -2))
    (kbd "<tab>") '(lambda () (interactive) (oblsk/indent-region 2))
    (kbd "<backtab>") '(lambda () (interactive) (oblsk/indent-region -2)))

  ;; Window movement bindings
  (define-key evil-normal-state-map (kbd "C-h C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  ;; Additional keybindings
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (global-set-key (kbd "C-q") 'delete-window)

  (evil-mode 1))

(use-package evil-collection
  :defer nil
  :after evil
  :config
  (evil-collection-init))

;; Multi-cursor support
(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds))

(use-package which-key
  :defer nil
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.05)
  :config
  (which-key-mode))

(use-package general
  :after evil
  :config

  ;; Define the main leader keys
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Customize keybindings
  (my/leader-keys
    "SPC" '(fw-M-x :which-key "M-x")
    "f" '(fw-find-file :which-key "Find File")
    "b" '(fw-switch-buffer :which-key "Switch Buffer")
    "g" '(consult-ripgrep :which-key "Grep")
    "G" '(magit-status :which-key "Git")
    "c" '(comment-line :which-key "Comment")
    "m" '(oblsk/find-makefile-targets :which-key "Make")
    "M" '(compile :which-key "Compile")
    ;; "s" '(oblsk/auto-split-window :which-key "Split")
    "s" '((lambda ()
            (interactive)
            (split-window-horizontally)
            (other-window 1)
            (fw-find-file))
          :which-key "Split Horizontally")
    "S" '((lambda ()
            (interactive)
            (split-window-vertically)
            (other-window 1)
            (fw-find-file))
          :which-key "Split Vertically")
    "w" '(save-buffer :which-key "Save Buffer")
    "z" '(oblsk/toggle-window-layout :which-key "Toggle Zoom")
    "F" '(format-all-buffer :which-key "Autoformat Buffer")
    "a" '(with-editor-async-shell-command :which-key "Async Shell Command")

    ;; Bookmarks
    "k" '(oblsk/browse-bookmarks :which-key "Bookmarks")

    ;; Help and documentation
    "h"  '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "describe function")
    "hv" '(describe-variable :which-key "describe variable")
    "hk" '(describe-key :which-key "describe key")

    ;; LSP commands
    "l"  '(:ignore t :which-key "LSP")
    "la" '(lsp-execute-code-action :which-key "Code Actions")
    "ld" '(xref-find-definitions :which-key "Go to Definition")
    "lD" '(lsp-find-declaration :which-key "Go to Declaration")
    "li" '(lsp-find-implementation :which-key "Go to Implementation")
    "lt" '(lsp-find-type-definition :which-key "Go to Type Definition")
    "lR" '(xref-find-references :which-key "Find References")
    "lr" '(lsp-rename :which-key "Rename Symbol")
    "le" '(lsp-ui-flycheck-list :which-key "List Errors/Diagnostics")
    "lh" '(lsp-describe-thing-at-point :which-key "Help/Describe at Point")
    "ls" '(lsp-ui-imenu :which-key "Symbol Outline")

;; Debugging (DAP)
    "d"  '(:ignore t :which-key "Debug")
    "db" '(dap-breakpoint-toggle :which-key "Toggle Breakpoint") ;; <-- This line is now correct
    "dc" '(dap-continue :which-key "Continue")
    "dd" '(dap-debug :which-key "Start Debug Session")
    "dD" '(dap-debug-last :which-key "Debug Last Session")
    "di" '(dap-step-in :which-key "Step In")
    "do" '(dap-step-out :which-key "Step Out")
    "dn" '(dap-next :which-key "Next Line")
    "de" '(dap-eval :which-key "Eval Expression")
    "dq" '(dap-disconnect :which-key "Disconnect/Quit")
    "dr" '(dap-ui-repl :which-key "Open REPL")))

;; LSP (Language Server Protocol) support with lsp-mode
;; NOTE: You must install the language servers for your languages
;; (e.g., clangd, pylsp, rust-analyzer) for lsp-mode to work.
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp-deferred)
         (kill-emacs . #'lsp-shutdown-all-workspaces))
  :init
  ;; Set these before lsp-mode loads to prevent startup warnings
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-completion-provider :capf)
  (setq lsp-enable-snippet nil)
  :config
  ;; Enable which-key integration for better keybinding discovery
  (lsp-enable-which-key-integration t)
  ;; Set performance options
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil))

;; Optional but recommended: UI enhancements for lsp-mode
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  ;; --- Fixes for *Messages* buffer errors ---
  ;; The `lsp--on-idle` timer error is often caused by automatic UI updates
  ;; on hover. Disabling these makes the UI more stable.
  ;; You can still view documentation manually with `lsp-describe-thing-at-point`
  ;; (bound to `SPC l h` in your config).
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  ;; ------------------------------------------
  ;; Configure other UI elements.
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-imenu-window-width 30)
  (setq lsp-ui-flycheck-enable t))

;; DAP (Debug Adapter Protocol) support
(use-package dap-mode
  :after lsp-mode
  :config
  ;; Enable dap-mode's global minor mode. This makes commands available
  ;; and sets up integrations without starting a debug session.
  (dap-mode 1)

  ;; Configure breakpoint visuals for better compatibility.
  (setq dap-breakpoint-indicator "●") ;; A solid circle
  (setq dap-breakpoint-disabled-indicator "○") ;; A hollow circle
  (setq dap-breakpoint-pending-indicator "»") ;; Arrows for pending
  (custom-set-faces
   '(dap-breakpoint-face ((t (:foreground "red" :weight 'bold))))
   '(dap-breakpoint-disabled-face ((t (:foreground "gray")))))

  (dap-auto-configure-mode)
  ;; --- Language-specific setup ---
  ;; NOTE: You must install the actual debug adapters on your system.
  ;; e.g., `pip install debugpy` for Python.
  (require 'dap-python)
  ;; Use the modern `debugpy` adapter instead of the old `ptvsd`
  (setq dap-python-debugger 'debugpy)
  (require 'dap-go)
  (require 'dap-gdb-lldb))

;; UI for DAP.
(use-package dap-ui
  ;; As you correctly pointed out, dap-ui is part of the dap-mode package,
  ;; so we tell `straight` not to install it as a separate package.
  :straight nil
  :after dap-mode
  :config
  ;; Enable the UI globally. It is designed to only show its windows
  ;; when a debug session is active.
  (dap-ui-mode 1))

(use-package rust-mode)

(use-package go-mode)

(use-package zig-mode)

(use-package lua-mode)

(use-package odin-mode
  :straight (:host github :repo "mattt-b/odin-mode"))

(use-package python-mode
  :straight nil
  :hook (python-mode . (lambda ()
                         (setq python-indent-offset 2))))

(use-package markdown-mode)

(use-package tuareg
  :config
  (add-to-list 'load-path (concat (getenv "HOME") "/.opam/default/share/emacs/site-lisp"))
  (require 'ocp-indent))

;; LaTeX configuration
;; (use-package tex
;;   :ensure auctex
;;   :custom
;;   (TeX-auto-save t)                  ; Enable parse on save
;;   (TeX-parse-self t)                 ; Enable parse on load
;;   (TeX-master nil)                   ; Query for master file
;;   (TeX-electric-sub-and-superscript t)  ; Automatically insert braces in math mode
;;   (LaTeX-electric-left-right-brace t)   ; Auto-insert closing braces
;;   (TeX-electric-math '("$" . "$"))      ; Auto-insert closing $
;;   (LaTeX-indent-level 2)                ; Indentation amount
;;   (LaTeX-item-indent 0)                 ; List item indentation
;;   (TeX-source-correlate-mode t)         ; Enable source-preview correlation
;;   (TeX-source-correlate-start-server t) ; Start server for inverse search
;;   (TeX-view-program-selection '((output-pdf "PDF Tools"))) ; Use PDF-tools for viewing
;;   :config
;;   ;; Enable additional LaTeX features
;;   (add-hook 'LaTeX-mode-hook
;;             (lambda ()
;;               (turn-on-auto-fill)       ; Auto-wrap lines
;;               (LaTeX-math-mode)         ; Enable math mode
;;               (reftex-mode)             ; Enable reference management
;;               (flyspell-mode)           ; Enable spell checking
;;               (outline-minor-mode)))    ; Enable outline for folding

;;   ;; Set up latexmk as the default command
;;   (push
;;    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
;;      :help "Run latexmk on file")
;;    TeX-command-list))

;; CDLaTeX for fast math input
(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode)
  :custom
  (cdlatex-math-symbol-alist
   '((?= ("\\equiv" "\\approx" "\\cong" "\\triangleq"))
     (?< ("\\leq" "\\ll" "\\prec" "\\subset"))
     (?> ("\\geq" "\\gg" "\\succ" "\\supset"))
     (?| ("\\mid" "\\parallel" "\\perp" "\\|"))
     (?/ ("\\not"))
     (?~ ("\\tilde" "\\sim" "\\simeq" "\\approx"))))
  :config
  (setq cdlatex-math-modify-alist
        '((?b "\\mathbb" nil t nil nil)
          (?c "\\mathcal" nil t nil nil)
          (?f "\\mathfrak" nil t nil nil)
          (?s "\\mathscr" nil t nil nil))))

;; PDF Tools for viewing PDFs
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t))

;; ;; LaTeX preview pane (optional but useful)
;; (use-package latex-preview-pane
;;   :hook (LaTeX-mode . latex-preview-pane-mode)
;;   :custom
;;   (latex-preview-pane-multifile-mode 'auctex))

(use-package magit)

;; Share clipboard with system in nw mode
(use-package pbcopy
  :defer nil
  :config
  (turn-on-pbcopy))

(use-package doom-themes
  :defer nil
  :config
  (load-theme 'doom-tokyo-night t))

;; Flymake configuration for static analysis
;; Flymake is built into Emacs and is used by lsp-mode as a backend.
(use-package flymake
  :straight nil  ; built-in package
  :hook (prog-mode . flymake-mode)
  :config
  ;; Show diagnostics in echo area when point is on the problematic line
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  ;; Customize faces for better visibility in terminal
  (custom-set-faces
   '(flymake-error   ((t (:underline (:style wave :color "red")))))
   '(flymake-warning ((t (:underline (:style wave :color "yellow")))))
   '(flymake-note    ((t (:underline (:style wave :color "green"))))))

  ;; Optional key bindings for navigation
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; diff-hl for git gutter functionality
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (conf-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; Update diff-hl after git operations
  (add-hook 'after-save-hook 'diff-hl-update)

  ;; Configure diff-hl for terminal use
  (diff-hl-margin-mode 1)

  ;; Customize the margin symbols for better visibility in terminal
  (setq diff-hl-margin-symbols-alist
        '((insert . "+")
          (delete . "-")
          (change . "~")
          (unknown . "?")
          (ignored . " ")))

  ;; Remove background colors and keep only the signs colored.
  ;; We achieve this by *only* setting the foreground color.
  (custom-set-faces
   '(diff-hl-insert ((t (:foreground "green"))))
   '(diff-hl-delete ((t (:foreground "red"))))
   '(diff-hl-change ((t (:foreground "yellow"))))

   ;; Also set margin faces to match
   '(diff-hl-margin-insert ((t (:foreground "green"))))
   '(diff-hl-margin-delete ((t (:foreground "red"))))
   '(diff-hl-margin-change ((t (:foreground "yellow"))))))

;; vterm for terminal emulation
(use-package vterm)

;; autoformatter
(use-package format-all
  ;; https://ianyepan.github.io/posts/format-all/
  :commands format-all-mode
  :hook
  (prog-mode . format-all-mode))

;; Copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist"))
  :hook (prog-mode . copilot-mode)

  :config
  ;; 1) Grab whatever was originally on C-l in Evil insert state:
  (let ((old-c-l (lookup-key evil-insert-state-map (kbd "C-l"))))

    ;; 2) Define a helper that calls `copilot-accept-completion` if a suggestion
    ;; is visible. Otherwise, it calls whatever was originally bound to C-l (or
    ;; defaults to `recenter-top-bottom` if nothing was bound).
    (defun oblsk/copilot-accept-or-fallback ()
      (interactive)
      (if (copilot--overlay-visible)
          (copilot-accept-completion)
        (if old-c-l
            (call-interactively old-c-l)
          ;; If there was no old binding, use the default Emacs action:
          (call-interactively #'recenter-top-bottom))))
    ;; 3) Finally, override C-l in Evil insert state with our new function:
    (define-key evil-insert-state-map (kbd "C-l") #'oblsk/copilot-accept-or-fallback))

  ;; https://github.com/copilot-emacs/copilot.el/issues/312#issuecomment-2149054737
  (add-to-list 'copilot-indentation-alist '(prog-mode tab-width))
  (add-to-list 'copilot-indentation-alist '(org-mode tab-width))
  (add-to-list 'copilot-indentation-alist '(text-mode tab-width))
  (add-to-list 'copilot-indentation-alist '(closure-mode tab-width))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode tab-width)))

;; Claude Code
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c ç" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

;; Print startup time
;; (add-hook 'emacs-startup-hook #'oblsk/display-startup-time)
