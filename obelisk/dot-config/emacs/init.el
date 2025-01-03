;;; init.el -*- lexical-binding: t; -*-

;; Early init performance optimizations
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Initialize package system
(setq package-enable-at-startup nil)

;; Bootstrap Elpaca
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
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
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

;; #############################################################################
;; Helper functions
;; #############################################################################
(load-file (expand-file-name "oblsk.el" user-emacs-directory))

;; Basic UI settings
(use-package emacs
  :ensure nil
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
  ;; Restore gc-cons-threshold after startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 800000)))
  ;; Font settings
  (set-frame-font "Source Code Pro-12" nil t))
;; Files and backup settings
(use-package files
  :ensure nil
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

;; Built-in completion configuration
(use-package minibuffer
  :ensure nil
  :custom
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  :config
  (setq completion-styles '(basic partial-completion substring initials flex)))

;; Language-specific settings
(use-package prog-mode
  :ensure nil
  :hook
  ((c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (python-mode . eglot-ensure)
   (emacs-lisp-mode . (lambda ()
                        (setq indent-tabs-mode nil
                              tab-width 2)))))

;; Custom file settings
(use-package cus-edit
  :ensure nil
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (when (file-exists-p custom-file)
    (load custom-file)))

;; ;; Basic settings
;; (setq inhibit-startup-message t)
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (global-display-line-numbers-mode 1)
;; (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; (global-auto-revert-mode 1)
;; (winner-mode 1)
;; (show-paren-mode 1)
;; (global-hl-line-mode 1)
;; (setq-default history-length 1000)
;; (savehist-mode t)
;;
;; ;; Backup settings
;; ;; Create backup directory if it doesn't exist
;; (let ((backup-dir (expand-file-name "backups" user-emacs-directory)))
;;   (unless (file-exists-p backup-dir)
;;     (make-directory backup-dir t)))
;;
;; ;; Configure backup settings
;; (setq
;;  ;; Use versioned backups
;;  version-control t
;;
;;  ;; Keep all versions
;;  kept-new-versions 10
;;  kept-old-versions 5
;;
;;  ;; Delete old versions without asking
;;  delete-old-versions t
;;
;;  ;; Create backup files by copying
;;  backup-by-copying t
;;
;;  ;; Set backup directory
;;  backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
;;
;;  ;; Also save auto-save files in the backup directory
;;  auto-save-file-name-transforms `((".*" ,(expand-file-name "backups/" user-emacs-directory) t))
;;
;;  ;; Don't create lockfiles
;;  create-lockfiles nil)
;;
;; ;; Optional: Enable automatic cleanup of old backups
;; (setq delete-by-moving-to-trash t)  ; Move to trash instead of immediate deletion
;;
;; ;; Built-in completion configurations
;; (setq completion-cycle-threshold 3)            ; Show candidates when 3 or more are available
;; (setq tab-always-indent 'complete)             ; Make TAB do completion after indenting
;; (setq completion-styles '(basic partial-completion substring initials flex))
;; (setq completion-category-defaults nil)        ; Don't use predefined style defaults
;; (setq completion-category-overrides nil)       ; Don't override styles for different categories

;; Package configurations
(use-package ido
  :ensure nil
  :config
  (ido-mode 1)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching t)
  (ido-everywhere 1))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package amx
  :after ido
  :config
  (amx-mode 1))

;;; Meow configuration with Vim-like keybindings
(use-package meow
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :hook
  (ido-setup . (lambda ()
                 (define-key ido-completion-map (kbd "C-l") 'ido-next-match)
                 (define-key ido-completion-map (kbd "C-h") 'ido-prev-match)))
  :config
  ;; Custom functions to emulate Vim behavior
  (setq meow-use-clipboard t) ;;
  (setq meow-leader-key "SPC")
  (oblsk/meow-setup)
  ;; Initialize modal state
  (meow-global-mode 1))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :config
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3)
  (setq completion-styles '(basic partial-completion substring)))

(use-package rust-mode
  :hook (rust-mode . eglot-ensure))

(use-package go-mode
  :hook (go-mode . eglot-ensure))

(use-package zig-mode
  :hook (zig-mode . eglot-ensure))

(use-package lua-mode
  :hook (lua-mode . eglot-ensure))

(use-package markdown-mode)

;; LaTeX configuration
(use-package tex
  :ensure auctex
  :custom
  (TeX-auto-save t)                   ; Enable parse on save
  (TeX-parse-self t)                  ; Enable parse on load
  (TeX-master nil)                    ; Query for master file
  (TeX-electric-sub-and-superscript t)  ; Automatically insert braces in math mode
  (LaTeX-electric-left-right-brace t)   ; Auto-insert closing braces
  (TeX-electric-math '("$" . "$"))      ; Auto-insert closing $
  (LaTeX-indent-level 2)                ; Indentation amount
  (LaTeX-item-indent 0)                 ; List item indentation
  (TeX-source-correlate-mode t)         ; Enable source-preview correlation
  (TeX-source-correlate-start-server t) ; Start server for inverse search
  (TeX-view-program-selection '((output-pdf "PDF Tools"))) ; Use PDF-tools for viewing
  :config
  ;; Enable additional LaTeX features
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (turn-on-auto-fill)        ; Auto-wrap lines
              (LaTeX-math-mode)          ; Enable math mode
              (reftex-mode)              ; Enable reference management
              (flyspell-mode)            ; Enable spell checking
              (outline-minor-mode)))     ; Enable outline for folding

  ;; Set up latexmk as the default command
  (push
   '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
     :help "Run latexmk on file")
   TeX-command-list))

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

;; LaTeX preview pane (optional but useful)
(use-package latex-preview-pane
  :hook (LaTeX-mode . latex-preview-pane-mode)
  :custom
  (latex-preview-pane-multifile-mode 'auctex))

(use-package magit)

(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))

;; Flymake configuration for static analysis
;; Flymake is built into Emacs and works well with eglot
(use-package flymake
  :ensure nil  ; built-in package
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

  ;; Remove background colors and keep only the signs colored
  (custom-set-faces
   '(diff-hl-insert ((t (:foreground "green" :background nil :inherit nil))))
   '(diff-hl-delete ((t (:foreground "red" :background nil :inherit nil))))
   '(diff-hl-change ((t (:foreground "yellow" :background nil :inherit nil))))

   ;; Also set margin faces to match
   '(diff-hl-margin-insert ((t (:foreground "green" :inherit nil))))
   '(diff-hl-margin-delete ((t (:foreground "red" :inherit nil))))
   '(diff-hl-margin-change ((t (:foreground "yellow" :inherit nil))))))

;; ;; Language specific settings
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'python-mode-hook 'eglot-ensure)
;;
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode nil)
;;             (setq tab-width 2)))
;;
;; ;; Font settings
;; (set-frame-font "Source Code Pro-12" nil t)
;;
;; ;; Save customizations to separate file
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;; ;; Restore gc-cons-threshold
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold 800000)))
