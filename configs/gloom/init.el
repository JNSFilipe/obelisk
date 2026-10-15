;;; init.el --- Plain Elisp, Meow and Sheprd -*- lexical-binding: t; -*-
(when (< emacs-major-version 30)
  (error "This configuration requires Emacs 30 or newer"))
(load (expand-file-name "packages.el" (file-name-directory load-file-name)) nil t)

(setq user-full-name "JNSFilipe"
      user-mail-address "jose.filipe@ieee.org"
      inhibit-startup-screen t
      initial-scratch-message nil
      use-short-answers t
      read-process-output-max (* 1024 1024)
      gc-cons-threshold (* 100 1024 1024)
      load-prefer-newer t
      sentence-end-double-space nil
      vc-follow-symlinks t
      ring-bell-function #'ignore
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auth-sources '("~/.authinfo.gpg" "~/.authinfo")
      auth-source-save-behavior t
      org-directory "~/org/"
      savehist-file (expand-file-name "history" gloom-state-directory)
      save-place-file (expand-file-name "places" gloom-state-directory)
      recentf-save-file (expand-file-name "recentf" gloom-state-directory)
      project-list-file (expand-file-name "projects" gloom-state-directory)
      bookmark-default-file (expand-file-name "bookmarks" gloom-state-directory))
(let ((backups (expand-file-name "backups/" gloom-state-directory))
      (autosaves (expand-file-name "autosaves/" gloom-state-directory)))
  (make-directory backups t)
  (make-directory autosaves t)
  (setq backup-directory-alist `(("." . ,backups))
        auto-save-file-name-transforms `((".*" ,autosaves t))
        auto-save-list-file-prefix (expand-file-name ".saves-" autosaves)))
(setq-default indent-tabs-mode nil
              tab-width 4
              display-line-numbers-type 'relative)
(prefer-coding-system 'utf-8)
(savehist-mode 1)
(save-place-mode 1)
(require 'recentf)
;; Opening the recent-files menu should not contact every remembered host.
(add-to-list 'recentf-exclude #'file-remote-p)
(setq recentf-auto-cleanup 'never recentf-max-saved-items 200)
(recentf-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-remote-files nil)
(show-paren-mode 1)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode 1)
(column-number-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-to-list 'default-frame-alist '(font . "Iosevka-14"))

(gloom-require 'doom-themes)
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/" gloom-doom-directory))
(load-theme 'doom-oxocarbon t)
(doom-themes-org-config)
(require 'which-key)
(setq which-key-idle-delay 0.5)
(which-key-mode 1)

;; One completion stack for commands, files, projects and language servers.
(dolist (feature '(vertico orderless marginalia consult embark embark-consult))
  (gloom-require feature))
(setq vertico-count 17 vertico-cycle t vertico-resize t
      completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion)))
      completion-in-region-function #'consult-completion-in-region
      xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)
(vertico-mode 1)
(marginalia-mode 1)
(keymap-set vertico-map "TAB" #'vertico-next)
(keymap-set vertico-map "<backtab>" #'vertico-previous)
(require 'vertico-directory)
(require 'vertico-repeat)
(keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
(keymap-set vertico-map "C-h" #'vertico-directory-up)
(keymap-set vertico-map "C-j" #'vertico-next)
(keymap-set vertico-map "C-k" #'vertico-previous)
(keymap-set vertico-map "M-RET" #'vertico-exit-input)
(keymap-set vertico-map "M-d" #'consult-dir)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
(keymap-set minibuffer-local-map "C-c C-a" #'embark-act)
(keymap-set minibuffer-local-map "C-c C-e" #'embark-export)
(keymap-set minibuffer-local-map "M-." #'embark-dwim)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(gloom-require 'projectile)
(setq projectile-completion-system 'default
      projectile-project-search-path '("~/Documents/GitHub/" "~/Documents/Probe/")
      projectile-cache-file (expand-file-name "projectile.cache" gloom-state-directory)
      projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" gloom-state-directory)
      projectile-switch-project-action #'gloom-project-open)
(projectile-mode 1)
(defun gloom-project-open ()
  "Open the chosen project in its own session."
  (let* ((root (projectile-project-root))
         ;; Full root avoids collisions between projects with the same basename.
         (name (abbreviate-file-name (directory-file-name root))))
    (+workspace-switch name t)
    (setq default-directory root)
    (projectile-find-file)))

(load (expand-file-name "workspaces.el" gloom-config-directory) nil t)
(load (expand-file-name "editing.el" gloom-config-directory) nil t)
(load (expand-file-name "coding.el" gloom-config-directory) nil t)
(load (expand-file-name "integrations.el" gloom-config-directory) nil t)

;; Use Dired's own navigation, marks, writable mode, remote files and commands.
(require 'dired)
(require 'dired-x)
(setq dired-dwim-target t
      dired-listing-switches "-alh"
      dired-kill-when-opening-new-dired-buffer t
      ls-lisp-use-insert-directory-program nil)
(keymap-set dired-mode-map "h" #'dired-up-directory)
(keymap-set dired-mode-map "l" #'dired-find-file)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(autoload 'diredfl-mode "diredfl" nil t)
(add-hook 'dired-mode-hook #'diredfl-mode)

(require 'tramp)
(setq tramp-default-method "ssh"
      tramp-persistency-file-name (expand-file-name "tramp" gloom-state-directory)
      tramp-auto-save-directory (expand-file-name "autosaves/" gloom-state-directory))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; Built-in backends remain available, including containers on newer Emacs.
(require 'tramp-container nil t)

;; Keep Ghostel's shell keys in input state. Normal state uses its read-only
;; scrollback through the Meow bridge in editing.el.
(with-eval-after-load 'ghostel
  (keymap-set ghostel-semi-char-mode-map "C-s" #'consult-line)
  (keymap-set ghostel-semi-char-mode-map "C-k" #'gloom-ghostel-kill-line)
  (keymap-set ghostel-semi-char-mode-map "M-p"
              (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
  (keymap-set ghostel-semi-char-mode-map "M-n"
              (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
  (keymap-set project-prefix-map "m" #'ghostel-project)
  (keymap-set project-prefix-map "M" #'ghostel-project-list-buffers)
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))
(defun gloom-ghostel-kill-line ()
  (interactive)
  (kill-ring-save (point) (line-end-position))
  (ghostel-send-key "k" "ctrl"))

(setq consult-gh-default-clone-directory "~/Documents/GitHub/")
(keymap-set global-map "C-x m" #'ghostel)
(with-eval-after-load 'org
  (setq org-src-fontify-natively t org-src-tab-acts-natively t
        org-log-done 'time org-return-follows-link t))
(when (file-exists-p custom-file) (load custom-file nil t))
(provide 'gloom-init)
;;; init.el ends here
