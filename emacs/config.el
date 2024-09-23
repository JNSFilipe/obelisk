;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(load-file "~/.config/doom/defs.el")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "JNSFilipe"
      user-mail-address "jose.filipe@ieee.org")

;; Setting it to 100mb seems to strike a nice balance between GC pauses and performance.
(setq gc-cons-threshold (* 100 1024 1024))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'semi-bold)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 14))
;; (setq doom-font (font-spec :family "Cascadia Code" :size 14 :weight 'semi-bold))
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'semi-bold))

;; Set terminal name for TRAMP agent (useful to disable fancy features when using TRAMP, cause this is going to be the value of $TERM)
(setq tramp-terminal-type "tramp")

;; Set default tab size as two spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)
;; (setq doom-theme 'doom-ayu-dark)
;; (load-theme 'modus-vivendi :no-confirm)

(after! doom-modeline
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

;; Simpler dashboard
(use-package dashboard
  :config
  (setq dashboard-projects-backend 'projectile
        dashboard-banner-logo-title nil
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-set-footer nil
        dashboard-page-separator "\n\n\n"
        dashboard-items '((projects . 10)
                          (recents  . 10)))
  (dashboard-setup-startup-hook))

;; Simpler git gutter
(use-package git-gutter
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :config
  (custom-set-variables
   '(git-gutter:modified-sign "~")
   '(git-gutter:added-sign "+")
   '(git-gutter:deleted-sign "-"))
  (setq git-gutter:update-interval 0.2))

;; Config Copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config (setq copilot-idle-delay 0.4))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq global-display-line-numbers-mode 'relative)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Set Projectile path
(setq projectile-project-search-path '("~/Documents/GitHub/" "~/.local/share/"))

;; Load Drag-stuff with default keybinds (Alt+arrows)
(use-package! drag-stuff
  :config (drag-stuff-define-keys))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc)
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Keybinds!
(load-file "~/.config/doom/keybinds.el")

;; (defun alfred ()
;;   "Launch an application using fuzzy search."
;;   (interactive)
;;   (raise-frame '((minibuffer . only)))
;;   (let ((app-list (split-string (shell-command-to-string "compgen -c") "\n" t)))
;;     (ivy-read "Launch application: " app-list
;;               :action (lambda (app)
;;                         (shell-command (format "%s >/dev/null 2>&1 & disown" app) nil nil)
;;                         (kill-emacs))
;;               :caller 'alfred)))

;; Check emacs mode and act accordingle
(message "---------------> Emacs mode: ")
(setq em-mode (getenv "EMACS_MODE"))
(cond
 ((eq 'em-mode nil)
  (message "---------------> Normal"))
 ((string= em-mode "terminal")
  (message "---------------> Terminal")
  (set-frame-name "Terminal")
  (setq confirm-kill-emacs nil))
 ((string= em-mode "dired")
  (message "---------------> Dired")
  (set-frame-name "Dired")
  (setq confirm-kill-emacs nil))
 ((string= em-mode "quake")
  (message "---------------> Quake")
  (set-frame-name "Quake"))
 ((string= em-mode "launcher")
  ;; (alfred)
  (message "---------------> Launcher")))
