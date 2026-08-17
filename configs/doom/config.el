;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here. Nix bundles this directory, so run
;; `make switch` from the dotfiles repository after modifying this file.


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "JNSFilipe"
      user-mail-address "jose.filipe@ieee.org")

;; ── TRAMP (remote editing) ───────────────────────────────────────────────────
(after! tramp
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Persist TRAMP passwords across sessions via auth-source
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")
      auth-source-save-behavior t)

;; Setting it to 100mb seems to strike a nice balance between GC pauses and performance.
(setq gc-cons-threshold (* 100 1024 1024))

;; Increase the amount of data Emacs reads from a process (default is 4k)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq doom-font (font-spec :family "Iosevka" :size 14))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. Themes in $DOOMDIR/themes are picked up automatically by Doom.
(setq doom-theme 'doom-oxocarbon)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Doom reads this while building Evil's insert-state bindings.
(setq evil-disable-insert-state-bindings nil)

;; Set Projectile project roots
(after! projectile
  (setq projectile-project-search-path
        '("~/Documents/GitHub/"
          "~/Documents/Probe/")))

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
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Make which-key appear faster
(after! which-key
  (setq which-key-idle-delay 0.5))

;; Keep selection after unindent
(after! evil
  ;; Map tab key to indent region when in visual mode
  (map! :n "<escape>" #'keyboard-escape-quit
        :n "j" "gj" ;; Enable easier navigation in wrapped lines
        :n "k" "gk" ;; Enable easier navigation in wrapped lines
        ;; :n "C-h C-h" #'evil-window-left ;; TODO: This is not working in C files
        :n "C-h" #'evil-window-left ;; TODO: This is not working in C files
        :n "C-l" #'evil-window-right
        :n "C-j" #'evil-window-down
        :n "C-k" #'evil-window-up
        :n "C-q" #'evil-window-delete
        ;; Some ideas stolen from Meow
        :n "$" "g_" ;; https://stackoverflow.com/questions/20165596/select-entire-line-in-vim-without-the-new-line-character
        :n "w" "viw"
        :n "W" "viW")

  ;; Keep the selection active after shifting with <> (nice QoL)
  (setq evil-keep-visual-state-on-shift t)

  ;; Jump through git hunks
  (map! :n "ç" #'+vc-gutter/next-hunk
        :n "Ç" #'+vc-gutter/previous-hunk)

  ;; Bind C-g and
  (map! :map global-map
        "C-q" #'kill-this-buffer ;; Kill current buffer
        [escape] #'doom/escape   ;; Bind ESC and C-g together
        "C-["    #'doom/escape   ;; Bind ESC and C-g together (ESC in most terminals)
        "C-g"    #'doom/escape)  ;; Bind ESC and C-g together
  (global-set-key [remap keyboard-quit] #'doom/escape) ;; Add quitting insert mode to doom/escape (C-g by default)
  (setq evil-esc-delay 0.01)) ;; make ESC detection snappier in terminals

;; Leader keybindings
(after! general
  ;; The minimal `(default)' module does not install Doom's +bindings preset,
  ;; so retain the canonical command launcher explicitly in every Evil state.
  (define-key! 'override
    "M-x" #'execute-extended-command
    "A-x" #'execute-extended-command)

  (map! :leader
    :desc "M-x" "SPC" #'execute-extended-command
    :desc "Eval" "X" #'eval-expression
    :desc "IBuffer" "." #'ibuffer
    :desc "Scratch Terminal" "," #'+ghostel/scratch-toggle
    :desc "Scratch Buffer" ";" #'doom/open-scratch-buffer
    :desc "Terminal Here" "t" #'+ghostel/here
    :desc "Search Buffer" "v" #'+default/search-buffer
    :desc "Search Project" "g" #'+default/search-project
    :desc "Git" "G" #'magit
    :desc "Save Buffer" "w" #'save-buffer
    ;; :desc "Code Actions" "a" #'lsp-execute-code-action
    :desc "Dired" "o" #'dired-at-point
    :desc "Files" "f" #'projectile-find-file
    :desc "Buffers" "b" #'consult-buffer
    :desc "Toggle Comment" "c" #'comment-line
    :desc "LSP Diagnostics" "D" #'consult-eglot-symbols
    :desc "Run" "r" #'async-shell-command
    :desc "Make" "m" #'+make/run
    :desc "Compile" "M" #'compile
    :desc "Vertical Split" "s" #'evil-window-vsplit
    :desc "Horizontal Split" "S" #'evil-window-split
    :desc "Vundo" "u" #'vundo
    :desc "Yanks" :n "y" #'consult-yank-pop
    :desc "Yanks" :v "y" #'consult-yank-replace
    :desc "Switch Project" "p" #'projectile-switch-project))

;; Keep the command palette visible and exploratory.  Doom normally defers
;; Vertico and Marginalia until its first-input hook; loading them eagerly makes
;; the very first M-x invocation show candidates and command descriptions.
(use-package! vertico
  :demand t
  :config
  (setq vertico-count 17
        vertico-cycle t
        vertico-resize t)
  (vertico-mode 1)
  (map! :map vertico-map
        "TAB" #'vertico-next
        [tab] #'vertico-next
        "S-TAB" #'vertico-previous
        [backtab] #'vertico-previous))

(use-package! marginalia
  :demand t
  :config
  (marginalia-mode 1))

;; Separate dape keybindings with proper prefix using 'd' for debug
(map! :leader
      (:prefix ("d" . "debug")
       :desc "Toggle Breakpoint" "b" #'dape-breakpoint-toggle
       :desc "Start Debug" "d" #'dape
       :desc "Debug Continue" "c" #'dape-continue
       :desc "Debug Step Over" "n" #'dape-next
       :desc "Debug Step Into" "i" #'dape-step-in
       :desc "Debug Step Out" "o" #'dape-step-out
       :desc "Debug Restart" "r" #'dape-restart
       :desc "Debug Quit" "q" #'dape-quit
       :desc "Debug Evaluate" "e" #'dape-evaluate-expression))


;; Config custom packages
;; accept completion from copilot and fallback to company
(use-package! ghostel
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("C-k"  . my/ghostel-send-C-k-and-kill)
         ;; ;; I'm used to go up/down the shell history with M-n/p from eshell
         ;; ;; Simulate this behavior in ghostel by sending C-p and C-n
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :config
  (defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

;; (use-package! consult-gh
;;   :after consult)
(use-package! consult-gh
  :after consult
  :config
  ;; (require 'consult-gh) is not needed inside :config, use-package handles it!
  
  ;; Set your default clone path
  (setq consult-gh-default-clone-directory "~/Documents/GitHub/"))


(use-package! vundo
  :bind ("C-x u" . vundo))


;; Native smooth scrolling, no package needed
(pixel-scroll-precision-mode 1)

;; Flyspell: disable auto-activation, toggle manually via SPC - s
(after! flyspell
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))
;; Remove flyspell from every hook Doom's spell module adds it to
(remove-hook! (text-mode-hook prog-mode-hook conf-mode-hook) #'flyspell-mode)
(remove-hook! (text-mode-hook prog-mode-hook conf-mode-hook) #'flyspell-prog-mode)

(map! :leader
  (:prefix ("-" . "toggle")
   :desc "Flyspell" "s" #'flyspell-mode))

;; Herdr-like Doom workspaces and Ghostel coding-agent sidebar.
(load! "sheprd")

;; Floating scratch terminal + open-terminal-here helpers.
(load! "scratch-term")

(map! :leader
  (:prefix ("TAB" . "workspace")
   :desc "Display workspace bar" "TAB" #'+workspace/display
   :desc "Switch workspace" "." #'+workspace/switch-to
   :desc "Switch to last workspace" "`" #'+workspace/other
   :desc "Previous workspace" "[" #'+workspace/switch-left
   :desc "Next workspace" "]" #'+workspace/switch-right
   :desc "New workspace" "n" #'+workspace/new
   :desc "New named workspace" "N" #'+workspace/new-named
   :desc "Load workspace" "l" #'+workspace/load
   :desc "Save workspace" "s" #'+workspace/save
   :desc "Kill workspace" "d" #'+workspace/kill
   :desc "Delete saved workspace" "D" #'+workspace/delete
   :desc "Rename workspace" "r" #'+workspace/rename
   :desc "Restore last session" "R" #'+workspace/restore-last-session
   :desc "Kill workspace session" "x" #'+workspace/kill-session
   :desc "Switch to workspace 1" "1" #'+workspace/switch-to-0
   :desc "Switch to workspace 2" "2" #'+workspace/switch-to-1
   :desc "Switch to workspace 3" "3" #'+workspace/switch-to-2
   :desc "Switch to workspace 4" "4" #'+workspace/switch-to-3
   :desc "Switch to workspace 5" "5" #'+workspace/switch-to-4
   :desc "Switch to workspace 6" "6" #'+workspace/switch-to-5
   :desc "Switch to workspace 7" "7" #'+workspace/switch-to-6
   :desc "Switch to workspace 8" "8" #'+workspace/switch-to-7
   :desc "Switch to workspace 9" "9" #'+workspace/switch-to-8
   :desc "Switch to final workspace" "0" #'+workspace/switch-to-final
   :desc "Toggle Sheprd" "h" #'sheprd-toggle
   :desc "Focus Sheprd spaces" "w" #'sheprd-focus-spaces
   :desc "Focus Sheprd agents" "a" #'sheprd-focus-agents))

;; GUI Emacs reports the physical Tab key as <tab>, while Doom's workspace
;; prefix uses TAB (the terminal/C-i event).  Point both events at the same map.
(define-key doom-leader-map (kbd "<tab>")
  (lookup-key doom-leader-map (kbd "TAB")))
