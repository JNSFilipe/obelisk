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
  ;; ESC in normal state does exactly what C-g does.  `keyboard-escape-quit',
  ;; which used to be here, falls through to `delete-other-windows' whenever
  ;; more than one window is live, so a stray ESC collapsed the whole layout.
  (map! :n "<escape>" #'doom/escape
        :n "j" "gj" ;; Enable easier navigation in wrapped lines
        :n "k" "gk" ;; Enable easier navigation in wrapped lines
        ;; :n "C-h C-h" #'evil-window-left ;; TODO: This is not working in C files
        :n "C-h" #'evil-window-left ;; TODO: This is not working in C files
        :n "C-l" #'evil-window-right
        :n "C-j" #'evil-window-down
        :n "C-k" #'evil-window-up
        ;; `C-q' kills the current buffer in every state.  The global binding
        ;; below covers the states that leave it alone; insert and replace bind
        ;; it to `evil-quoted-insert' themselves, so they need it spelled out.
        ;; `C-v' still inserts a literal character there.
        :i "C-q" #'kill-this-buffer
        :r "C-q" #'kill-this-buffer
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
        "C-q" #'kill-this-buffer ;; Kill current buffer; delete windows with C-w c
        [escape] #'doom/escape   ;; Bind ESC and C-g together
        ;; No binding for `C-[' here.  Emacs reads it as the ESC byte, so binding
        ;; it in `global-map' replaces the ESC prefix map and breaks every Meta
        ;; key a terminal frame sends as ESC+key.  `evil-esc-mode', enabled by
        ;; the `evil-esc-delay' below, is what makes a lone ESC an escape there.
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
    :desc "Scratch Buffer" ";" #'doom/open-scratch-buffer
    :desc "New Terminal" "t" #'sheprd-new-terminal
    :desc "Split Terminal Right" "," #'sheprd-split-right
    :desc "Split Terminal Down" "<" #'sheprd-split-down
    :desc "Search Buffer" "v" #'+default/search-buffer
    :desc "Search Project" "g" #'+default/search-project
    :desc "Git" "G" #'magit
    :desc "Save Buffer" "w" #'save-buffer
    ;; :desc "Code Actions" "a" #'lsp-execute-code-action
    :desc "Dired" "o" #'dired-at-point
    :desc "Files" "f" #'projectile-find-file
    :desc "Buffers" "b" #'persp-switch-to-buffer
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
    :desc "Switch Project" "p" #'projectile-switch-project)

  ;; Expose the same commands outside the leader menu, with which-key labels.
  (map! :map global-map
    :desc "New Terminal" "C-c t" #'sheprd-new-terminal
    :desc "Find Project File" "C-c f" #'projectile-find-file
    :desc "Switch Workspace Buffer" "C-c b" #'persp-switch-to-buffer))

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

;; Separate dape keybindings with proper prefix using 'd' for debug.
;; Commented out until dape is installed: uncomment `(package! dape)' in
;; packages.el and run `make doom-sync' before restoring these.
;; (map! :leader
;;       (:prefix ("d" . "debug")
;;        :desc "Toggle Breakpoint" "b" #'dape-breakpoint-toggle
;;        :desc "Start Debug" "d" #'dape
;;        :desc "Debug Continue" "c" #'dape-continue
;;        :desc "Debug Step Over" "n" #'dape-next
;;        :desc "Debug Step Into" "i" #'dape-step-in
;;        :desc "Debug Step Out" "o" #'dape-step-out
;;        :desc "Debug Restart" "r" #'dape-restart
;;        :desc "Debug Quit" "q" #'dape-quit
;;        :desc "Debug Evaluate" "e" #'dape-evaluate-expression))


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

;; Switch buffers within the current perspective only.  `switch-to-buffer' and
;; `consult-buffer' both see every buffer in the instance, which reads straight
;; through Sheprd's session isolation.  Doom remaps `persp-switch-to-buffer' to
;; `+vertico/switch-workspace-buffer', so that is what this key actually runs:
;; same scoping, with other workspaces reachable only by narrowing to them.
(map! :map global-map "C-x b" #'persp-switch-to-buffer)

;; Sheprd: Herdr rebuilt on persp-mode and Ghostel.  Sessions, panes and coding
;; agents, all hermetically sealed per session.  Sheprd installs its own
;; keymap on `C-c s' and on the leader's TAB prefix (both TAB and <tab>), so no
;; workspace or terminal bindings are declared here.
(load! "sheprd")
