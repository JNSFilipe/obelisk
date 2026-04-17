;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "JNSFilipe"
      user-mail-address "jose.filipe@ieee.org")

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
(setq evil-disable-insert-state-bindings t)

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
  (map! :leader
    :desc "M-x" "SPC" #'execute-extended-command
    :desc "Eval" ";" #'eval-expression
    :desc "IBuffer" "." #'ibuffer
    :desc "Scratch Buffer" "," #'doom/open-scratch-buffer
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
(use-package! copilot
  ;; :hook (prog-mode . copilot-mode) # I decided to not enable it globally, since it is slow to start, I will enable it manually when I need it
  :bind (:map copilot-completion-map
              ("C-l" . 'copilot-accept-completion)))

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
(remove-hook! (text-mode-hook org-mode-hook markdown-mode-hook) #'flyspell-mode)
(after! flyspell
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(map! :leader
  (:prefix ("-" . "toggle")
   :desc "Flyspell" "s" #'flyspell-mode))
