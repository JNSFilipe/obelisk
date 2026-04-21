;;; doom-oxocarbon-theme.el --- Oxocarbon for doom-themes -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: JNSFilipe
;; Source: https://github.com/nyoom-engineering/oxocarbon-alacritty/blob/main/oxocarbon-dark.toml
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

(defgroup doom-oxocarbon-theme nil
  "Options for the `doom-oxocarbon' theme."
  :group 'doom-themes)

(defcustom doom-oxocarbon-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds padding to the mode-line."
  :group 'doom-oxocarbon-theme
  :type '(choice integer boolean))

(def-doom-theme doom-oxocarbon
  "A Doom port of the Oxocarbon dark palette."

  ;; name        default   256       16
  ((bg         '("#161616" "black"   "black"))
   (bg-alt     '("#161616" "black"   "black"))
   (base0      '("#0f0f0f" "black"   "black"))
   (base1      '("#161616" "#1c1c1c" "black"))
   (base2      '("#262626" "#262626" "brightblack"))
   (base3      '("#393939" "#3a3a3a" "brightblack"))
   (base4      '("#525252" "#4e4e4e" "brightblack"))
   (base5      '("#6f6f6f" "#6c6c6c" "brightblack"))
   (base6      '("#9ea4ac" "#9e9e9e" "brightblack"))
   (base7      '("#dde1e6" "#dadada" "white"))
   (base8      '("#ffffff" "#ffffff" "brightwhite"))
   (fg         '("#ffffff" "#ffffff" "brightwhite"))
   (fg-alt     '("#dde1e6" "#dadada" "white"))

   (grey       base4)
   (red        '("#ee5396" "#d75f87" "red"))
   (orange     '("#ffe97b" "#ffff87" "yellow"))
   (green      '("#42be65" "#5faf5f" "green"))
   (teal       '("#3ddbd9" "#5fd7d7" "brightcyan"))
   (yellow     '("#ffe97b" "#ffff87" "yellow"))
   (blue       '("#33b1ff" "#5fafff" "brightblue"))
   (dark-blue  '("#33b1ff" "#5fafff" "blue"))
   (magenta    '("#ff7eb6" "#ff87af" "magenta"))
   (violet     '("#ee5396" "#d75f87" "brightmagenta"))
   (cyan       '("#3ddbd9" "#5fd7d7" "brightcyan"))
   (dark-cyan  '("#3ddbd9" "#5fd7d7" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base2)
   (selection      base3)
   (builtin        cyan)
   (comments       base5)
   (doc-comments   base6)
   (constants      magenta)
   (functions      blue)
   (keywords       red)
   (methods        cyan)
   (operators      fg-alt)
   (type           yellow)
   (strings        green)
   (variables      fg-alt)
   (numbers        magenta)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (modeline-fg base7)
   (modeline-fg-alt comments)
   (modeline-bg base2)
   (modeline-bg-inactive base1)
   (level1 blue)
   (level2 magenta)
   (level3 yellow)
   (level4 cyan)
   (level5 green)
   (level6 red)
   (level7 fg-alt)
   (level8 base7)
   (-modeline-pad
    (when doom-oxocarbon-padded-modeline
      (if (integerp doom-oxocarbon-padded-modeline)
          doom-oxocarbon-padded-modeline
        4))))

  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (hl-line :background base1)
   (lazy-highlight :background base3 :foreground fg)
   (link :foreground blue :underline t)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad
             `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground fg)
   (vertical-border :foreground vertical-bar)
   (show-paren-match :background blue :foreground bg :weight 'bold)
   (tooltip :background base2 :foreground fg)

   ;; corfu
   (corfu-current :background base3 :foreground fg)
   (corfu-border :background base2)
   (corfu-bar :background blue)

   ;; consult
   (consult-highlight-match :foreground yellow :weight 'bold)

   ;; doom-modeline
   (doom-modeline-bar :background highlight)

   ;; magit
   (magit-diff-added :background (doom-darken green 0.55) :foreground green)
   (magit-diff-removed :background (doom-darken red 0.6) :foreground red)
   (magit-diff-context-highlight :background base1 :foreground base6)

   ;; org
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :background bg-alt :foreground comments)
   ((org-block-end-line &override) :background bg-alt :foreground comments)
   ((org-code &override) :foreground yellow)
   ((org-verbatim &override) :foreground cyan)
   ((org-level-1 &override) :foreground level1 :weight 'bold)
   ((org-level-2 &override) :foreground level2 :weight 'bold)
   ((org-level-3 &override) :foreground level3 :weight 'bold)
   ((org-level-4 &override) :foreground level4 :weight 'bold)
   ((org-level-5 &override) :foreground level5)
   ((org-level-6 &override) :foreground level6)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground blue)
   (rainbow-delimiters-depth-2-face :foreground magenta)
   (rainbow-delimiters-depth-3-face :foreground yellow)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground green)
   (rainbow-delimiters-depth-6-face :foreground red)
   (rainbow-delimiters-depth-7-face :foreground fg-alt)
   (rainbow-delimiters-unmatched-face :foreground red :weight 'bold)

   ;; vertico
   (vertico-current :background base3 :foreground fg)

   ;; whitespace
   (whitespace-tab :foreground base3)
   (whitespace-trailing :background red :foreground bg)))

(provide 'doom-oxocarbon-theme)

;;; doom-oxocarbon-theme.el ends here
