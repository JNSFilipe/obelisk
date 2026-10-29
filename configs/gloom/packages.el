;;; packages.el --- Reuse libraries without loading Doom -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'package)

(defconst gloom-config-directory
  (file-name-directory (file-truename (or load-file-name buffer-file-name))))
(defconst gloom-doom-directory
  (expand-file-name "../doom/" gloom-config-directory))
(defconst gloom-state-directory
  (file-name-as-directory
   (expand-file-name (or (getenv "GLOOM_EMACS_STATE")
                         "~/.local/share/gloom-emacs/"))))

;; --init-directory selects our source; mutable state lives elsewhere.
(setq user-emacs-directory gloom-state-directory
      package-user-dir (expand-file-name "elpa/" gloom-state-directory)
      custom-file (expand-file-name "custom.el" gloom-state-directory)
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(make-directory gloom-state-directory t)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" gloom-state-directory)))

(defconst gloom-packages
  '(evil evil-collection evil-surround evil-nerd-commenter evil-ghostel
    persp-mode ghostel doom-themes vertico orderless marginalia
    consult embark embark-consult consult-eglot projectile magit diff-hl
    yasnippet doom-snippets vundo avy helpful hl-todo makefile-executor
    consult-gh diredfl rust-mode go-mode typescript-mode web-mode lua-mode
    nix-mode yaml-mode json-mode zig-mode terraform-mode dockerfile-mode
    csv-mode cmake-mode markdown-mode pdf-tools
    flycheck consult-flycheck dumb-jump eros quickrun macrostep elisp-refs
    python-pytest pyvenv pipenv pip-requirements nose rustic gorepl-mode go-tag
    go-gen-test nodejs-repl emmet-mode docker consult-dir consult-yasnippet
    auto-yasnippet wgrep saveplace-pdf-view org-pdftools toc-org org-cliplink
    ox-gfm ob-go ob-async org-noter ligature kkp xclip ns-auto-titlebar
    git-link git-timemachine haml-mode pug-mode slim-mode sass-mode stylus-mode
    sws-mode rainbow-mode htmlize ox-clip org-mac-link
    ;; Sheprd runs Pi agents in Pilish.
    pilish))

(defvar gloom-doom-local-directory
  (expand-file-name (or (getenv "DOOMLOCALDIR")
                        "~/.local/share/doom-emacs/.local/"))
  "Existing Doom package store, used as a read-only library source.")

(let ((build-root
       (expand-file-name (format "straight/build-%d.%d/"
                                 emacs-major-version emacs-minor-version)
                         gloom-doom-local-directory)))
  (when (file-directory-p build-root)
    (dolist (directory (directory-files build-root t "^[^.]"))
      ;; Prefer Emacs's own core libraries, not Doom's pinned replacements.
      (unless (member (file-name-nondirectory directory)
                      '("org" "eglot" "eldoc" "flymake" "project" "seq" "xref"))
        (when (file-directory-p directory)
          (add-to-list 'load-path directory t)
          (let ((extensions (expand-file-name "extensions/" directory)))
            (when (file-directory-p extensions)
              (add-to-list 'load-path extensions t))))))))

;; Own ELPA packages take precedence over reused builds.
(package-initialize)
(dolist (package gloom-packages)
  (let ((autoload-file (locate-library (format "%s-autoloads" package))))
    (when autoload-file (load autoload-file nil t))))

(defun gloom-require (feature)
  "Load required FEATURE, with an actionable error for missing libraries."
  (unless (require feature nil t)
    (error "Missing %s; run the configs/gloom/install.el setup described in README.md"
           feature)))

(provide 'gloom-packages)
