;;; integrations.el --- Reuse enabled Doom package facilities -*- lexical-binding: t; -*-

;; Eglot supplies Flymake; Flycheck covers buffers without a language server.
(gloom-require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit flycheck-idle-change-delay 1.0)
(setq flycheck-check-syntax-automatically
      (delq 'new-line flycheck-check-syntax-automatically))
(defun gloom-syntax-setup ()
  (flycheck-mode (if (or (eglot-managed-p)
                         (file-remote-p default-directory)) -1 1)))
(add-hook 'prog-mode-hook #'gloom-syntax-setup)
(add-hook 'eglot-managed-mode-hook #'gloom-syntax-setup)
(defun gloom-diagnostics ()
  (interactive)
  (call-interactively (if (bound-and-true-p flymake-mode)
                          #'flymake-show-buffer-diagnostics
                        #'flycheck-list-errors)))
(autoload 'dumb-jump-xref-activate "dumb-jump")
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)
(with-eval-after-load 'dumb-jump (setq dumb-jump-prefer-searcher 'rg))
(keymap-set global-map "C-h f" #'helpful-callable)
(keymap-set global-map "C-h v" #'helpful-variable)
(keymap-set global-map "C-h k" #'helpful-key)
(keymap-set global-map "C-h o" #'helpful-symbol)
(add-hook 'emacs-lisp-mode-hook #'eros-mode)
(keymap-set gloom-code-map "E" #'macrostep-expand)
(keymap-set gloom-code-map "v" #'gloom-evaluate)
(keymap-set gloom-code-map "j" #'gloom-repl)
(keymap-set gloom-code-map "T" #'gloom-test)
(keymap-set gloom-code-map "c" #'rustic-cargo-check)
(keymap-set gloom-code-map "B" #'rustic-cargo-build)
(keymap-set gloom-code-map "m" #'emmet-expand-line)
(keymap-set gloom-code-map "V" #'pyvenv-workon)
(keymap-set gloom-code-map "F" #'find-file-at-point)
(keymap-set gloom-code-map "I" #'eglot-find-implementation)
(keymap-set gloom-code-map "Y" #'eglot-find-typeDefinition)
(keymap-set gloom-code-map "i" #'consult-yasnippet)
(keymap-set global-map "C-c - y" #'aya-create)
(keymap-set global-map "C-c - Y" #'aya-expand)
(keymap-set global-map "C-c - d" #'docker)
(keymap-set global-map "C-c - r" #'vertico-repeat)

(defun gloom-evaluate ()
  "Evaluate the selection or run the buffer using existing evaluation tools."
  (interactive)
  (if (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
      (if (use-region-p) (eval-region (region-beginning) (region-end))
        (call-interactively #'eval-defun))
    (if (use-region-p) (quickrun-region (region-beginning) (region-end))
      (quickrun))))
(defun gloom-repl ()
  "Open the current language's installed REPL frontend."
  (interactive)
  (call-interactively
   (cond ((derived-mode-p 'python-mode 'python-ts-mode) #'run-python)
         ((derived-mode-p 'js-mode 'js-ts-mode 'typescript-mode 'typescript-ts-mode
                          'tsx-ts-mode 'web-mode) #'nodejs-repl)
         ((derived-mode-p 'go-mode 'go-ts-mode) #'gorepl-run)
         ((derived-mode-p 'lua-mode 'lua-ts-mode) #'run-lua)
         ((derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode) #'ielm)
         (t (user-error "Use a Ghostel pane for this language's REPL")))))
(defun gloom-test ()
  "Run tests through the installed language integration or compilation."
  (interactive)
  (cond ((derived-mode-p 'python-mode 'python-ts-mode)
         (call-interactively #'python-pytest-dispatch))
        ((derived-mode-p 'rust-mode 'rust-ts-mode 'rustic-mode)
         (call-interactively #'rustic-cargo-test))
        ((derived-mode-p 'go-mode 'go-ts-mode) (compile "go test ./..."))
        ((derived-mode-p 'emacs-lisp-mode) (call-interactively #'ert))
        (t (call-interactively #'compile))))

;; Do not let Rustic initialize a competing LSP client or install packages.
(setq rustic-lsp-client nil rustic-format-trigger nil)
(with-eval-after-load 'rustic-lsp
  (remove-hook 'rustic-mode-hook #'rustic-setup-lsp))
(with-eval-after-load 'rustic-flycheck
  (remove-hook 'rustic-mode-hook #'flymake-mode-off))
(with-eval-after-load 'python
  (setq python-shell-interpreter "python3" python-indent-guess-indent-offset-verbose nil)
  (add-to-list 'python-shell-process-environment "PYTHON_BASIC_REPL=1"))
;; Track explicit .dir-locals virtualenv settings before Eglot starts.
(autoload 'pyvenv-track-virtualenv "pyvenv")
(add-hook 'hack-local-variables-hook
          (lambda ()
            (when (derived-mode-p 'python-mode 'python-ts-mode)
              (pyvenv-track-virtualenv))))
(setq pipenv-with-projectile nil)
(add-hook 'python-mode-hook #'pipenv-mode)
(add-hook 'python-ts-mode-hook #'pipenv-mode)
;; Keep requirement files offline until the user requests package completion.
(with-eval-after-load 'pip-requirements
  (advice-add 'pip-requirements-mode :around #'gloom-requirements-offline)
  (advice-add 'pip-requirements-complete-at-point :before #'gloom-requirements-fetch))
(defun gloom-requirements-offline (function &rest arguments)
  (cl-letf (((symbol-function 'pip-requirements-fetch-packages) #'ignore))
    (apply function arguments)))
(defun gloom-requirements-fetch (&rest _)
  (unless pip-packages (pip-requirements-fetch-packages)))
(defun gloom-node-path ()
  "Make a project's installed Node tools visible to completion and checkers."
  (when (and (not (file-remote-p default-directory))
             (derived-mode-p 'js-mode 'js-ts-mode 'typescript-mode
                             'typescript-ts-mode 'tsx-ts-mode 'web-mode
                             'css-mode 'css-ts-mode 'html-mode 'json-mode))
    (when-let* ((root (locate-dominating-file default-directory "package.json"))
                (bin (expand-file-name "node_modules/.bin" root))
                (_ (file-directory-p bin)))
      (setq-local exec-path (cons bin (remove bin exec-path)))
      (setq-local process-environment (copy-sequence process-environment))
      (setenv "PATH" (concat bin path-separator (getenv "PATH"))))))
(add-hook 'hack-local-variables-hook #'gloom-node-path)
(dolist (mode '(html-mode web-mode css-mode css-ts-mode nxml-mode haml-mode
                pug-mode slim-mode sass-mode stylus-mode))
  (add-hook (intern (format "%s-hook" mode)) #'emmet-mode))
(dolist (mode '(css-mode css-ts-mode sass-mode stylus-mode))
  (add-hook (intern (format "%s-hook" mode)) #'rainbow-mode))
(dolist (entry '(("\\.[jt]sx\\'" . web-mode) ("\\.vue\\'" . web-mode)
                 ("\\.svelte\\'" . web-mode)))
  (add-to-list 'auto-mode-alist entry))
(when (and (fboundp 'treesit-ready-p) (treesit-ready-p 'tsx t)
           (treesit-ready-p 'typescript t))
  (add-to-list 'auto-mode-alist '("\\.[jt]sx\\'" . tsx-ts-mode)))

;; PDF autoloads alone don't activate pdf-view-mode for files.
(gloom-require 'pdf-tools)
(pdf-tools-install-noverify)
(setq-default pdf-view-display-size 'fit-page)
(setq pdf-view-use-scaling t)
(with-eval-after-load 'pdf-view (require 'saveplace-pdf-view))
(defun gloom-pdf-ready-p ()
  (and (require 'pdf-info nil t)
       (stringp pdf-info-epdfinfo-program)
       (file-executable-p pdf-info-epdfinfo-program)))
(defun gloom-pdf-view (function &rest arguments)
  (if (gloom-pdf-ready-p) (apply function arguments)
    (fundamental-mode)
    (message "PDF Tools needs epdfinfo; use M-x pdf-tools-install to build it")))
(advice-add 'pdf-view-mode :around #'gloom-pdf-view)
(defun gloom-org-pdf-handler (function)
  "Defer the PDF dependency until a PDF link needs it."
  (lambda (&rest arguments)
    (when (gloom-pdf-ready-p)
      (require 'org-pdftools)
      (apply function arguments))))
(with-eval-after-load 'org
  (require 'ox-gfm)
  (require 'org-cliplink)
  (require 'ob-async)
  (org-link-set-parameters "pdf"
                           :follow (gloom-org-pdf-handler #'org-pdftools-open)
                           :complete (gloom-org-pdf-handler #'org-pdftools-complete-link)
                           :store (gloom-org-pdf-handler #'org-pdftools-store-link)
                           :export (gloom-org-pdf-handler #'org-pdftools-export))
  (add-hook 'org-mode-hook #'toc-org-mode)
  (setq org-babel-python-command "python3")
  (advice-add 'org-babel-execute-src-block :before #'gloom-babel-load))
(defun gloom-babel-load (&rest _)
  "Load installed Babel language support on first use."
  (when-let* ((info (org-babel-get-src-block-info 'light)) (language (car info)))
    (pcase language
      ("rust" (require 'rustic-babel)
       (defalias 'org-babel-execute:rust #'org-babel-execute:rustic))
      (_ (require (intern (concat "ob-" (if (member language '("sh" "bash"))
                                            "shell" language))) nil t)))))

;; Generic templates use the existing Yas engine.
;; Doom's package/module templates embed Doom helper calls and aren't portable.
(defun gloom-new-file-template ()
  (when (and buffer-file-name (not (file-exists-p buffer-file-name))
             (= (buffer-size) 0))
    (when-let* ((template
                (cond ((derived-mode-p 'python-mode 'python-ts-mode)
                       "#!/usr/bin/env python3\n\n$0")
                      ((derived-mode-p 'emacs-lisp-mode)
                       ";;; `(file-name-nondirectory buffer-file-name)` --- ${1:Description} -*- lexical-binding: t; -*-\n\n$0")
                      ((derived-mode-p 'nix-mode) "{ config, lib, pkgs, ... }:\n\n{\n  $0\n}\n")
                      ((derived-mode-p 'sh-mode 'bash-ts-mode) "#!/usr/bin/env bash\n\n$0"))))
      (yas-minor-mode 1)
      (yas-expand-snippet template))))
(add-hook 'find-file-hook #'gloom-new-file-template t)

;; Built-in display rules replace Doom's popup implementation.
(add-to-list 'display-buffer-alist
             '("\\*\\(?:Help\\|helpful.*\\|eldoc.*\\|Flycheck errors\\|compilation\\|quickrun.*\\)\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)))
(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil locate-command "mdfind"
        delete-by-moving-to-trash (not noninteractive))
  (require 'ns-auto-titlebar)
  (ns-auto-titlebar-mode 1)
  (with-eval-after-load 'auth-source
    (add-to-list 'auth-sources 'macos-keychain-internet)
    (add-to-list 'auth-sources 'macos-keychain-generic)))
(defun gloom-tty-setup ()
  (xterm-mouse-mode 1)
  (require 'kkp) (global-kkp-mode 1)
  (require 'xclip) (xclip-mode 1))
(add-hook 'tty-setup-hook #'gloom-tty-setup)
(gloom-require 'ligature)
(ligature-set-ligatures 'prog-mode '("==" "!=" "=>" "->" "<-" "<=" ">=" "&&" "||" "::"))
(global-ligature-mode 1)
(provide 'gloom-integrations)
