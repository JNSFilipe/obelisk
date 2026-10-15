;;; coding.el --- Built-ins plus existing coding packages -*- lexical-binding: t; -*-
(require 'eglot)
(require 'flymake)
(require 'compile)
(require 'ansi-color)
(require 'completion-preview)
(require 'treesit nil t)
(setq eglot-autoshutdown t
      eglot-events-buffer-config '(:size 0 :format full)
      eldoc-documentation-strategy #'eldoc-documentation-compose
      eldoc-idle-delay 0.1
      compilation-scroll-output 'first-error)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
(add-hook 'prog-mode-hook #'completion-preview-mode)

(defconst gloom-language-servers
  '(((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd"))
    ((python-mode python-ts-mode) . ("pylsp"))
    ((go-mode go-ts-mode) . ("gopls"))
    ((rust-mode rust-ts-mode rustic-mode) . ("rust-analyzer"))
    ((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
     . ("typescript-language-server" "--stdio"))
    ((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
    ((lua-mode lua-ts-mode) . ("lua-language-server"))
    ((nix-mode nix-ts-mode) . ("nil"))
    ((yaml-mode yaml-ts-mode) . ("yaml-language-server" "--stdio"))
    ((json-mode json-ts-mode) . ("vscode-json-language-server" "--stdio"))
    ((terraform-mode) . ("terraform-ls" "serve"))
    ((zig-mode) . ("zls"))))
(dolist (entry gloom-language-servers)
  (add-to-list 'eglot-server-programs entry))

(defun gloom-eglot-ensure ()
  "Start an installed server, checking the remote PATH for TRAMP buffers."
  (let* ((entry (cl-find-if (lambda (entry)
                            (apply #'derived-mode-p (car entry)))
                          gloom-language-servers))
         (program (cadr entry)))
    (when (and program (executable-find program t)) (eglot-ensure))))
;; File/directory locals and project environment hooks must run first. Non-file
;; buffers and explicitly changed modes can use C-c e l to connect manually.
(add-hook 'find-file-hook #'gloom-eglot-ensure t)

;; Existing modes remain available when grammars aren't installed. No grammar
;; downloads at startup; install one explicitly with treesit-install-language-grammar.
(dolist (entry '((c-mode c-ts-mode c) (c++-mode c++-ts-mode cpp)
                 (python-mode python-ts-mode python) (go-mode go-ts-mode go)
                 (rust-mode rust-ts-mode rust) (js-mode js-ts-mode javascript)
                 (typescript-mode typescript-ts-mode typescript)
                 (css-mode css-ts-mode css) (json-mode json-ts-mode json)
                 (yaml-mode yaml-ts-mode yaml) (sh-mode bash-ts-mode bash)))
  (when (and (fboundp 'treesit-ready-p) (fboundp (cadr entry))
             (treesit-ready-p (nth 2 entry) t))
    (add-to-list 'major-mode-remap-alist (cons (car entry) (cadr entry)))))

;; Autoloaded libraries, with their package autoloads providing file associations.
(dolist (spec '((magit-status "magit") (vundo "vundo") (avy-goto-char-timer "avy")
                (consult-eglot-symbols "consult-eglot")
                (makefile-executor-execute-project-target "makefile-executor")
                (helpful-at-point "helpful") (consult-gh "consult-gh")
                (hl-todo-mode "hl-todo") (diff-hl-mode "diff-hl")
                (diff-hl-next-hunk "diff-hl") (diff-hl-previous-hunk "diff-hl")))
  (autoload (car spec) (cadr spec) nil t))
(add-hook 'prog-mode-hook #'hl-todo-mode)
(add-hook 'prog-mode-hook #'diff-hl-mode)
(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
;; Loading Yas runs doom-snippets' autoloaded initialization too. Reuse these
;; libraries' bytecode: the snippet build lacks compiler dependency declarations.
(let ((native-comp-jit-compilation nil))
  (gloom-require 'yasnippet)
  (gloom-require 'doom-snippets))
(yas-global-mode 1)

;; One prefix for language operations, using Eglot/Flymake/Xref directly.
(defvar gloom-code-map (make-sparse-keymap))
(keymap-set mode-specific-map "e" gloom-code-map)
(dolist (binding '(("a" . eglot-code-actions) ("r" . eglot-rename)
                   ("f" . eglot-format-buffer) ("s" . consult-eglot-symbols)
                   ("d" . xref-find-definitions) ("R" . xref-find-references)
                   ("h" . eldoc-doc-buffer) ("D" . flymake-show-project-diagnostics)
                   ("n" . flymake-goto-next-error) ("p" . flymake-goto-prev-error)
                   ("l" . eglot) ("q" . eglot-shutdown) ("i" . yas-insert-snippet)
                   ("b" . eval-buffer) ("x" . eval-defun) ("?" . helpful-at-point)))
  (keymap-set gloom-code-map (car binding) (cdr binding)))
(provide 'gloom-coding)
