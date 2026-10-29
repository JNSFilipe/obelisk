;;; install.el --- Explicit setup; never runs at startup -*- lexical-binding: t; -*-
(load (expand-file-name "packages.el" (file-name-directory load-file-name)) nil t)
(let ((missing (cl-remove-if
                (lambda (package) (locate-library (symbol-name package)))
                gloom-packages)))
  (when missing
    ;; These packages are available upstream without an archive refresh.
    (dolist (package missing)
      (pcase package
        ((or 'ghostel 'evil-ghostel)
         ;; evil-ghostel ships in Ghostel's repository.
         (unless (package-installed-p 'ghostel)
           (package-vc-install '(ghostel :url "https://github.com/dakra/ghostel"
                                         :lisp-dir "lisp"))))
        ('doom-snippets
         (package-vc-install '(doom-snippets :url "https://github.com/doomemacs/snippets"
                                            :main-file "doom-snippets.el")))
        (_ nil)))
    (let ((archive-packages (cl-set-difference missing '(ghostel evil-ghostel doom-snippets))))
      (when archive-packages
        (package-refresh-contents)
        (dolist (package archive-packages) (package-install package))))))
(require 'ghostel-module-install)
(let ((ghostel-module-auto-install 'download))
  (ghostel--load-module t))
(unless (featurep 'ghostel-module)
  (error "Ghostel native module setup failed"))
(message "Gloom dependencies and Ghostel native module ready")
