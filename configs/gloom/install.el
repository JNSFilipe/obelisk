;;; install.el --- Explicit setup; never runs at startup -*- lexical-binding: t; -*-
(load (expand-file-name "packages.el" (file-name-directory load-file-name)) nil t)
(let ((missing (cl-remove-if
                (lambda (package) (locate-library (symbol-name package)))
                gloom-packages)))
  (when missing
    ;; These packages are available upstream without an archive refresh.
    ;; Installing Meow alone should not fetch three unrelated archive indexes.
    (dolist (package missing)
      (pcase package
        ('meow
         ;; Meow has no external dependencies. package-vc otherwise contacts
         ;; archive hosts even when given a complete upstream URL.
         (let ((package-archives nil))
           (package-vc-install "https://github.com/meow-edit/meow")))
        ('ghostel
         (package-vc-install '(ghostel :url "https://github.com/dakra/ghostel"
                                       :lisp-dir "lisp")))
        ('doom-snippets
         (package-vc-install '(doom-snippets :url "https://github.com/doomemacs/snippets"
                                            :main-file "doom-snippets.el")))
        ('mx-machina
         ;; Not on any archive.  Its own dependencies are optional interfaces.
         (let ((package-archives nil))
           (package-vc-install '(mx-machina :url "https://github.com/eliraz-refael/m-x-machina"
                                            :lisp-dir "lisp"))))
        (_ nil)))
    (let ((archive-packages (cl-set-difference missing '(meow ghostel doom-snippets mx-machina))))
      (when archive-packages
        (package-refresh-contents)
        (dolist (package archive-packages) (package-install package))))))
(require 'ghostel-module-install)
(let ((ghostel-module-auto-install 'download))
  (ghostel--load-module t))
(unless (featurep 'ghostel-module)
  (error "Ghostel native module setup failed"))
(message "Gloom dependencies and Ghostel native module ready")
