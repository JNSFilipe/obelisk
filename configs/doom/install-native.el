;;; install-native.el --- Prepare Ghostel outside the GUI -*- lexical-binding: t; -*-
;; Invoked by doom-manage.sh after a successful package synchronization.
(let ((build-dir (expand-file-name
                  (format "straight/build-%d.%d" emacs-major-version emacs-minor-version)
                  (or (getenv "DOOMLOCALDIR") (error "DOOMLOCALDIR is required")))))
  (dolist (package '("compat" "ghostel"))
    (add-to-list 'load-path (expand-file-name package build-dir)))
  (when (locate-library "ghostel-module-install")
    (require 'ghostel-module-install)
    (let ((ghostel-module-auto-install 'download))
      (ghostel--load-module t))
    (unless (featurep 'ghostel-module)
      (error "Ghostel native module setup failed"))
    (message "Ghostel native module verified")))
