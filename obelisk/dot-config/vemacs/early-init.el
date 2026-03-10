;;; early-init.el --- fast startup -*- lexical-binding: t; -*-

(defvar oblsk--gc-before-init gcs-done)

(setq package-enable-at-startup nil)
(setq inhibit-startup-message t)
(setq frame-inhibit-implied-resize t)

(setq package-quickstart t)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Ensure Evil variables exist early to avoid post-command-hook void-variable errors.
(defvar evil-mode nil)
(defvar evil-mode-buffers nil)

(defvar oblsk--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook
 'emacs-startup-hook
 (lambda ()
   ;; Restore GC thresholds after startup idle to avoid GC during init.
   (run-with-idle-timer
    2 nil
    (lambda ()
      (setq gc-cons-threshold (* 64 1024 1024)
            gc-cons-percentage 0.15)))
   (setq file-name-handler-alist oblsk--file-name-handler-alist)
   (makunbound 'oblsk--file-name-handler-alist)))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq redisplay-skip-fontification-on-input t)

;;; early-init.el ends here
