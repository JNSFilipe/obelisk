(use-package helm
  :defer nil
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key helm-map (kbd "<C-backspace>") 'helm-find-files-up-one-level)
  (define-key helm-map (kbd "RET") 'helm-maybe-exit-minibuffer)  ;; Changed this line
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (setq helm-M-x-always-show-doc t)
  
  ;; Set maximum height to 30% of screen height
  (setq helm-autoresize-max-height 30)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  
  ;; Commonly used Helm options
  (setq helm-split-window-in-side-p t)  ;; Open helm buffer inside current window
  (setq helm-move-to-line-cycle-in-source t)  ;; Cycle through list when reaching end
  (setq helm-ff-search-library-in-sexp t)  ;; Search for library in `require' and `declare-function'
  (setq helm-scroll-amount 8)  ;; Scroll 8 lines at a time with M-<next>/M-<prior>
  (setq helm-ff-file-name-history-use-recentf t)  ;; Use recentf for file name history
  (setq helm-echo-input-in-header-line t)  ;; Show input in header line
  
  (helm-mode 1))

(defalias 'fw-M-x 'helm-M-x)
(defalias 'fw-find-file 'project-find-file)
(defalias 'fw-switch-buffer 'helm-buffers-list)
