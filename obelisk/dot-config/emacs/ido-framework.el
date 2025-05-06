(use-package ido
  :defer nil
  :ensure nil
  :config
  (ido-mode 1)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching t)
  (ido-everywhere 1)
  ;; Navigation keybindings
  (define-key ido-common-completion-map (kbd "C-l") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "C-h") 'ido-prev-match)
  (define-key ido-common-completion-map (kbd "<escape>") 'abort-recursive-edit)
  (define-key ido-common-completion-map (kbd "<escape>") 'abort-recursive-edit))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package amx
  :after ido
  :config
  (amx-mode 1))

(defalias 'fw-M-x 'amx)
(defalias 'fw-find-file 'projec-find-file)
(defalias 'fw-switch-buffer 'ido-switch-buffer)


;; (use-package vertico
;;      :defer nil
;;      :init
;;      (vertico-mode 1)
;;      (setq vertico-cycle t))
;;    (use-package vertico-directory
;;      :after vertico
;;      :defer nil
;;      :config
;;      (add-to-list 'vertico-directory-exclude '("\\`\\*Embark Collect\\*\\'")))
;;    (use-package vertico-multiform
;;      :after vertico
;;      :defer nil
;;      :config
;;      (vertico-multiform-mode 1))
