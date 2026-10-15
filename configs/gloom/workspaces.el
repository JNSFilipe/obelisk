;;; workspaces.el --- persp-mode adapter for shared Sheprd -*- lexical-binding: t; -*-
;; This build contains optional Workgroups macros unavailable to a standalone
;; compiler. Its existing bytecode supports the native window-state path we use.
(let ((native-comp-jit-compilation nil))
  (gloom-require 'persp-mode))
(setq persp-save-dir (expand-file-name "workspaces/" gloom-state-directory)
      persp-auto-save-fname "autosave"
      persp-auto-save-opt 1
      persp-auto-resume-time -1
      persp-nil-hidden t
      persp-set-last-persp-for-new-frames t
      persp-switch-to-added-buffer nil
      persp-autokill-buffer-on-remove nil
      persp-autokill-persp-when-removed-last-buffer nil
      persp-remove-buffers-from-nil-persp-behaviour nil
      persp-reset-windows-on-nil-window-conf nil
      uniquify-buffer-name-style nil)

;; These are the small workspace API surface Sheprd uses. No Doom libraries
;; or macros are loaded. The adapter also works in batch verification.
(defun +workspace-current-name () (safe-persp-name (get-current-persp)))
(defun +workspace-list-names ()
  (cl-remove persp-nil-name (copy-sequence persp-names-cache) :test #'equal))
(defun +workspace-get (name &optional noerror)
  (or (persp-get-by-name name *persp-hash* nil)
      (unless noerror (user-error "No session named %s" name))))
(defun +workspace-exists-p (name) (and (+workspace-get name t) t))
(defun +workspace-buffer-list (&optional persp)
  (cl-remove-if-not #'buffer-live-p (persp-buffers (or persp (get-current-persp)))))
(defun +workspace-new (name)
  (when (or (equal name persp-nil-name) (+workspace-exists-p name))
    (user-error "Session name unavailable: %s" name))
  (let ((persp (persp-add-new name)))
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer (get-buffer-create (format "*scratch:%s*" name)))
      (persp-add-buffer (current-buffer) persp nil nil)
      (setf (persp-window-conf persp)
            (funcall persp-window-state-get-function (selected-frame))))
    persp))
(defun +workspace-switch (name &optional auto-create)
  (unless (+workspace-exists-p name)
    (if auto-create (+workspace-new name)
      (user-error "No session named %s" name)))
  (persp-frame-switch name)
  (equal name (+workspace-current-name)))
(defun +workspace-rename (name new-name)
  (when (+workspace-exists-p new-name)
    (user-error "Session already exists: %s" new-name))
  (persp-rename new-name (+workspace-get name)))
(defun +workspace-delete (persp)
  ;; Sheprd kills owned terminals first. Retain ordinary file buffers.
  (persp-kill (if (stringp persp) persp (persp-name persp)) t)
  (unless (+workspace-list-names) (+workspace-switch "main" t)))
(defun +workspace-kill (workspace &optional keep-buffers)
  (persp-kill (if (stringp workspace) workspace (persp-name workspace)) keep-buffers)
  (unless (+workspace-list-names) (+workspace-switch "main" t)))
(defun +workspace/display () (message "%s" (+workspace-current-name)))

(defun gloom-workspace-initialize (&rest _)
  (when (and persp-mode (null (get-current-persp)))
    (+workspace-switch (or (car (+workspace-list-names)) "main") t)))
(add-hook 'persp-mode-hook #'gloom-workspace-initialize)
(add-hook 'persp-after-load-state-functions #'gloom-workspace-initialize)

(defun gloom-workspace-register-buffer (&rest _)
  "Register visible file buffers without adopting another session's terminal."
  (when (and persp-mode (get-current-persp))
    (dolist (window (window-list nil 'no-minibuffer))
      (let ((buffer (window-buffer window)))
        (with-current-buffer buffer
          (when (and (not (derived-mode-p 'ghostel-mode 'sheprd-sidebar-mode))
                     (or buffer-file-name (derived-mode-p 'dired-mode)))
            (persp-add-buffer buffer (get-current-persp) nil nil)))))))
(add-hook 'window-buffer-change-functions #'gloom-workspace-register-buffer)
(add-hook 'find-file-hook #'gloom-workspace-register-buffer)
(persp-mode 1)

(load (expand-file-name "sheprd.el" gloom-doom-directory) nil t)
(provide 'gloom-workspaces)
