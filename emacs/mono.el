;;; ../../Documents/GitHub/monolith.nvim/demacs/mono.el -*- lexical-binding: t; -*-

(defvar dired-copy-paste-func nil)
(defvar dired-copy-paste-stored-file-list nil)

(defun mono/find-file ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'find-file)))

(defun mono/dired ()
  (interactive)
  (if (projectile-project-p)
      (dired (projectile-project-root))
    (dired "~/")))

(defun mono/list-buffers ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively '+vertico/switch-workspace-buffer) ; or '+helm/switch-workspace-buffer for Helm users
    (call-interactively 'ibuffer))) ; or another buffer listing command as per preference

(defun mono/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun mono/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun mono/xy-window-pixel-ratio ()
  "Return the ratio of the window's width to its height in pixels."
  (interactive)
  (let* ((edges (window-pixel-edges))
         (width (- (nth 2 edges) (nth 0 edges)))
         (height (- (nth 3 edges) (nth 1 edges)))
         (ratio (/ (float width) height)))
    (if (called-interactively-p 'interactive)
        (message "Width/Height Ratio: %f" ratio)
      ratio)))

(defun mono/auto-split-window ()
  (interactive)
  ;; Determine split direction based on window dimensions
  (if (> (mono/xy-window-pixel-ratio) 1.0)
      (split-window-horizontally)       ; Wider window, split horizontally
    (split-window-vertically))          ; Taller window, split vertically
  ;; Open a file in the new window using projectile-find-file
  (other-window 1)
  (projectile-find-file))

(defun mono/minibuffer-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun mono/run-make-or-compile ()
  "Run `+make/run` if a Makefile exists, otherwise `compile`."
  (interactive)
  (if (file-exists-p (expand-file-name "Makefile" (doom-project-root)))
      (+make/run)
    (call-interactively 'compile)))

(defun mono/dired-open ()
  "In Dired, open the file or directory under the cursor.
Opens directories in Emacs. Opens files with Emacs if possible, otherwise uses xdg-open."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-find-file)
      (if (file-executable-p file)
          (start-process "" nil "xdg-open" file)
        (find-file file)))))

(defun mono/dired-open-split ()
  "Open the current item in Dired in a new right split.
If it's a directory, open in Dired. If it's a file Emacs can open, open it in Emacs.
Otherwise, open it using xdg-open."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (progn
          (split-window-right)
          (other-window 1)
          (dired file))
      (if (file-exists-p file)
          (progn
            (split-window-right)
            (other-window 1)
            (if (file-readable-p file)
                (find-file file)
              (start-process "" nil "xdg-open" file)))
        (message "File does not exist")))))

(defun mono/dired-create-path (name)
  "Create a file or directory based on the input NAME.
If NAME ends with a '/', it creates a directory, otherwise a file."
  (interactive "sEnter file or directory name: ")
  (let ((dir (if (string-suffix-p "/" name)
                 name
               (file-name-directory name))))
    (when dir
      (make-directory dir t))
    (unless (string-suffix-p "/" name)
      (write-region "" nil name))))

(defun mono/close-dired ()
  "Kill the current dired buffer and close its window."
  (interactive)
  (when (eq major-mode 'dired-mode)
    (kill-this-buffer)
    (delete-window)))

(defun mono/dired-do-yank ()
  "In dired-mode, copy a file/dir on current line or all marked file/dir(s)."
  (interactive)
  (setq dired-copy-paste-stored-file-list (dired-get-marked-files)
        dired-copy-paste-func 'dired-copy-file)
  (message
   (format "%S is/are copied."dired-copy-paste-stored-file-list)))

(defun mono/dired-do-paste ()
  "In dired-mode, paste cut/copied file/dir(s) into current directory."
  (interactive)
  (let ((stored-file-list nil))
    (dolist (stored-file dired-copy-paste-stored-file-list)
      (condition-case nil
          (progn
            (funcall dired-copy-paste-func stored-file (dired-current-directory) 1)
            (push stored-file stored-file-list))))
    ;; (error nil)

    (if (eq dired-copy-paste-func 'rename-file)
        (setq dired-copy-paste-stored-file-list nil
              dired-copy-paste-func nil))
    (revert-buffer)
    (message
     (format "%d file/dir(s) pasted into current directory." (length stored-file-list)))))

(defun mono/dired-toggle-mark ()
  "Toggle the mark on the current file and stay on the same line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at dired-re-mark)
        (dired-unmark 1)
      (dired-mark 1))))

(defun mono/toggle-maximize-layout ()
  "Maximize the current window and close others."
  (interactive)
  (if (> (length (window-list)) 1)
      (progn
        (setq my-last-window-configuration (current-window-configuration))
        (delete-other-windows))
    (if (bound-and-true-p my-last-window-configuration)
        (set-window-configuration my-last-window-configuration)
      (message "No previous window configuration to restore."))))
