;; Add history variable for ido completion
(defvar oblsk/make-history nil
  "History list for make targets.")

(defvar oblsk/window-layout nil
 "Saved window layout configuration.")

(defun oblsk/ido-find-file-recursive (&optional dir)
 "Find file recursively using ido with improved performance."
 (interactive)
 (let* ((default-directory (or dir command-line-default-directory))
        (cmd "fd --type f --hidden --exclude .git")
        (file-list (split-string (shell-command-to-string cmd) "\n" t))
        (recent-files-full (mapcar 'expand-file-name recentf-list))
        (recent-project-files
         (seq-filter
          (lambda (f)
            (string-prefix-p (expand-file-name default-directory) f))
          recent-files-full))
        (recent-relative
         (mapcar
          (lambda (f) (file-relative-name f default-directory))
          recent-project-files))
        (sorted-files (delete-dups
                      (append recent-relative
                             file-list)))
        (filename (ido-completing-read
                  (format "Find file recursively from %s: "
                         (abbreviate-file-name default-directory))
                  sorted-files)))
   (find-file filename)))

(defun oblsk/xy-window-pixel-ratio ()
  "Return the ratio of the window's width to its height in pixels."
  (interactive)
  (let* ((edges (window-pixel-edges))
         (width (- (nth 2 edges) (nth 0 edges)))
         (height (- (nth 3 edges) (nth 1 edges)))
         (ratio (/ (float width) height)))
    (if (called-interactively-p 'interactive)
        (message "Width/Height Ratio: %f" ratio)
      ratio)))

(defun oblsk/auto-split-window ()
  "Split the current window along its biggest dimension and run `projectile-find-file`."
  (interactive)
  (if (> (oblsk/xy-window-pixel-ratio) 1.0)
      (split-window-horizontally)       ; Wider window, split horizontally
    (split-window-vertically))          ; Taller window, split vertically
  (other-window 1)
  (call-interactively 'oblsk/ido-find-file-recursive))

(defun oblsk/toggle-window-layout ()
 "Toggle between single window and saved window layout."
 (interactive)
 (if (= (count-windows) 1)
     (when oblsk/window-layout
       (set-window-configuration oblsk/window-layout)
       (setq oblsk/window-layout nil))
   (setq oblsk/window-layout (current-window-configuration))
   (delete-other-windows)))

(defun oblsk/indent-region (num-spaces)
  "Indent or unindent the selected region by NUM-SPACES."
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (indent-rigidly (point) (min (1+ end) (line-end-position)) num-spaces)
            (forward-line)))
        (setq deactivate-mark nil))))

(defun oblsk/find-makefile-targets ()
  "Find and parse Makefile targets from the Emacs launch directory."
  (interactive)
  (let* ((root-dir command-line-default-directory)
         (default-directory root-dir)  ; Set directory for compile command
         (makefile-names '("Makefile" "makefile" "GNUmakefile"))
         (makefile-path (cl-find-if #'file-exists-p makefile-names)))

    (if (not makefile-path)
        (error "No Makefile found in %s" root-dir)

      ;; Parse the Makefile targets
      (let ((targets nil))
        ;; Read the Makefile content
        (with-temp-buffer
          (insert-file-contents makefile-path)

          ;; Go through each line
          (goto-char (point-min))
          (while (not (eobp))
            (if (looking-at "^\\([a-zA-Z0-9_-]+\\):[^=]") ; Match target pattern, exclude variable definitions
                (push (match-string 1) targets))
            (forward-line 1)))

        ;; Remove duplicate targets and sort them
        (setq targets (sort (delete-dups targets) #'string<))

        ;; If we found targets, let user select one
        (if targets
            (let* ((selected-target (ido-completing-read
                                   "Make target: "
                                   targets
                                   nil  ; no predicate
                                   t    ; require match
                                   nil  ; no initial input
                                   'oblsk/make-history)))  ; history variable
              ;; Run make with the selected target
              (compile (concat "make " selected-target)))
          (error "No targets found in %s" makefile-path))))))

