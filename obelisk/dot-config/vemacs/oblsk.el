;; Add history variable for ido completion
(defvar oblsk/make-history nil
  "History list for make targets.")

(defvar oblsk/window-layout nil
 "Saved window layout configuration.")

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
  (call-interactively 'fw-find-file))

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
            (let* ((selected-target (completing-read
                                   "Make target: "
                                   targets
                                   nil  ; no predicate
                                   t    ; require match
                                   nil  ; no initial input
                                   'oblsk/make-history)))  ; history variable
              ;; Run make with the selected target
              (compile (concat "make " selected-target)))
          (error "No targets found in %s" makefile-path))))))

;; Bookmarks
;; Numbered bookmarks configuration for Emacs
;; Set bookmarks with C-S-<1-9> and jump with C-<1-9>

;; Storage for bookmarks (file path and position)
(defvar oblsk/numbered-bookmarks (make-hash-table :test 'equal)
  "Hash table to store numbered bookmarks.")

(defun oblsk/set-numbered-bookmark (number)
  "Set a numbered bookmark at current position."
  (interactive)
  (let ((bookmark-info (list (buffer-file-name) (point))))
    (puthash number bookmark-info oblsk/numbered-bookmarks)
    (message "Bookmark %d set at %s:%d" 
             number 
             (file-name-nondirectory (buffer-file-name))
             (line-number-at-pos))))

(defun oblsk/jump-to-numbered-bookmark (number)
  "Jump to a numbered bookmark."
  (interactive)
  (let ((bookmark-info (gethash number oblsk/numbered-bookmarks)))
    (if bookmark-info
        (let ((file (car bookmark-info))
              (position (cadr bookmark-info)))
          (if (and file (file-exists-p file))
              (progn
                (find-file file)
                (goto-char position)
                (message "Jumped to bookmark %d" number))
            (message "Bookmark %d points to non-existent file" number)))
      (message "Bookmark %d not set" number))))

;; Create functions for each number (1-9)
(dotimes (i 9)
  (let ((num (1+ i)))
    ;; Set bookmark functions
    (eval `(defun ,(intern (format "oblsk/set-bookmark-%d" num)) ()
             ,(format "Set bookmark %d at current position." num)
             (interactive)
             (oblsk/set-numbered-bookmark ,num)))
    
    ;; Jump to bookmark functions
    (eval `(defun ,(intern (format "oblsk/jump-to-bookmark-%d" num)) ()
             ,(format "Jump to bookmark %d." num)
             (interactive)
             (oblsk/jump-to-numbered-bookmark ,num)))))

;; Key bindings
;; Set bookmarks with C-S-<1-9>
(global-set-key (kbd "C-!") 'oblsk/set-bookmark-1)
(global-set-key (kbd "C-\"") 'oblsk/set-bookmark-2)
(global-set-key (kbd "C-#") 'oblsk/set-bookmark-3)
(global-set-key (kbd "C-$") 'oblsk/set-bookmark-4)
(global-set-key (kbd "C-%") 'oblsk/set-bookmark-5)
(global-set-key (kbd "C-&") 'oblsk/set-bookmark-6)
(global-set-key (kbd "C-/") 'oblsk/set-bookmark-7)
(global-set-key (kbd "C-(") 'oblsk/set-bookmark-8)
(global-set-key (kbd "C-)") 'oblsk/set-bookmark-9)

;; Jump to bookmarks with C-<1-9>
(global-set-key (kbd "C-1") 'oblsk/jump-to-bookmark-1)
(global-set-key (kbd "C-2") 'oblsk/jump-to-bookmark-2)
(global-set-key (kbd "C-3") 'oblsk/jump-to-bookmark-3)
(global-set-key (kbd "C-4") 'oblsk/jump-to-bookmark-4)
(global-set-key (kbd "C-5") 'oblsk/jump-to-bookmark-5)
(global-set-key (kbd "C-6") 'oblsk/jump-to-bookmark-6)
(global-set-key (kbd "C-7") 'oblsk/jump-to-bookmark-7)
(global-set-key (kbd "C-8") 'oblsk/jump-to-bookmark-8)
(global-set-key (kbd "C-9") 'oblsk/jump-to-bookmark-9)

;; Function to browse bookmarks with Vertico
;; TODO: Add preview functionality
(defun oblsk/browse-bookmarks ()
  "Browse and jump to bookmarks using Vertico."
  (interactive)
  (let ((bookmarks '()))
    (maphash (lambda (key value)
               (let* ((file (car value))
                      (position (cadr value))
                      (line-num (when (and file (file-exists-p file))
                                  (with-current-buffer (find-file-noselect file)
                                    (save-excursion
                                      (goto-char position)
                                      (line-number-at-pos)))))
                      (display-name (format "Bookmark %d: %s:%s" 
                                          key 
                                          (if file (file-name-nondirectory file) "?")
                                          (or line-num "?"))))
                 (push (cons display-name (list key file position)) bookmarks)))
             oblsk/numbered-bookmarks)
    (if bookmarks
        (let* ((sorted-bookmarks (sort bookmarks (lambda (a b) 
                                                 (< (car (cdr a)) (car (cdr b))))))
               (choice (completing-read "Jump to bookmark: " 
                                      (mapcar 'car sorted-bookmarks))))
          (when choice
            (let* ((bookmark-data (cdr (assoc choice sorted-bookmarks)))
                   (number (car bookmark-data))
                   (file (cadr bookmark-data))
                   (position (caddr bookmark-data)))
              (if (and file (file-exists-p file))
                  (progn
                    (find-file file)
                    (goto-char position)
                    (message "Jumped to bookmark %d" number))
                (message "Bookmark %d points to non-existent file" number)))))
      (message "No bookmarks set"))))

;; Startup time
(defun oblsk/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))
