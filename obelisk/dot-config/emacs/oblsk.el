(defun oblsk/find-file ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'find-file)))

;; Recursive file finding with ido
(defun oblsk/ido-find-file-recursive (&optional dir)
  "Find file recursively from Emacs' launch directory using ido."
  (interactive)
  (let* ((current-dir (or dir command-line-default-directory))
         (file-list (oblsk/ido-recursive-get-files current-dir))
         (file-alist (mapcar (lambda (file)
                              (cons (file-relative-name file current-dir) file))
                            file-list))
         (filename (ido-completing-read (format "Find file recursively from %s: "
                                              (abbreviate-file-name current-dir))
                                      (mapcar 'car file-alist))))
    (find-file (cdr (assoc filename file-alist)))))

(defun oblsk/ido-recursive-get-files (dir)
  "Get list of files recursively in DIR, excluding version control and backup files."
  (let ((files '())
        (ignored-dirs '(".git" ".svn" "CVS" ".hg" ".bzr" ".DS_Store" "node_modules"))
        (ignored-extensions '(".elc" "~" ".swp" ".cache")))
    (dolist (file (directory-files dir t "." t))
      (let ((file-name (file-name-nondirectory file)))
        (cond
         ;; Skip . and ..
         ((member file-name '("." "..")) nil)
         ;; Skip ignored directories
         ((and (file-directory-p file)
               (member file-name ignored-dirs)) nil)
         ;; Recurse into directories
         ((file-directory-p file)
          (setq files (append files (oblsk/ido-recursive-get-files file))))
         ;; Skip files with ignored extensions
         ((member (file-name-extension file t) ignored-extensions) nil)
         ;; Add regular files
         (t (push file files)))))
    (nreverse files)))

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
        (setq deactivate-mark nil))
    (if (< num-spaces 0) (meow-unpop-to-mark) (meow-pop-to-mark))))


(defun oblsk/meow-append ()
  "Append after cursor or after selection if region is active."
  (interactive)
  (if (region-active-p)
      (goto-char (max (region-beginning) (region-end))))
  (unless (eolp) (forward-char 1))
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--direction-forward)
    (when (bound-and-true-p delete-selection-mode)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun oblsk/meow-left ()
  (interactive)
  (if (region-active-p)
      (meow-left-expand)
    (meow-left)))

(defun oblsk/meow-next ()
  (interactive)
  (if (region-active-p)
      (meow-next-expand 1)
    (meow-next 1)))

(defun oblsk/meow-prev ()
  (interactive)
  (if (region-active-p)
      (meow-prev-expand 1)
    (meow-prev 1)))

(defun oblsk/meow-right ()
  (interactive)
  (if (region-active-p)
      (meow-right-expand)
    (meow-right)))

(defun oblsk/meow-kill-or-delete ()
  (interactive)
  (if (region-active-p)
      (meow-kill)
    (meow-delete)))

(defun oblsk/meow-kill-or-delete-whole-line ()
  (interactive)
  (if (region-active-p)
      (meow-kill)
    (meow-kill-whole-line)))

(defun oblsk/meow-append ()
  "Append after cursor (like Vim's 'a')"
  (interactive)
  (meow-append)
  (forward-char))

(defun oblsk/meow-append-end-of-line ()
  "Append at the end of current line, ignoring any active selection."
  (interactive)
  (end-of-line)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--direction-forward)
    (when (bound-and-true-p delete-selection-mode)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun oblsk/meow-insert-beginning-of-line ()
  "Insert at the beginning of current line, ignoring any active selection."
  (interactive)
  (beginning-of-line)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--direction-forward)
    (when (bound-and-true-p delete-selection-mode)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun oblsk/meow-vim-open-below ()
  "Open line below (like Vim's 'o')"
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (meow-insert))

(defun oblsk/meow-vim-open-above ()
  "Open line above (like Vim's 'O')"
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  (meow-insert))

(defun oblsk/meow-save-line ()
  "Select the current line and copy it using meow-save."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (meow-save)))

(defun oblsk/meow-save ()
  (interactive)
  (if (region-active-p)
      (meow-save)
    (oblsk/meow-save-line)))

(defun oblsk/meow-toggle-case ()
  "Toggle case of character under cursor or selected text.
If a region is active, toggle case of all characters in region.
If no region is active, toggle case of character at point."
  (interactive)
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring start end))
             (toggled-text
              (mapconcat
               (lambda (char)
                 (cond
                  ((= char (upcase char)) (char-to-string (downcase char)))
                  (t (char-to-string (upcase char)))))
               text
               "")))
        (delete-region start end)
        (insert toggled-text)
        (meow--cancel-selection))
    (let* ((char (char-after))
           (toggled-char
            (if (and char (= char (upcase char)))
                (downcase char)
              (upcase char))))
      (delete-char 1)
      (insert-char toggled-char)
      (backward-char))))

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

;; Add history variable for ido completion
(defvar oblsk/make-history nil
  "History list for make targets.")

(defun oblsk/meow-setup ()
  ;; Enable modeline indicator
  (meow-setup-indicator)
  ;; Define prefixes that are baypassed to keypad mode
  (setq meow-keypad-start-keys '((?x . ?x)))
  ;; Define the layout
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("?" . meow-cheatsheet)
   '(";" . eval-expression)
   '("." . ibuffer)
   '("," . scratch-buffer)
   '("*" . project-search)
   '("'" . text-scale-adjust)
   '("b" . ido-switch-buffer)
   '("c" . comment-line)
   '("w" . save-buffer)
   '("a" . lsp-execute-code-action)
   ;; '("o" . oblsk/dired)
   '("f" . oblsk/ido-find-file-recursive)
   ;; '("h" . replace-string)
   '("d" . consult-flymake)
   '("r" . async-shell-command)
   '("m" . compile)
   '("M" . oblsk/find-makefile-targets)
   '("SPC" . execute-extended-command)
   ;; '("u" . undo-tee-visualize)
   '("s" . oblsk/auto-split-window))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("/" . meow-visit)
   '("," . meow-beginning-of-thing) ;; '("[" . meow-beginning-of-thing)
   '("." . meow-end-of-thing) ;; '("]" . meow-end-of-thing)
   '("t" . meow-inner-of-thing) ;; '("," . meow-inner-of-thing)
   '("T" . meow-bounds-of-thing) ;; '("." . meow-bounds-of-thing)
   '("a" . oblsk/meow-append)      ; Append after cursor
   '("A" . oblsk/meow-append-end-of-line) ; Append at end of line
   '("o" . oblsk/meow-vim-open-below)  ; Open line below
   '("O" . oblsk/meow-vim-open-above)  ; Open line above
   '("c" . meow-change)
   '("d" . oblsk/meow-kill-or-delete) ;; '("d" . meow-delete)
   '("D" . oblsk/meow-kill-or-delete-whole-line) ;; '("D" . meow-backward-delete)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("gg" . beginning-of-buffer) ; beginning of buffer
   '("G" . end-of-buffer)       ; end of buffer
   '("f" . meow-find)
   '("V" . meow-visual-line)     ; Enter line visual mode
   '("v" . meow-visit)
   '("S" . meow-grab)
   '("i" . meow-insert)
   '("I" . oblsk/meow-insert-beginning-of-line)
   '("h" . oblsk/meow-left)
   '("H" . meow-left-expand)
   '("j" . oblsk/meow-next)
   '("J" . meow-next-expand)
   '("k" . oblsk/meow-prev)
   '("K" . meow-prev-expand)
   '("l" . oblsk/meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("c" . oblsk/meow-toggle-case)
   '("Q" . meow-block) ;; '("o" . meow-block)
   ;; '("C-B" . meow-to-block) ;; '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop) ;; This presents a paste menu
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   ;; '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("C-r" . undo-redo)          ; Redo
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   ;; '("V" . meow-line) ;; '("x" . meow-line)
   '(":" . meow-goto-line) ;; '("X" . meow-goto-line)
   '("y" . oblsk/meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("ç" . git-gutter:next-hunk)
   '("Ç" . git-gutter:previous-hunk)
   '("<" .  (lambda () (interactive) (oblsk/indent-region 2)))
   '(">" .  (lambda () (interactive) (oblsk/indent-region -2)))
   '("<escape>" . meow-cancel-selection) ;; '("<escape>" . ignore)
   '("C-g" . meow-cancel-selection)
   '("C-h C-h" . windmove-left)
   '("C-l" . windmove-right)
   '("C-k" . windmove-up)
   '("C-j" . windmove-down)
   '("C-q" . delete-window)))

