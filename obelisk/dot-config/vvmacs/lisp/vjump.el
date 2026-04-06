;;; vjump.el --- Visual jump tree  -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: JNSFilipe <jose.filipe@ieee.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.5"))
;; Keywords: navigation, jump, tree

;;; Commentary:
;; vjump.el provides a visual jump tree similar to vundo but for navigation
;; history.  Jump history is stored as a tree: going back and then jumping
;; elsewhere creates a branch, preserving all history.
;;
;; Usage:
;;   (vjump-mode 1)                 ; enable tracking
;;   (global-set-key "C-x j" #'vjump-visualize)
;;   (global-set-key "C-o"   #'vjump-go-back)
;;   (global-set-key "C-i"   #'vjump-go-forward)
;;
;; In the *vjump-tree* buffer:
;;   f/→  forward to child     b/←  back to parent
;;   n/↓  next sibling         p/↑  previous sibling
;;   a    back to branch point  e    forward to tip
;;   q    quit (roll back)      RET  confirm and close

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;; Customization

(defgroup vjump nil
  "Visual jump tree."
  :group 'navigation)

(defcustom vjump-distance-threshold 10
  "Lines of movement that count as a jump (for post-command-hook detection)."
  :type 'integer
  :group 'vjump)

(defcustom vjump-window-max-height 3
  "Maximum height of the vjump side window."
  :type 'integer
  :group 'vjump)

(defcustom vjump-window-side 'bottom
  "Side where the vjump window appears."
  :type '(choice (const bottom) (const top))
  :group 'vjump)

(defcustom vjump-roll-back-on-quit t
  "If non-nil, quit rolls back to the node current when vjump-visualize was called."
  :type 'boolean
  :group 'vjump)

(defcustom vjump-compact-display nil
  "Show compact tree (shorter stems) if non-nil."
  :type 'boolean
  :group 'vjump)

(defcustom vjump-glyph-alist
  '((selected-node   . ?●)
    (node            . ?○)
    (horizontal-stem . ?─)
    (vertical-stem   . ?│)
    (branch          . ?├)
    (last-branch     . ?└))
  "Alist mapping tree part names to characters.
If vundo is loaded, you can set this to `vundo-glyph-alist'."
  :type 'alist
  :group 'vjump)

;;; Faces

(defface vjump-default '((t . (:inherit default)))
  "Default face for the vjump buffer."
  :group 'vjump)

(defface vjump-node '((t . (:inherit vjump-default)))
  "Face for unselected nodes."
  :group 'vjump)

(defface vjump-stem '((t . (:inherit vjump-default)))
  "Face for stems between nodes."
  :group 'vjump)

(defface vjump-branch-stem '((t . (:inherit vjump-stem :weight bold)))
  "Face for stems at branching points."
  :group 'vjump)

(defface vjump-highlight
  '((((background light)) . (:inherit vjump-node :weight bold :foreground "red"))
    (((background dark))  . (:inherit vjump-node :weight bold :foreground "yellow")))
  "Face for the currently selected node."
  :group 'vjump)

;;; Node struct

(cl-defstruct vjump-node
  "A node in the jump tree.
`marker' encodes buffer+position.  `point' is the text position of
this node's glyph inside the *vjump-tree* buffer (set by the draw engine)."
  marker    ; Emacs marker (buffer + position); nil for sentinel root
  parent    ; parent vjump-node or nil (sentinel root has nil parent)
  children  ; list of child vjump-nodes, newest first
  point     ; integer: text position of this node's glyph in *vjump-tree*
  timestamp) ; float-time, for display

;;; Global state

(defvar vjump--root nil
  "Sentinel root node.  Has no marker.  All recorded jumps hang off its children.")

(defvar vjump--current nil
  "Pointer to the node representing the current position in the jump tree.")

;;; Core tree operations

(defun vjump--make-sentinel ()
  "Create and return a new sentinel root node."
  (make-vjump-node :marker nil :parent nil :children nil
                   :timestamp (float-time)))

(defun vjump--push (pos buffer)
  "Record a jump to POS in BUFFER as a new child of `vjump--current'.
Initialises the tree on first call.  When `vjump--current' already has
children, the new node becomes a new sibling branch — existing children
are never discarded."
  (unless vjump--root
    (setq vjump--root    (vjump--make-sentinel)
          vjump--current vjump--root))
  (let* ((marker (with-current-buffer buffer
                   (copy-marker pos)))
         (node (make-vjump-node
                :marker    marker
                :parent    vjump--current
                :children  nil
                :timestamp (float-time))))
    ;; Prepend so children are newest-first.  This makes `vjump-go-forward'
    ;; (C-i) return to the most recently visited branch after a C-o, which
    ;; mirrors the feel of Vim's C-i/C-o.  The spec originally specified
    ;; newest-last; the ordering was intentionally reversed during
    ;; implementation for better UX.
    (setf (vjump-node-children vjump--current)
          (cons node (vjump-node-children vjump--current)))
    (setq vjump--current node)))

(defun vjump--jump-to-current ()
  "Visit the buffer and position stored in `vjump--current'.
Does nothing if current is the sentinel root or its marker is dead."
  (when-let* ((node vjump--current)
              (marker (vjump-node-marker node))
              (buf (marker-buffer marker))
              ((buffer-live-p buf)))
    (switch-to-buffer buf)
    (goto-char (marker-position marker))))

;;;###autoload
(defun vjump-go-back ()
  "Move to the parent node (tree-aware C-o).
Does not move past the first real node (child of sentinel root)."
  (interactive)
  (when-let* ((parent (and vjump--current
                           (vjump-node-parent vjump--current)))
              ;; Parent must have a marker — sentinel root does not
              ((vjump-node-marker parent)))
    (setq vjump--current parent)
    (vjump--jump-to-current)))

;;;###autoload
(defun vjump-go-forward ()
  "Move to the first child node (tree-aware C-i)."
  (interactive)
  (when-let* ((children (and vjump--current
                             (vjump-node-children vjump--current)))
              (first-child (car children)))
    (setq vjump--current first-child)
    (vjump--jump-to-current)))

;;; Jump detection

(defvar vjump--pre-command-point nil
  "Value of `point' before the current command, set by `vjump--pre-command'.")

(defvar vjump--pre-command-buffer nil
  "Current buffer before the current command, set by `vjump--pre-command'.")

(defvar vjump--push-mark-called nil
  "Non-nil when `push-mark' fired during the current command.
Used to prevent double-recording between the push-mark advice and
`vjump--post-command'.")

(defun vjump--pre-command ()
  "Record current point and buffer before every command."
  (setq vjump--pre-command-point  (point)
        vjump--pre-command-buffer (current-buffer)))

(defun vjump--post-command ()
  "After each command, record a jump if warranted."
  (cond
   ;; push-mark advice already recorded this — just clear the flag
   (vjump--push-mark-called
    (setq vjump--push-mark-called nil))
   ;; Buffer changed
   ((and vjump--pre-command-buffer
         (not (eq (current-buffer) vjump--pre-command-buffer)))
    (vjump--push (point) (current-buffer)))
   ;; Large distance movement in the same buffer
   ((and vjump--pre-command-point
         (> (abs (- (line-number-at-pos (point))
                    (line-number-at-pos vjump--pre-command-point)))
            vjump-distance-threshold))
    (vjump--push (point) (current-buffer)))))

(defun vjump--push-mark-advice (&rest _)
  "Advice for `push-mark': record a jump and set the dedup flag."
  (setq vjump--push-mark-called t)
  (vjump--push (point) (current-buffer)))

(defun vjump--pop-mark-advice (&rest _)
  "Advice for `pop-to-mark-command': move vjump--current to its parent."
  (when (and vjump--current
             (vjump-node-parent vjump--current)
             (vjump-node-marker (vjump-node-parent vjump--current)))
    (setq vjump--current (vjump-node-parent vjump--current))))

;;; Draw engine (ported from vundo by Yuan Fu, GPL-3+)

(defun vjump--translate (text)
  "Translate each character in TEXT using `vjump-glyph-alist'."
  (seq-mapcat
   (lambda (ch)
     (let ((key (pcase ch
                  (?○ 'node)
                  (?● 'selected-node)
                  (?─ 'horizontal-stem)
                  (?│ 'vertical-stem)
                  (?├ 'branch)
                  (?└ 'last-branch)
                  (_ nil))))
       (if key
           (char-to-string (alist-get key vjump-glyph-alist))
         (char-to-string ch))))
   text 'string))

(defun vjump--next-line-at-column (col)
  "Move point to the next line at column COL, inserting a newline if needed."
  (unless (and (eq 0 (forward-line)) (not (eobp)))
    (goto-char (point-max))
    (insert "\n"))
  (move-to-column col)
  (unless (eq (current-column) col)
    (let ((indent-tabs-mode nil))
      (indent-to-column col))))

(defun vjump--put-node-at-point (node)
  "Store NODE as a `vjump-node' text property at point."
  (put-text-property (1- (point)) (point) 'vjump-node node))

(defun vjump--get-node-at-point ()
  "Return the `vjump-node' text property at point, or nil."
  (get-text-property (1- (point)) 'vjump-node))

(defun vjump--node-label (node)
  "Return a short label for NODE: \"buffer-name:line\" or nil if dead."
  (when-let* ((marker (vjump-node-marker node))
              (buf    (marker-buffer marker))
              ((buffer-live-p buf))
              (pos    (marker-position marker)))
    (with-current-buffer buf
      (format "%s:%d" (buffer-name buf) (line-number-at-pos pos)))))

(defun vjump--draw-tree (root)
  "Draw the jump tree rooted at ROOT into the current buffer.
ROOT is typically `vjump--root' (the sentinel).  Each node's `point'
field is set to the text position of its glyph so later commands can
navigate to it."
  (let ((node-queue (list root))
        (inhibit-read-only t)
        (inhibit-modification-hooks t))
    (erase-buffer)
    (while node-queue
      (let* ((node              (pop node-queue))
             (children          (vjump-node-children node))
             (parent            (vjump-node-parent node))
             (siblings          (and parent (vjump-node-children parent)))
             (only-child-p      (and parent (= (length siblings) 1)))
             (last-child-p      (and parent (eq node (car (last siblings)))))
             (stem-face         (if only-child-p 'vjump-stem 'vjump-branch-stem)))
        ;; Navigate to parent's glyph position in the buffer
        (when parent
          (goto-char (vjump-node-point parent)))
        (let ((room-rx (rx-to-string
                        `(or (>= ,(if vjump-compact-display 3 4) ?\s) eol))))
          (if (null parent)
              ;; Sentinel root: just insert the root glyph
              (insert (propertize (vjump--translate "○") 'face 'vjump-node))
            (let ((planned-point (point)))
              ;; Find room on the same line, or break to next line
              (while (not (looking-at room-rx))
                (vjump--next-line-at-column (max 0 (1- (current-column))))
                (let ((replace-char
                       (if (looking-at
                            (rx-to-string
                             `(or ,(vjump--translate "├")
                                  ,(vjump--translate "└"))))
                           (vjump--translate "├")
                         (vjump--translate "│"))))
                  (unless (eolp) (delete-char 1))
                  (insert (propertize replace-char 'face stem-face))))
              (unless (looking-at "$")
                (delete-char (if vjump-compact-display 2 3)))
              (if (eq (point) planned-point)
                  ;; Inline child: ──○
                  (progn
                    (insert (propertize
                             (vjump--translate (if vjump-compact-display "─" "──"))
                             'face stem-face))
                    (insert (propertize (vjump--translate "○") 'face 'vjump-node)))
                ;; Broke to new line: └──○ or ├──○
                (delete-char -1)
                (insert (propertize
                         (vjump--translate
                          (if last-child-p
                              (if vjump-compact-display "└─" "└──")
                            (if vjump-compact-display "├─" "├──")))
                         'face stem-face))
                (insert (propertize (vjump--translate "○") 'face 'vjump-node))))))
        ;; Record this node's glyph position and store text property
        (setf (vjump-node-point node) (point))
        (vjump--put-node-at-point node)
        ;; Enqueue children (depth-first, matching vundo's traversal order)
        (setq node-queue (append children node-queue))))))

;;; Visualization buffer

(defvar-local vjump--orig-window nil
  "The window vjump-visualize was invoked from.")

(defvar-local vjump--roll-back-to-this nil
  "The node that was current when vjump-visualize was called.
Used by `vjump-quit' to roll back if `vjump-roll-back-on-quit' is non-nil.")

(defvar-local vjump--highlight-overlay nil
  "Overlay used to show the selected node as ●.")

(defvar vjump-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f")       #'vjump-forward)
    (define-key map (kbd "<right>") #'vjump-forward)
    (define-key map (kbd "b")       #'vjump-backward)
    (define-key map (kbd "<left>")  #'vjump-backward)
    (define-key map (kbd "n")       #'vjump-next)
    (define-key map (kbd "<down>")  #'vjump-next)
    (define-key map (kbd "p")       #'vjump-previous)
    (define-key map (kbd "<up>")    #'vjump-previous)
    (define-key map (kbd "a")       #'vjump-stem-root)
    (define-key map (kbd "e")       #'vjump-stem-end)
    (define-key map (kbd "q")       #'vjump-quit)
    (define-key map (kbd "C-g")     #'vjump-quit)
    (define-key map (kbd "RET")     #'vjump-confirm)
    map)
  "Keymap for `vjump-tree-mode'.")

(define-derived-mode vjump-tree-mode special-mode "VJump"
  "Major mode for the vjump tree buffer."
  (setq mode-line-format nil
        truncate-lines    t
        cursor-type       nil)
  (jit-lock-mode nil)
  (face-remap-add-relative 'default 'vjump-default))

(defun vjump--buffer ()
  "Return (creating if needed) the *vjump-tree* buffer."
  (get-buffer-create " *vjump-tree*"))

(defun vjump--highlight-node (node)
  "Highlight NODE as the current node (display ● at its glyph position)."
  (when (vjump-node-point node)
    (with-current-buffer (vjump--buffer)
      (unless vjump--highlight-overlay
        (setq vjump--highlight-overlay
              (make-overlay (1- (vjump-node-point node))
                            (vjump-node-point node)))
        (overlay-put vjump--highlight-overlay 'display
                     (vjump--translate "●"))
        (overlay-put vjump--highlight-overlay 'face 'vjump-highlight))
      (move-overlay vjump--highlight-overlay
                    (1- (vjump-node-point node))
                    (vjump-node-point node)))))

(defun vjump--refresh-buffer ()
  "Redraw the vjump tree buffer and highlight `vjump--current'."
  (with-current-buffer (vjump--buffer)
    (unless (derived-mode-p 'vjump-tree-mode)
      (vjump-tree-mode))
    (when vjump--root
      ;; Nil the overlay before erase-buffer (called inside vjump--draw-tree)
      ;; destroys it, so vjump--highlight-node always creates a fresh one.
      (setq vjump--highlight-overlay nil)
      (vjump--draw-tree vjump--root)
      (vjump--highlight-node vjump--current)
      (goto-char (vjump-node-point vjump--current)))))

;;; Navigation commands (used inside *vjump-tree* buffer)

(defmacro vjump--with-orig-window (&rest body)
  "Execute BODY in `vjump--orig-window', if it is still live."
  `(when (window-live-p vjump--orig-window)
     (with-selected-window vjump--orig-window
       ,@body)))

(defun vjump--move-to-node (node)
  "Set `vjump--current' to NODE, jump the original window, refresh tree."
  (setq vjump--current node)
  ;; Jump the originating window live
  (vjump--with-orig-window
   (when-let* ((marker (vjump-node-marker node))
               (buf    (marker-buffer marker))
               ((buffer-live-p buf)))
     (switch-to-buffer buf)
     (goto-char (marker-position marker))))
  ;; Echo the label
  (when-let ((label (vjump--node-label node)))
    (message "vjump: %s" label))
  ;; Redraw + highlight in tree buffer
  (vjump--refresh-buffer))

(defun vjump-forward ()
  "Move forward to the first child of the current node."
  (interactive)
  (when-let ((child (car (vjump-node-children vjump--current))))
    (vjump--move-to-node child)))

(defun vjump-backward ()
  "Move backward to the parent of the current node.
Stops at the first real node (child of sentinel root)."
  (interactive)
  (when-let* ((parent (vjump-node-parent vjump--current))
              ((vjump-node-marker parent)))  ; sentinel root has no marker
    (vjump--move-to-node parent)))

(defun vjump-next (arg)
  "Move to the next sibling node (ARG steps downward)."
  (interactive "p")
  (when-let* ((parent   (vjump-node-parent vjump--current))
              (siblings (vjump-node-children parent))
              (idx      (seq-position siblings vjump--current))
              (new-idx  (max 0 (min (+ idx arg) (1- (length siblings)))))
              (dest     (nth new-idx siblings))
              ((not (eq dest vjump--current))))
    (vjump--move-to-node dest)))

(defun vjump-previous (arg)
  "Move to the previous sibling node (ARG steps upward)."
  (interactive "p")
  (vjump-next (- arg)))

(defun vjump-stem-root ()
  "Move back to the nearest branching point (or root of current stem)."
  (interactive)
  (let ((node vjump--current))
    (while (and (vjump-node-parent node)
                (vjump-node-marker (vjump-node-parent node))
                (= 1 (length (vjump-node-children (vjump-node-parent node)))))
      (setq node (vjump-node-parent node)))
    (unless (eq node vjump--current)
      (vjump--move-to-node node))))

(defun vjump-stem-end ()
  "Move forward to the tip of the current branch."
  (interactive)
  (let ((node vjump--current))
    (while (= 1 (length (vjump-node-children node)))
      (setq node (car (vjump-node-children node))))
    (unless (eq node vjump--current)
      (vjump--move-to-node node))))

(defun vjump-quit ()
  "Quit the vjump tree buffer.
If `vjump-roll-back-on-quit' is non-nil, return to the node that was
current when `vjump-visualize' was called."
  (interactive)
  (when (and vjump-roll-back-on-quit
             vjump--roll-back-to-this
             (not (eq vjump--roll-back-to-this vjump--current)))
    ;; Roll back: update current pointer and jump the window directly,
    ;; without triggering a full tree redraw that is immediately discarded.
    (setq vjump--current vjump--roll-back-to-this)
    (vjump--with-orig-window
     (vjump--jump-to-current)))
  (let ((orig-window vjump--orig-window))
    (kill-buffer-and-window)
    (when (window-live-p orig-window)
      (select-window orig-window))))

(defun vjump-confirm ()
  "Confirm the current position and close the vjump tree buffer."
  (interactive)
  (let ((orig-window vjump--orig-window))
    (kill-buffer-and-window)
    (when (window-live-p orig-window)
      (select-window orig-window))))

;;; Public entry point

;;;###autoload
(defun vjump-visualize ()
  "Open the *vjump-tree* buffer to browse and navigate the jump history.
Navigation keys (f/b/n/p/a/e) move through the tree and jump live.
Press RET to confirm the new position or q/C-g to roll back."
  (interactive)
  (unless vjump--root
    (user-error "No jump history yet"))
  (let ((orig-window (selected-window))
        (vjump-buf   (vjump--buffer)))
    (with-current-buffer vjump-buf
      (unless (derived-mode-p 'vjump-tree-mode)
        (vjump-tree-mode))
      (setq vjump--orig-window      orig-window
            vjump--roll-back-to-this vjump--current))
    (vjump--refresh-buffer)
    (select-window
     (display-buffer
      vjump-buf
      `(display-buffer-in-side-window
        (side          . ,vjump-window-side)
        (window-height . ,vjump-window-max-height))))
    (set-window-dedicated-p nil t)
    (let ((window-min-height 3))
      (fit-window-to-buffer nil vjump-window-max-height))
    (when (vjump-node-point vjump--current)
      (goto-char (vjump-node-point vjump--current)))))

;;; Global minor mode

;;;###autoload
(define-minor-mode vjump-mode
  "Global minor mode that records jump history as a tree.
When enabled, advises `push-mark' and `pop-to-mark-command', and
installs `pre-command-hook' / `post-command-hook' handlers to
detect large movements and buffer switches."
  :global t
  :group  'vjump
  (if vjump-mode
      (progn
        (advice-add 'push-mark          :before #'vjump--push-mark-advice)
        (advice-add 'pop-to-mark-command :after  #'vjump--pop-mark-advice)
        (add-hook 'pre-command-hook  #'vjump--pre-command)
        (add-hook 'post-command-hook #'vjump--post-command))
    (advice-remove 'push-mark          #'vjump--push-mark-advice)
    (advice-remove 'pop-to-mark-command #'vjump--pop-mark-advice)
    (remove-hook 'pre-command-hook  #'vjump--pre-command)
    (remove-hook 'post-command-hook #'vjump--post-command)))

(provide 'vjump)
;;; vjump.el ends here
