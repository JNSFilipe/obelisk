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

(provide 'vjump)
;;; vjump.el ends here
