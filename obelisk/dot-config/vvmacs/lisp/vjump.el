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
  children  ; list of child vjump-nodes, newest last
  point     ; integer: text position of this node's glyph in *vjump-tree*
  timestamp) ; float-time, for display

(provide 'vjump)
;;; vjump.el ends here
