;;; vjump-tests.el --- ERT tests for vjump.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'vjump)

(ert-deftest vjump-node-creation ()
  "Basic node creation and field access."
  (let ((node (make-vjump-node :marker nil :parent nil :children nil
                               :point nil :timestamp 1.0)))
    (should (vjump-node-p node))
    (should (null (vjump-node-marker node)))
    (should (null (vjump-node-parent node)))
    (should (null (vjump-node-children node)))
    (should (null (vjump-node-point node)))
    (should (equal 1.0 (vjump-node-timestamp node)))))

(ert-deftest vjump-node-setf ()
  "Fields can be updated with setf."
  (let ((node (make-vjump-node)))
    (setf (vjump-node-point node) 42)
    (should (= 42 (vjump-node-point node)))))

(defmacro vjump--with-clean-state (&rest body)
  "Run BODY with a fresh, empty jump tree."
  `(let ((vjump--root nil)
         (vjump--current nil))
     ,@body))

(ert-deftest vjump--push-initialises-tree ()
  "First push creates root sentinel and one child."
  (vjump--with-clean-state
   (with-temp-buffer
     (vjump--push (point) (current-buffer))
     (should (vjump-node-p vjump--root))
     (should (null (vjump-node-marker vjump--root)))   ; sentinel has no marker
     (should (= 1 (length (vjump-node-children vjump--root))))
     (should (eq vjump--current
                 (car (vjump-node-children vjump--root)))))))

(ert-deftest vjump--push-advances-current ()
  "Each push moves vjump--current to the new child."
  (vjump--with-clean-state
   (with-temp-buffer
     (vjump--push (point) (current-buffer))
     (let ((first vjump--current))
       (vjump--push (point) (current-buffer))
       (should (not (eq vjump--current first)))
       (should (eq (vjump-node-parent vjump--current) first))))))

(ert-deftest vjump--push-creates-branch ()
  "Pushing after going back creates a sibling branch."
  (vjump--with-clean-state
   (with-temp-buffer
     (vjump--push (point) (current-buffer)) ; root -> A
     (let ((node-a vjump--current))
       (vjump--push (point) (current-buffer)) ; A -> B
       ;; Simulate user having navigated back to A
       (setq vjump--current node-a)
       (vjump--push (point) (current-buffer)) ; A -> C (new branch)
       (should (= 2 (length (vjump-node-children node-a))))
       (should (eq vjump--current (car (vjump-node-children node-a))))
       (should (eq node-a
                   (vjump-node-parent (car (vjump-node-children node-a)))))))))

(ert-deftest vjump-go-back-moves-to-parent ()
  "vjump-go-back moves vjump--current to parent and jumps there."
  (vjump--with-clean-state
   (with-temp-buffer
     (insert "line1\nline2\n")
     (goto-char (point-min))
     (vjump--push (point) (current-buffer))    ; root -> A (line 1)
     (let ((node-a vjump--current))
       (forward-line 1)
       (vjump--push (point) (current-buffer))  ; A -> B (line 2)
       (vjump-go-back)
       (should (eq vjump--current node-a))
       (should (= (marker-position (vjump-node-marker node-a)) (point)))))))

(ert-deftest vjump-go-back-stops-at-sentinel-child ()
  "vjump-go-back does not move past the first real node."
  (vjump--with-clean-state
   (with-temp-buffer
     (vjump--push (point) (current-buffer))   ; root -> A
     (let ((node-a vjump--current))
       (vjump-go-back)
       ;; Still at A — parent of A is the sentinel root (no marker)
       (should (eq vjump--current node-a))))))

(ert-deftest vjump-go-forward-moves-to-first-child ()
  "vjump-go-forward moves vjump--current to the first child."
  (vjump--with-clean-state
   (with-temp-buffer
     (vjump--push (point) (current-buffer))   ; root -> A
     (let ((node-a vjump--current))
       (vjump--push (point) (current-buffer)) ; A -> B
       (let ((node-b vjump--current))
         (setq vjump--current node-a)
         (vjump-go-forward)
         (should (eq vjump--current node-b)))))))

(ert-deftest vjump-go-forward-noop-at-leaf ()
  "vjump-go-forward does nothing when current has no children."
  (vjump--with-clean-state
   (with-temp-buffer
     (vjump--push (point) (current-buffer))   ; root -> A (leaf)
     (let ((node-a vjump--current))
       (vjump-go-forward)
       (should (eq vjump--current node-a))))))

(ert-deftest vjump--post-command-dedup-flag ()
  "When vjump--push-mark-called is t, post-command resets it without pushing."
  (vjump--with-clean-state
   (with-temp-buffer
     (let ((vjump--push-mark-called t)
           (vjump--pre-command-buffer (current-buffer))
           (vjump--pre-command-point (point)))
       (vjump--post-command)
       (should (null vjump--push-mark-called))
       ;; No push happened
       (should (null vjump--root))))))

(ert-deftest vjump--post-command-buffer-change ()
  "Post-command records a jump when buffer changes."
  (vjump--with-clean-state
   (let* ((buf-a (get-buffer-create " *vjump-test-a*"))
          (buf-b (get-buffer-create " *vjump-test-b*"))
          (vjump--push-mark-called nil)
          (vjump--pre-command-buffer buf-a)
          (vjump--pre-command-point 1))
     (unwind-protect
         (with-current-buffer buf-b
           (vjump--post-command)
           (should (not (null vjump--root)))
           (should (= 1 (length (vjump-node-children vjump--root)))))
       (kill-buffer buf-a)
       (kill-buffer buf-b)))))

(ert-deftest vjump--post-command-distance ()
  "Post-command records a jump when point moves more than threshold lines."
  (vjump--with-clean-state
   (with-temp-buffer
     ;; Insert enough lines to exceed the threshold
     (dotimes (_ (+ vjump-distance-threshold 5))
       (insert "x\n"))
     (goto-char (point-min))
     (let ((vjump--push-mark-called nil)
           (vjump--pre-command-buffer (current-buffer))
           (vjump--pre-command-point (point-max)))
       (vjump--post-command)
       (should (not (null vjump--root)))))))

(ert-deftest vjump--translate-unicode ()
  "vjump--translate converts glyphs via glyph alist."
  (should (stringp (vjump--translate "○")))
  (should (stringp (vjump--translate "●──○")))
  ;; Result should be non-empty
  (should (> (length (vjump--translate "○")) 0)))

(ert-deftest vjump--draw-tree-single-node ()
  "Drawing a tree with only the sentinel root and one child produces output."
  (vjump--with-clean-state
   (with-temp-buffer
     (vjump--push 1 (current-buffer))  ; root -> A
     (with-temp-buffer
       (vjump--draw-tree vjump--root)
       (should (> (buffer-size) 0))))))

(ert-deftest vjump--node-label-live-buffer ()
  "vjump--node-label returns buffer:line for a live marker."
  (with-temp-buffer
    (insert "line1\nline2\n")
    (goto-char (point-min))
    (let* ((marker (copy-marker (point)))
           (node (make-vjump-node :marker marker)))
      (should (string-match-p ":[0-9]+" (vjump--node-label node))))))

;;; vjump-tests.el ends here
