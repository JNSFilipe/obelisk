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
       (should (eq vjump--current (cadr (vjump-node-children node-a))))
       (should (eq node-a
                   (vjump-node-parent (cadr (vjump-node-children node-a)))))))))

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

;;; vjump-tests.el ends here
