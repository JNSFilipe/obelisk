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

;;; vjump-tests.el ends here
