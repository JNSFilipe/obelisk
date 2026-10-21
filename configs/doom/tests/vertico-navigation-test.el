;;; vertico-navigation-test.el --- Minibuffer editing regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'vertico)
(load (expand-file-name "../vertico-navigation.el"
                        (file-name-directory load-file-name)) nil t)

(defmacro obelisk-vertico-test-with-input (input category base candidates &rest body)
  "Run BODY with INPUT and actual Vertico candidate state in a prompt buffer.
Only minibuffer primitives are replaced; candidate insertion uses Vertico."
  (declare (indent 4) (debug t))
  `(with-temp-buffer
     (insert (propertize "Prompt: " 'read-only t 'rear-nonsticky t))
     (let ((prompt-end (point)))
       (insert ,input)
       (let* ((vertico--base ,base)
              (vertico--candidates ,candidates)
              (vertico--total (length vertico--candidates))
              (vertico--index 0)
              (vertico--input (cons ,input (length ,input)))
              (vertico--metadata (list 'metadata (cons 'category ,category))))
         (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () prompt-end))
                   ((symbol-function 'minibuffer-contents-no-properties)
                    (lambda () (buffer-substring-no-properties prompt-end (point-max))))
                   ((symbol-function 'delete-minibuffer-contents)
                    (lambda () (delete-region prompt-end (point-max))))
                   ((symbol-function 'exit-minibuffer)
                    (lambda () (ert-fail "TAB must not exit the minibuffer"))))
           ,@body)))))

(ert-deftest obelisk-vertico-tab-completes-first-directory-not-whole-path ()
  (obelisk-vertico-test-with-input "~/pr" 'file "~/" '("projects/repo/main.el" "private/file")
    (setq vertico--index -1) ; Prompt selected: still insert the first candidate.
    (call-interactively (lookup-key vertico-map (kbd "TAB")))
    (should (equal (minibuffer-contents-no-properties) "~/projects/"))))

(ert-deftest obelisk-vertico-tab-completes-one-more-directory ()
  (obelisk-vertico-test-with-input "src/" 'project-file "" '("src/components/button.el")
    (obelisk-vertico-insert-level)
    (should (equal (minibuffer-contents-no-properties) "src/components/"))
    (obelisk-vertico-insert-level)
    (should (equal (minibuffer-contents-no-properties) "src/components/button.el"))))

(ert-deftest obelisk-vertico-tab-completes-file-without-exiting ()
  (obelisk-vertico-test-with-input "~/projects/re" 'file "~/projects/" '("readme.txt")
    (obelisk-vertico-insert-level)
    (should (equal (minibuffer-contents-no-properties) "~/projects/readme.txt"))))

(ert-deftest obelisk-vertico-tab-respects-highlighted-candidate ()
  (obelisk-vertico-test-with-input "~/p" 'file "~/" '("private/file" "projects/repo/file")
    (setq vertico--index 1)
    (obelisk-vertico-insert-level)
    (should (equal (minibuffer-contents-no-properties) "~/projects/"))))

(ert-deftest obelisk-vertico-tab-handles-abbreviated-parent-components ()
  (obelisk-vertico-test-with-input "d/p" 'file "" '("documents/projects/repo/file")
    (obelisk-vertico-insert-level)
    (should (equal (minibuffer-contents-no-properties) "documents/projects/"))))

(ert-deftest obelisk-vertico-tab-preserves-tramp-prefix-without-connecting ()
  (obelisk-vertico-test-with-input "/ssh:host:/home/u/pr" 'file "/ssh:host:/home/u/"
      '("projects/repo/file")
    (obelisk-vertico-insert-level)
    (should (equal (minibuffer-contents-no-properties) "/ssh:host:/home/u/projects/"))))

(ert-deftest obelisk-vertico-tab-handles-spaces-and-unicode ()
  (obelisk-vertico-test-with-input "~/c" 'file "~/" '("café projects/repo/file")
    (obelisk-vertico-insert-level)
    (should (equal (minibuffer-contents-no-properties) "~/café projects/"))))

(ert-deftest obelisk-vertico-tab-does-not-truncate-non-file-candidates ()
  (obelisk-vertico-test-with-input "proj" 'buffer "" '("projects/repo/buffer")
    (obelisk-vertico-insert-level)
    (should (equal (minibuffer-contents-no-properties) "projects/repo/buffer"))))

(ert-deftest obelisk-vertico-tab-with-no-candidates-preserves-input ()
  (obelisk-vertico-test-with-input "~/missing/path" 'file "~/" nil
    (obelisk-vertico-insert-level)
    (should (equal (minibuffer-contents-no-properties) "~/missing/path"))))

(ert-deftest obelisk-vertico-backspace-deletes-one-character-after-slash ()
  (obelisk-vertico-test-with-input "~/projects/" 'file "~/" nil
    (call-interactively (lookup-key vertico-map (kbd "DEL")))
    (should (equal (minibuffer-contents-no-properties) "~/projects"))))

(ert-deftest obelisk-vertico-gui-backspace-deletes-one-character ()
  (obelisk-vertico-test-with-input "~/café" 'file "~/" nil
    (call-interactively (lookup-key vertico-map [backspace]))
    (should (equal (minibuffer-contents-no-properties) "~/caf"))))

(ert-deftest obelisk-vertico-backspace-cannot-delete-the-prompt ()
  (obelisk-vertico-test-with-input "" 'file "" nil
    (should-error (call-interactively (lookup-key vertico-map (kbd "DEL")))
                  :type 'text-read-only)
    (should (equal (buffer-string) "Prompt: "))))

(ert-deftest obelisk-vertico-terminal-and-gui-tab-use-the-same-command ()
  (should (eq (lookup-key vertico-map (kbd "TAB")) #'obelisk-vertico-insert-level))
  (should (eq (lookup-key vertico-map [tab]) #'obelisk-vertico-insert-level)))

;;; vertico-navigation-test.el ends here
