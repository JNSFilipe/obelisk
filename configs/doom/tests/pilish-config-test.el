;;; pilish-config-test.el --- Pi frontend configuration checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defconst obelisk-pilish-test-directory
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name))))

(defun obelisk-pilish-test-forms (file)
  "Read top-level forms from FILE without loading the configuration."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file obelisk-pilish-test-directory))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun obelisk-pilish-test-declaration (macro file package)
  (cl-find-if (lambda (form)
                (and (listp form) (eq (car form) macro) (eq (cadr form) package)))
              (obelisk-pilish-test-forms file)))

(defvar pilish-executable)
(defvar pilish-project-trust-policy)
(defvar obelisk-pilish-test-bindings)

(ert-deftest obelisk-pilish-replaces-agent-shell ()
  (should (equal
           (plist-get (cddr (obelisk-pilish-test-declaration 'package! "packages.el" 'pilish))
                      :recipe)
           '(:host github :repo "dnouri/pilish")))
  (should-not (obelisk-pilish-test-declaration 'package! "packages.el" 'agent-shell))
  (should-not (obelisk-pilish-test-declaration 'use-package! "config.el" 'agent-shell)))

(ert-deftest obelisk-pilish-initialization-and-bindings ()
  (let* ((form (obelisk-pilish-test-declaration 'use-package! "config.el" 'pilish))
         (tail (cdr (memq :init form)))
         (pilish-executable nil)
         (pilish-project-trust-policy nil)
         obelisk-pilish-test-bindings)
    (should (equal (plist-get (cddr form) :commands)
                   '(pilish pilish-toggle pilish-session-browser)))
    ;; Capture Doom's binding declaration, without loading the rest of Doom or
    ;; launching Pi. Evaluate the real :init forms rather than a test copy.
    (cl-letf (((symbol-function 'map!)
               (cons 'macro
                     (lambda (&rest bindings)
                       `(setq obelisk-pilish-test-bindings ',bindings)))))
      (while (and tail (not (keywordp (car tail))))
        (eval (pop tail) t)))
    (should (equal pilish-executable '("/opt/homebrew/bin/pi")))
    (should (eq pilish-project-trust-policy 'default))
    (should
     (equal obelisk-pilish-test-bindings
            '(:leader
              (:prefix ("a" . "agents")
               :desc "Pi session" "p" #'pilish
               :desc "Toggle Pi session" "t" #'pilish-toggle
               :desc "Pi sessions" "s" #'pilish-session-browser))))))

(ert-deftest obelisk-pilish-upstream-commands-available ()
  ;; Test against the libraries installed by make doom-sync. No session starts.
  (require 'pilish)
  (dolist (command '(pilish pilish-toggle pilish-session-browser))
    (should (commandp command)))
  (should (treesit-available-p))
  (should (file-executable-p "/opt/homebrew/bin/pi")))

(ert-deftest obelisk-pilish-homebrew-rpc-handshake ()
  (require 'pilish)
  (let* ((root (make-temp-file "obelisk-pilish-" t))
         (process-environment (copy-sequence process-environment))
         (pilish-executable '("/opt/homebrew/bin/pi"))
         (pilish-project-trust-policy 'no-approve)
         (pilish-extra-args '("--offline" "--no-extensions" "--no-skills"))
         process)
    ;; Only inspect state: no prompt, provider request, live auth or extension
    ;; code. All writable state belongs to this disposable agent directory.
    (setenv "HOME" root)
    (setenv "PI_CODING_AGENT_DIR" (expand-file-name ".pi/agent" root))
    (setenv "PI_CODING_AGENT_SESSION_DIR" (expand-file-name "sessions" root))
    (setenv "PI_OFFLINE" "1")
    (setenv "PI_SKIP_VERSION_CHECK" "1")
    (unwind-protect
        (progn
          (setq process (pilish--start-process root))
          (let ((response (pilish--rpc-sync process '(:type "get_state") 20)))
            (should (eq (plist-get response :success) t))))
      (when (processp process)
        (when (process-live-p process) (delete-process process))
        (when-let* ((stderr (process-get process 'pilish-stderr-buf))
                    ((buffer-live-p stderr)))
          (kill-buffer stderr)))
      (delete-directory root t))))

;;; pilish-config-test.el ends here
