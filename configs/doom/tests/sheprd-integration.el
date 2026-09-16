;;; sheprd-integration.el --- Installed Doom/Ghostel smoke check -*- lexical-binding: t; -*-
;; Load in the configured Emacs daemon, then call sheprd-integration-check.
;; Creates and removes an isolated hidden GUI frame and a harmless shell process.
(require 'cl-lib)
(require 'ert)
(defvar sheprd-integration-source
  (expand-file-name "../sheprd.el" (file-name-directory load-file-name)))

(defun sheprd-integration-check ()
  ;; Do not initialize macOS GUI machinery from a terminal-only daemon.
  (unless (cl-find-if #'display-graphic-p (frame-list))
    (user-error "Open a healthy GUI client before running this check"))
  (require 'ghostel)
  (load sheprd-integration-source nil t)
  ;; This runs inside a live daemon on a hidden frame.  A prompt here would be
  ;; invisible and would wedge the whole Emacs, so answer everything for it.
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
            ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
   (let ((was-enabled sheprd-mode)
        (ghostel-query-before-killing nil)
        ;; The fixture must outlive the whole check and must not vanish when it
        ;; finally exits: a buffer Ghostel reaps mid-run fails later assertions
        ;; for reasons that have nothing to do with Sheprd.
        (ghostel-kill-buffer-on-exit nil)
        (origin-frame (selected-frame))
        (session-name (format "sheprd-test-%s" (float-time)))
        (other-name (format "sheprd-other-%s" (float-time)))
        ;; Doom associates a fresh "#N" workspace with every new frame, so the
        ;; check has to sweep up whatever its own frame caused as well.
        (sessions-before (+workspace-list-names))
        frame buffer created-sessions)
    (unwind-protect
        (progn
          (setq frame (make-frame `((name . "Sheprd integration")
                                    (window-system . ,(cond ((featurep 'mac) 'mac)
                                                            ((featurep 'ns) 'ns)
                                                            (t window-system)))
                                    (visibility . nil))))
          (with-selected-frame frame
            (+workspace-new session-name)
            (push session-name created-sessions)
            (+workspace-new other-name)
            (push other-name created-sessions)
            (sheprd--switch-session session-name)
            (let ((sheprd-agent-commands
                   '((codex . ("/bin/sh" "-c"
                               "printf 'Working (esc to interrupt)\n'; sleep 1; printf 'Do you want to proceed?\n'; sleep 600")))))
              (setq buffer (sheprd-launch-agent 'codex temporary-file-directory)))
            (let ((deadline (+ (float-time) 5)) info)
              (while (and (< (float-time) deadline)
                          (not (equal info '(codex blocked))))
                (accept-process-output nil 0.1)
                (setq info (sheprd--agent-info buffer)))
              (should (equal info '(codex blocked))))
            ;; The launched terminal is owned by exactly one session.
            (should (equal session-name (buffer-local-value 'sheprd--session buffer)))
            (sheprd--display)
            (should (window-live-p (sheprd--sidebar-window)))
            (should-not (eq (sheprd--frame-sidebar)
                            (with-selected-frame origin-frame
                              (sheprd--frame-sidebar))))
            (let ((entry (cl-find buffer (sheprd--agent-entries)
                                  :key (lambda (e) (plist-get e :buffer)))))
              (should entry)
              (should (equal (plist-get entry :session) (sheprd-current-session))))
            ;; Buffer switching stays inside the session.
            (should (memq buffer (sheprd--session-buffers session-name)))
            ;; Hide the terminal in another session, then activate its real
            ;; sidebar button and verify that both context and buffer follow.
            (sheprd--switch-session other-name)
            (should-not (memq buffer (sheprd--session-buffers other-name)))
            (sheprd--display)
            (with-current-buffer (sheprd--frame-sidebar)
              (let ((button (next-button (point-min) t)))
                (while (and button (not (eq buffer (button-get button 'sheprd-buffer))))
                  (setq button (next-button (button-end button) t)))
                (should button)
                (sheprd--activate-agent-button button)))
            (should (equal session-name (sheprd-current-session)))
            (should (eq buffer (window-buffer (selected-window))))
            ;; Splitting adds a pane without disturbing the sidebar.
            (let ((sheprd-split-spawns-terminal nil))
              (sheprd-split-right)
              (should (= 2 (length (sheprd--panes))))
              (sheprd-close-pane)
              (should (= 1 (length (sheprd--panes))))
              ;; Closing one half of a split must not kill a shared terminal.
              (should (buffer-live-p buffer)))
            ;; The side window must leave a normal main window available.
            (should-not (window-parameter (sheprd--main-window) 'window-side))
            (with-current-buffer buffer
              (delete-process ghostel--process))
            (accept-process-output nil 0.1)
            (should-not (sheprd--agent-info buffer))
            'passed))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (process-live-p ghostel--process) (delete-process ghostel--process)))
        (kill-buffer buffer))
      (when (frame-live-p frame) (delete-frame frame t))
      (dolist (name created-sessions)
        (when (+workspace-exists-p name) (+workspace-kill name t)))
      (dolist (name (cl-set-difference (+workspace-list-names) sessions-before
                                       :test #'equal))
        (when (and (+workspace-exists-p name)
                   (not (equal name (+workspace-current-name)))
                   (null (+workspace-buffer-list (+workspace-get name t))))
          (+workspace-kill name t)))
      (unless was-enabled (sheprd-mode -1))))))

(defun sheprd-ghostel-integration-check ()
  "Test the installed Ghostel native terminal without creating a GUI frame."
  (require 'ghostel)
  (load sheprd-integration-source nil t)
  (let ((buffer (generate-new-buffer " *sheprd-native-test*"))
        (ghostel-query-before-killing nil)
        (ghostel-kill-buffer-on-exit nil))
    (unwind-protect
        (progn
          (ghostel-exec buffer "/bin/sh"
                        '("-c" "printf 'Working (esc to interrupt)\n'; sleep 1; printf 'Do you want to proceed?\n'; sleep 600"))
          (with-current-buffer buffer (setq sheprd--tracked-client 'codex))
          (let ((deadline (+ (float-time) 5)) info)
            (while (and (< (float-time) deadline)
                        (not (equal info '(codex blocked))))
              (accept-process-output nil 0.1)
              (setq info (sheprd--agent-info buffer)))
            (should (equal info '(codex blocked))))
          (with-current-buffer buffer (delete-process ghostel--process))
          (accept-process-output nil 0.1)
          (should-not (sheprd--agent-info buffer))
          'passed)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (process-live-p ghostel--process) (delete-process ghostel--process)))
        (kill-buffer buffer)))))
