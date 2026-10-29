;;; sheprd-test.el --- Sheprd regression scenarios -*- lexical-binding: t; -*-
(require 'ert)
(defvar persp-mode nil)
(load (expand-file-name "../sheprd.el" (file-name-directory load-file-name)) nil t)

;; Never touch a real agent registry.
(setq sheprd-directory (file-name-as-directory (make-temp-file "sheprd-test-" t)))

(defmacro sheprd-test-registry (&rest body)
  "Run BODY against an empty registry and no runs."
  (declare (indent 0))
  `(let ((sheprd-directory (file-name-as-directory (make-temp-file "sheprd-registry-" t)))
         (sheprd--db nil) (sheprd--db-file nil)
         (sheprd--runs (make-hash-table :test #'equal))
         (sheprd--agent-timer nil))
     (unwind-protect (progn ,@body)
       (when (timerp sheprd--agent-timer) (cancel-timer sheprd--agent-timer))
       (when sheprd--db (sqlite-close sheprd--db))
       (delete-directory sheprd-directory t))))

(defun sheprd-test-agent (&rest slots)
  "Insert and return an agent with SLOTS over test defaults."
  (let ((agent (apply #'sheprd-agent--create
                      (append slots
                              (list :id (sheprd--uuid) :name "a1" :session "main"
                                    :directory temporary-file-directory
                                    :client 'claude :interface 'ghostel
                                    :conversation (sheprd--uuid) :status 'stopped
                                    :created "2026-10-29T00:00:00+0000")))))
    (sheprd--agent-insert agent)
    agent))

(defmacro sheprd-test-terminal (&rest body)
  `(with-temp-buffer
     (setq-local major-mode 'ghostel-mode)
     (setq-local ghostel--process nil)
     (setq-local ghostel--command-running nil)
     (cl-letf (((symbol-function 'sheprd--ghostel-live-p) (lambda () t))
               ((symbol-function 'sheprd--buffer-seen-p) (lambda (_) t)))
       ,@body)))


;;; --- Agent status -----------------------------------------------------------

(ert-deftest sheprd-status-newest-evidence ()
  (should (eq 'working (sheprd--agent-status
                        "Do you want to proceed?\nApproved\nWorking (esc to interrupt)")))
  (should (eq 'idle (sheprd--agent-status
                     "Working (esc to interrupt)\nDone\n› ")))
  (should (eq 'blocked (sheprd--agent-status
                        "Working (esc to interrupt)\nDo you want to proceed?")))
  (should (eq 'blocked (sheprd--agent-status "Which file?\n❯ ")))
  (should (eq 'idle (sheprd--agent-status "I approved the patch.\n› ")))
  (should (eq 'unknown (sheprd--agent-status "no recognizable agent output"))))

(ert-deftest sheprd-done-survives-until-the-pane-is-seen ()
  (with-temp-buffer
    (let ((seen nil))
      (cl-letf (((symbol-function 'sheprd--buffer-seen-p) (lambda (_) seen)))
        (should (eq 'working (sheprd--apply-seen-state (current-buffer) 'working)))
        ;; Finished while hidden: Herdr's `done', not `idle'.
        (should (eq 'done (sheprd--apply-seen-state (current-buffer) 'idle)))
        (should (eq 'done (sheprd--apply-seen-state (current-buffer) 'idle)))
        (setq seen t)
        (should (eq 'idle (sheprd--apply-seen-state (current-buffer) 'idle)))))))

(ert-deftest sheprd-done-is-not-raised-for-a-visible-pane ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'sheprd--buffer-seen-p) (lambda (_) t)))
      (should (eq 'working (sheprd--apply-seen-state (current-buffer) 'working)))
      (should (eq 'idle (sheprd--apply-seen-state (current-buffer) 'idle))))))

(ert-deftest sheprd-notifies-once-per-status-change ()
  (sheprd-test-terminal
   (let ((calls nil))
     (let ((sheprd-notify-statuses '(blocked done))
           (sheprd-notification-function
            (lambda (_buffer _session _client status) (push status calls))))
       (sheprd--maybe-notify (current-buffer) 'claude 'blocked)
       (sheprd--maybe-notify (current-buffer) 'claude 'blocked)
       (sheprd--maybe-notify (current-buffer) 'claude 'idle)
       (sheprd--maybe-notify (current-buffer) 'claude 'done)
       (should (equal '(done blocked) calls))))))


;;; --- Terminal snapshots -----------------------------------------------------

(ert-deftest sheprd-hidden-and-copy-snapshots ()
  (with-temp-buffer
    (insert "old")
    (setq-local ghostel--term 'terminal)
    (setq-local ghostel--last-output-time 1)
    (setq-local ghostel--pending-redraw t)
    (let ((output "working"))
      (cl-letf (((symbol-function 'ghostel--copy-all-text) (lambda (_) output)))
        (should (equal "working" (sheprd--terminal-text (current-buffer))))
        (setq output "waiting")
        (should (equal "waiting" (sheprd--terminal-text (current-buffer))))
        (setq-local ghostel--pending-redraw nil)
        (setq-local ghostel--input-mode 'copy)
        (should (equal "waiting" (sheprd--terminal-text (current-buffer))))))))

(ert-deftest sheprd-visible-cache-invalidation ()
  (with-temp-buffer
    (setq-local ghostel--last-output-time 1)
    (insert "old")
    (should (equal "old" (sheprd--terminal-text (current-buffer))))
    (erase-buffer) (insert "new")
    (should (equal "new" (sheprd--terminal-text (current-buffer))))))


;;; --- Agent identity ---------------------------------------------------------

(ert-deftest sheprd-direct-agent-without-shell-markers ()
  (sheprd-test-terminal
   (setq-local ghostel-identity '((kind . exec) (command . ("/bin/codex" "resume"))))
   (insert "Working (esc to interrupt)")
   (should (equal '(codex working) (sheprd--agent-info (current-buffer))))))

(ert-deftest sheprd-process-agent-without-shell-markers-revalidates ()
  (sheprd-test-terminal
   (insert "I approved the patch.\n› ")
   (let ((client 'claude))
     (cl-letf (((symbol-function 'sheprd--client-from-processes) (lambda () client)))
       (should (equal '(claude idle) (sheprd--agent-info (current-buffer))))
       (setq client nil sheprd--agent-probe-time nil)
       (should-not (sheprd--agent-info (current-buffer)))))))

(ert-deftest sheprd-does-not-detect-old-banners ()
  (sheprd-test-terminal
   (insert "Claude Code\n❯ grep codex log")
   (cl-letf (((symbol-function 'sheprd--client-from-processes) (lambda () nil)))
     (should-not (sheprd--agent-info (current-buffer))))))

(ert-deftest sheprd-process-executables-only-and-includes-root ()
  (with-temp-buffer
    (setq-local ghostel--pid 10)
    (let ((sheprd--process-table '((10 1 . "/bin/zsh") (11 10 . "grep codex log"))))
      (should-not (sheprd--client-from-processes)))
    (let ((sheprd--process-table '((10 1 . "/bin/claude --resume"))))
      (should (eq 'claude (sheprd--client-from-processes))))
    (setq default-directory "/ssh:example:/tmp/")
    (cl-letf (((symbol-function 'sheprd--ensure-process-table)
               (lambda () (ert-fail "Remote session inspected local processes"))))
      (should-not (sheprd--client-from-processes)))))

(ert-deftest sheprd-hook-never-reads-parser-mid-callback ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'sheprd--terminal-text)
               (lambda (_) (ert-fail "Snapshot during parser callback"))))
      (sheprd--ghostel-command-start (current-buffer))
      (should-not sheprd--agent-probe-time))))

(ert-deftest sheprd-manual-tracking-and-status ()
  (sheprd-test-terminal
   (sheprd-track-agent 'claude)
   (sheprd-set-status 'blocked)
   (should (equal '(claude blocked) (sheprd--agent-info (current-buffer))))
   (sheprd--ghostel-command-finish (current-buffer) 0)
   (should-not sheprd--status-override)
   (sheprd-track-agent 'auto)
   (should-not sheprd--tracked-client)))

(ert-deftest sheprd-manual-shell-agent-clears-on-command-exit ()
  (sheprd-test-terminal
   (setq ghostel--command-running t)
   (sheprd-track-agent 'claude)
   (sheprd--ghostel-command-finish (current-buffer) 0)
   (should-not sheprd--tracked-client)))


;;; --- Sessions ---------------------------------------------------------------

(ert-deftest sheprd-numeric-session-is-a-name-not-an-index ()
  (let (selected)
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () '("main" "2")))
              ((symbol-function '+workspace-current-name) (lambda () "main"))
              ((symbol-function '+workspace-switch) (lambda (name) (setq selected name)))
              ((symbol-function 'sheprd--save-current-tab) #'ignore)
              ((symbol-function 'sheprd--restore-tab) #'ignore)
              ((symbol-function 'sheprd--enforce-isolation) #'ignore))
      (sheprd-switch-session-index 2)
      (should (equal selected "2"))
      (should-error (sheprd-switch-session-index 3) :type 'user-error))))

(ert-deftest sheprd-session-index-is-one-based ()
  (cl-letf (((symbol-function '+workspace-list-names) (lambda () '("a" "b" "c"))))
    (should (equal 1 (sheprd-session-index "a")))
    (should (equal 3 (sheprd-session-index "c")))
    (should-not (sheprd-session-index "missing"))))

(ert-deftest sheprd-sessions-are-hermetically-sealed ()
  (let ((buffer (generate-new-buffer " *sealed-test*"))
        (removed nil))
    (unwind-protect
        (let ((persp-mode t))
          (with-current-buffer buffer (setq sheprd--session "owner"))
          ;; Perspectives are interned here so the owner is `eq' to itself,
          ;; the way real persp objects are.
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "owner"))
                    ((symbol-function '+workspace-get)
                     (lambda (name &optional _) (intern name)))
                    ((symbol-function 'persp-persps) (lambda (&rest _) '(owner other)))
                    ((symbol-function 'persp-contain-buffer-p) (lambda (&rest _) t))
                    ((symbol-function 'persp-remove-buffer)
                     (lambda (buf persp &rest _) (push (cons buf persp) removed))))
            (sheprd--confine-buffer buffer)
            ;; Removed from every foreign perspective, kept in its owner.
            (should (equal (list (cons buffer 'other)) removed))))
      (kill-buffer buffer))))

(ert-deftest sheprd-adopts-an-unowned-terminal-into-the-current-session ()
  (let ((buffer (generate-new-buffer " *adopt-test*")) added)
    (unwind-protect
        (let ((persp-mode t))
          (with-current-buffer buffer (setq major-mode 'ghostel-mode))
          (cl-letf (((symbol-function '+workspace-list-names) (lambda () '("main")))
                    ((symbol-function '+workspace-current-name) (lambda () "main"))
                    ((symbol-function '+workspace-get)
                     (lambda (name &optional _) (intern name)))
                    ((symbol-function 'persp-add-buffer)
                     (lambda (buf persp &rest _) (setq added (cons buf persp))))
                    ((symbol-function 'persp-persps) (lambda (&rest _) '(main)))
                    ((symbol-function 'persp-contain-buffer-p) (lambda (&rest _) nil)))
            (sheprd--enforce-isolation)
            (should (equal (cons buffer 'main) added))
            (should (equal "main" (buffer-local-value 'sheprd--session buffer)))))
      (kill-buffer buffer))))


;;; --- Panes ------------------------------------------------------------------

(ert-deftest sheprd-panes-exclude-side-windows ()
  (let ((window (selected-window)))
    (set-window-parameter window 'window-side 'left)
    (unwind-protect
        (should-not (sheprd--pane-p window))
      (set-window-parameter window 'window-side nil))
    (should (sheprd--pane-p window))))

(ert-deftest sheprd-close-pane-keeps-a-terminal-shown-elsewhere ()
  ;; Regression: closing one half of a split used to kill the shared terminal,
  ;; and prompt about it, even though another pane was still driving it.
  (let ((buffer (generate-new-buffer " *shared-pane-test*")) killed)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer buffer)
          (split-window-right)
          (cl-letf (((symbol-function 'sheprd--terminal-buffer-p) (lambda (_) t))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (ert-fail "Prompted for a shared terminal")))
                    ((symbol-function 'kill-buffer)
                     (lambda (&rest _) (setq killed t))))
            (sheprd-close-pane))
          (should-not killed))
      (kill-buffer buffer))))

(ert-deftest sheprd-focus-without-a-neighbour-errors ()
  (should-error (sheprd-focus-pane-left) :type 'user-error))


;;; --- Sidebar ----------------------------------------------------------------

(ert-deftest sheprd-preserves-adjacent-button-selection ()
  (with-temp-buffer
    (let ((render (lambda ()
                    (erase-buffer)
                    (dolist (name '("a" "b" "c"))
                      (insert-text-button name 'type 'sheprd-session-button
                                          'sheprd-session name)))))
      (funcall render)
      (goto-char 2)
      (let ((key (sheprd--button-key-at-point)))
        (funcall render)
        (goto-char (point-min))
        (sheprd--restore-button key)
        (should (equal '(session "b") (sheprd--button-key-at-point)))))))

(ert-deftest sheprd-session-rollup-counts-every-status ()
  (let ((entries (list (list :session "main" :status 'working)
                       (list :session "main" :status 'blocked)
                       (list :session "main" :status 'blocked)
                       (list :session "other" :status 'idle))))
    (let ((rollup (substring-no-properties (sheprd--session-rollup entries "main"))))
      (should (string-match-p "◍2" rollup))
      (should (string-match-p "●1" rollup))
      (should-not (string-match-p "○" rollup)))))

(ert-deftest sheprd-agent-entries-have-no-ungrouped-bucket ()
  (let ((buffer (generate-new-buffer " *entry-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'sheprd-session-names) (lambda () '("main")))
                  ((symbol-function 'sheprd--session-buffers) (lambda (&rest _) (list buffer)))
                  ((symbol-function 'sheprd--terminal-buffer-p) (lambda (_) t))
                  ((symbol-function 'sheprd--agent-info) (lambda (_) '(claude working))))
          (let ((entries (sheprd--agent-entries)))
            (should (= 1 (length entries)))
            (should (equal "main" (plist-get (car entries) :session)))
            (should (cl-every (lambda (entry) (plist-get entry :session)) entries))))
      (kill-buffer buffer))))


;;; --- Creating agents ---------------------------------------------------------

(ert-deftest sheprd-new-agent-passes-argv-and-directory ()
  (sheprd-test-registry
    (let ((sheprd-agent-commands '((codex . ("codex" "--arg=a; b")))))
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'executable-find) (lambda (&rest _) "/bin/codex"))
                ((symbol-function '+workspace-current-name) (lambda () "main"))
                ((symbol-function 'sheprd-session-names) (lambda () '("main")))
                ((symbol-function 'ghostel-exec)
                 (lambda (buffer program args)
                   (should (equal program "codex"))
                   (should (equal (car args) "--arg=a; b"))
                   ;; Codex reports through a per-run notify program.
                   (should (equal (nth 1 args) "-c"))
                   (should (string-prefix-p "notify=" (nth 2 args)))
                   (should (= 3 (length args)))
                   (with-current-buffer buffer
                     (should (equal default-directory temporary-file-directory))
                     (setq-local major-mode 'ghostel-mode)))))
        (save-window-excursion
          (let* ((id (sheprd-new-agent "c1" 'codex temporary-file-directory))
                 (buffer (sheprd--run-buffer id))
                 (agent (sheprd-agent id)))
            (unwind-protect
                (progn
                  (should (equal id (buffer-local-value 'sheprd--agent-id buffer)))
                  (should (eq 'codex (buffer-local-value 'sheprd--tracked-client buffer)))
                  ;; Codex has no startup signal: live once spawned, no ID yet.
                  (should (eq 'live (sheprd-agent-status agent)))
                  (should (sheprd-agent-launched agent))
                  (should-not (sheprd-agent-conversation agent)))
              (sheprd--end-run id)
              (kill-buffer buffer))))))))

(ert-deftest sheprd-new-agent-missing-executable-leaves-nothing ()
  (sheprd-test-registry
    (let ((before (buffer-list)))
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'ghostel-exec) (lambda (&rest _) (ert-fail "Launched")))
                ((symbol-function 'executable-find) (lambda (&rest _) nil))
                ((symbol-function '+workspace-current-name) (lambda () "main")))
        (should-error (sheprd-new-agent "c1" 'codex temporary-file-directory)
                      :type 'user-error))
      (should (equal before (buffer-list)))
      (should-not (sheprd-agents)))))

(ert-deftest sheprd-new-agent-claims-the-session-before-display ()
  (sheprd-test-registry
    (let ((persp-mode t) added)
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'executable-find) (lambda (&rest _) "/bin/codex"))
                ((symbol-function '+workspace-current-name) (lambda () "project"))
                ((symbol-function 'sheprd-session-names) (lambda () '("project")))
                ((symbol-function '+workspace-get) (lambda (&rest _) 'project-persp))
                ((symbol-function 'persp-persps) (lambda (&rest _) '(project-persp)))
                ((symbol-function 'persp-contain-buffer-p) (lambda (&rest _) nil))
                ((symbol-function 'ghostel-exec) (lambda (&rest _) nil))
                ((symbol-function 'persp-add-buffer)
                 (lambda (buffer persp switch &rest _)
                   (should (eq persp 'project-persp))
                   (should-not switch)
                   (setq added buffer)))
                ((symbol-function 'switch-to-buffer)
                 (lambda (buffer &rest _) (should (eq added buffer)))))
        (let* ((id (sheprd-new-agent "c1" 'codex temporary-file-directory))
               (buffer (sheprd--run-buffer id)))
          (should (equal "project" (buffer-local-value 'sheprd--session buffer)))
          (should (equal "project" (sheprd-agent-session (sheprd-agent id))))
          (sheprd--end-run id)
          (kill-buffer buffer))))))


;;; --- Registry and observation -----------------------------------------------

(ert-deftest sheprd-registry-round-trips-and-forgets-dead-runs ()
  (sheprd-test-registry
    (let* ((agent (sheprd-test-agent :name "fix auth" :client 'pi :status 'live
                                     :activity 'working :unread t :launched t))
           (id (sheprd-agent-id agent))
           (read (sheprd-agent id)))
      (should (equal "fix auth" (sheprd-agent-name read)))
      (should (eq 'pi (sheprd-agent-client read)))
      (should (eq 'ghostel (sheprd-agent-interface read)))
      (should (sheprd-agent-unread read))
      (should (sheprd-agent-launched read))
      (should (equal (list id) (mapcar #'sheprd-agent-id (sheprd-agents "main"))))
      (should-not (sheprd-agents "other"))
      ;; A new Emacs (connection) has no runs: live records become stopped.
      (sqlite-close sheprd--db)
      (setq sheprd--db nil)
      (let ((after (sheprd-agent id)))
        (should (eq 'stopped (sheprd-agent-status after)))
        (should-not (sheprd-agent-activity after))
        (should (eq 'stopped (sheprd--agent-state after)))))))

(ert-deftest sheprd-observe-saves-conversation-once-and-rejects-another ()
  (sheprd-test-registry
    (let* ((agent (sheprd-test-agent :client 'codex :conversation nil :status 'live))
           (id (sheprd-agent-id agent)))
      (puthash id (list :buffer nil) sheprd--runs)
      (sheprd--observe id :activity 'input :conversation "thread-1")
      (should (equal "thread-1" (sheprd-agent-conversation (sheprd-agent id))))
      (sheprd--observe id :conversation "thread-1")
      (should (eq 'live (sheprd-agent-status (sheprd-agent id))))
      (sheprd--observe id :conversation "thread-2")
      (let ((after (sheprd-agent id)))
        (should (eq 'failed (sheprd-agent-status after)))
        (should (equal "thread-1" (sheprd-agent-conversation after)))
        (should (string-match-p "different conversation" (sheprd-agent-error after))))
      (should-not (gethash id sheprd--runs)))))

(ert-deftest sheprd-observe-marks-unread-only-off-screen ()
  (sheprd-test-registry
    (let* ((id (sheprd-agent-id (sheprd-test-agent :status 'live :activity 'working)))
           (visible nil) notified)
      (puthash id (list :buffer nil) sheprd--runs)
      (cl-letf (((symbol-function 'sheprd--agent-visible-p) (lambda (_) visible))
                ((symbol-function 'sheprd--notify-agent)
                 (lambda (_agent status) (push status notified))))
        (setq visible t)
        (sheprd--observe id :activity 'input)
        (should-not (sheprd-agent-unread (sheprd-agent id)))
        (sheprd--observe id :activity 'working)
        (setq visible nil)
        (sheprd--observe id :activity 'input)
        (should (sheprd-agent-unread (sheprd-agent id)))
        (sheprd--observe id :activity 'approval)
        (should (equal '(blocked done) notified))))))

(ert-deftest sheprd-observe-ignores-agents-without-a-run ()
  (sheprd-test-registry
    (let ((id (sheprd-agent-id (sheprd-test-agent :status 'stopped))))
      (sheprd--observe id :status 'live :activity 'working)
      (should (eq 'stopped (sheprd-agent-status (sheprd-agent id)))))))

(ert-deftest sheprd-read-state-clears-unread-after-the-delay ()
  (sheprd-test-registry
    (let ((id (sheprd-agent-id (sheprd-test-agent :unread t)))
          (sheprd-read-delay 0)
          (sheprd--visible-since (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'sheprd--agent-visible-p) (lambda (_) nil)))
        (sheprd--update-read-state)
        (should (sheprd-agent-unread (sheprd-agent id))))
      (cl-letf (((symbol-function 'sheprd--agent-visible-p) (lambda (_) t)))
        (sheprd--update-read-state)
        (should-not (sheprd-agent-unread (sheprd-agent id)))))))


;;; --- Client reports ---------------------------------------------------------

(ert-deftest sheprd-claude-hook-events-map-to-states ()
  (sheprd-test-registry
    (let* ((agent (sheprd-test-agent :status 'starting))
           (id (sheprd-agent-id agent))
           (conversation (sheprd-agent-conversation agent)))
      (puthash id (list :buffer nil) sheprd--runs)
      (cl-letf (((symbol-function 'sheprd--agent-visible-p) (lambda (_) t)))
        (sheprd--claude-event id `((hook_event_name . "SessionStart")
                                   (session_id . ,conversation)))
        (should (eq 'input (sheprd--agent-state (sheprd-agent id))))
        (should (sheprd-agent-launched (sheprd-agent id)))
        (sheprd--claude-event id '((hook_event_name . "UserPromptSubmit")))
        (should (eq 'working (sheprd--agent-state (sheprd-agent id))))
        (sheprd--claude-event id '((hook_event_name . "Notification")
                                   (message . "Claude needs your permission to use Bash")))
        (should (eq 'approval (sheprd--agent-state (sheprd-agent id))))
        (sheprd--claude-event id '((hook_event_name . "PostToolUse")))
        (should (eq 'working (sheprd--agent-state (sheprd-agent id))))
        (sheprd--claude-event id '((hook_event_name . "Stop")))
        (should (eq 'input (sheprd--agent-state (sheprd-agent id))))
        (sheprd--claude-event id '((hook_event_name . "Notification")
                                   (message . "Claude is waiting for your input")))
        (should (eq 'input (sheprd--agent-state (sheprd-agent id))))))))

(ert-deftest sheprd-codex-notifications-capture-the-thread ()
  (sheprd-test-registry
    (let ((id (sheprd-agent-id (sheprd-test-agent :client 'codex :conversation nil
                                                  :status 'live :activity 'working))))
      (puthash id (list :buffer nil) sheprd--runs)
      (sheprd--codex-event id '((type . "approval-requested")))
      (should (eq 'approval (sheprd--agent-state (sheprd-agent id))))
      (sheprd--codex-event id '((type . "agent-turn-complete") (thread-id . "t-9")))
      (should (equal "t-9" (sheprd-agent-conversation (sheprd-agent id))))
      (should (eq 'input (sheprd--agent-state (sheprd-agent id)))))))

(ert-deftest sheprd-events-file-is-read-incrementally ()
  (sheprd-test-registry
    (let* ((id "agent") (files (sheprd--run-files id))
           (events (expand-file-name "events.jsonl" files))
           (run (list :events events :position 0)))
      (puthash id run sheprd--runs)
      (write-region "{\"a\":1}\n{\"b\":\"é\"}\n{\"c\":" nil events t 'silent)
      (should (equal '(((a . 1)) ((b . "é")))
                     (sheprd--read-new-events id (gethash id sheprd--runs))))
      ;; The partial third line is read once complete.
      (should-not (sheprd--read-new-events id (gethash id sheprd--runs)))
      (write-region "3}\n" nil events t 'silent)
      (should (equal '(((c . 3))) (sheprd--read-new-events id (gethash id sheprd--runs)))))))

(ert-deftest sheprd-client-arguments-name-and-resume-conversations ()
  (sheprd-test-registry
    (let ((files (sheprd--run-files "x")))
      (let ((claude (sheprd-agent--create :client 'claude :conversation "c-1")))
        (should (equal '("--session-id" "c-1")
                       (last (sheprd--client-arguments claude nil files) 2)))
        (should (equal '("--resume" "c-1")
                       (last (sheprd--client-arguments claude t files) 2)))
        (should (equal "--settings" (car (sheprd--client-arguments claude nil files)))))
      (let ((pi-agent (sheprd-agent--create :client 'pi :conversation "p-1")))
        (should (equal '("--session-id" "p-1") (sheprd--client-arguments pi-agent nil files)))
        (should (equal '("--session-id" "p-1") (sheprd--client-arguments pi-agent t files))))
      (let ((codex (sheprd-agent--create :client 'codex :conversation "t-1")))
        (should (equal '("resume" "t-1") (last (sheprd--client-arguments codex t files) 2)))
        (should (= 2 (length (sheprd--client-arguments codex nil files))))))))

(ert-deftest sheprd-claude-settings-hooks-append-to-the-events-file ()
  (sheprd-test-registry
    (let* ((files (sheprd--run-files "x"))
           (settings (json-parse-string
                      (with-temp-buffer
                        (insert-file-contents (sheprd--claude-settings files))
                        (buffer-string))
                      :object-type 'alist))
           (hooks (alist-get 'hooks settings))
           (command (alist-get 'command (aref (alist-get 'hooks (aref (alist-get 'Stop hooks) 0)) 0)))
           (events (expand-file-name "events.jsonl" files)))
      (dolist (event '(SessionStart UserPromptSubmit PreToolUse PostToolUse Notification Stop))
        (should (alist-get event hooks)))
      ;; The hook command really appends one line per (multi-line) payload.
      (with-temp-buffer
        (insert "{\"hook_event_name\":\n \"Stop\"}")
        (should (zerop (call-process-region (point-min) (point-max) "sh" nil nil nil "-c" command))))
      (should (equal '(((hook_event_name . "Stop")))
                     (sheprd--read-new-events "x" (list :events events :position 0)))))))

(ert-deftest sheprd-codex-notify-program-appends-its-argument ()
  (sheprd-test-registry
    (let* ((files (sheprd--run-files "x"))
           (override (sheprd--codex-notify files))
           (argv (append (json-parse-string (string-remove-prefix "notify=" override)
                                            :array-type 'list)
                         (list "{\"type\":\"agent-turn-complete\",\"thread-id\":\"t\"}"))))
      (should (zerop (apply #'call-process (car argv) nil nil nil (cdr argv))))
      (should (equal '(((type . "agent-turn-complete") (thread-id . "t")))
                     (sheprd--read-new-events
                      "x" (list :events (expand-file-name "events.jsonl" files) :position 0)))))))


;;; --- Sidebar and attention ---------------------------------------------------

(ert-deftest sheprd-rollup-counts-registered-agents ()
  (sheprd-test-registry
    (let ((agents (list (sheprd-test-agent :status 'live :activity 'approval)
                        (sheprd-test-agent :status 'live :activity 'working :unread t)
                        (sheprd-test-agent :status 'stopped :unread t)
                        (sheprd-test-agent :session "other" :status 'live :activity 'working))))
      (should (equal "◍1 ●1 *2"
                     (substring-no-properties (sheprd--session-rollup nil "main" agents)))))))

(ert-deftest sheprd-sidebar-lists-sessions-agents-and-closed-sessions ()
  (sheprd-test-registry
    (sheprd-test-agent :name "fixer" :status 'live :activity 'approval)
    (sheprd-test-agent :name "elsewhere" :session "gone")
    (cl-letf (((symbol-function 'sheprd-session-names) (lambda () '("main" "web")))
              ((symbol-function 'sheprd-current-session) (lambda () "main"))
              ((symbol-function 'sheprd--agent-entries) (lambda () nil)))
      (with-temp-buffer
        (let ((sheprd--toggled-sessions nil))
          (sheprd--render-sidebar)
          (let ((text (buffer-string)))
            (should (string-match-p "▾ 1 main" text))
            (should (string-match-p "▸ 2 web" text))
            (should (string-match-p "◍ fixer" text))
            (should (string-match-p "claude · approval" text))
            (should (string-match-p "CLOSED" text))
            (should (string-match-p "gone (1)" text))
            (should-not (string-match-p "elsewhere" text)))
          (goto-char (point-min))
          (search-forward "fixer")
          (should (sheprd--agent-at-point))
          (should (equal "main" (sheprd--session-at-point))))))))

(ert-deftest sheprd-attention-cycles-approvals-before-unread ()
  (sheprd-test-registry
    (let* ((unread (sheprd-test-agent :name "u" :status 'stopped :unread t))
           (approval (sheprd-test-agent :name "a" :status 'live :activity 'approval))
           opened)
      (sheprd-test-agent :name "busy" :status 'live :activity 'working)
      (should (equal (list (sheprd-agent-id approval) (sheprd-agent-id unread))
                     (mapcar #'sheprd-agent-id (sheprd--attention-agents))))
      (cl-letf (((symbol-function 'sheprd-open-agent)
                 (lambda (id) (setq opened id sheprd--attention-last id))))
        (let ((sheprd--attention-last nil))
          (sheprd-next-attention)
          (should (equal (sheprd-agent-id approval) opened))
          (sheprd-next-attention)
          (should (equal (sheprd-agent-id unread) opened))
          (sheprd-next-attention)
          (should (equal (sheprd-agent-id approval) opened)))))))

(ert-deftest sheprd-attention-only-selects-agents-needing-it ()
  (let ((buffer (generate-new-buffer " *attention-test*")) opened)
    (unwind-protect
        (cl-letf (((symbol-function 'sheprd--attention-agents) (lambda () nil))
                  ((symbol-function 'sheprd--agent-entries)
                   (lambda () (list (list :session "2" :buffer buffer :status 'blocked)
                                    (list :session "main" :buffer buffer :status 'working))))
                  ((symbol-function 'completing-read)
                   (lambda (_ choices &rest _)
                     (should (= 1 (length choices))) (caar choices)))
                  ((symbol-function 'sheprd--show-pane-buffer)
                   (lambda (session _buffer) (setq opened session))))
          (sheprd-attention)
          (should (equal "2" opened)))
      (kill-buffer buffer))))


;;; --- Keymaps and lifecycle --------------------------------------------------

(ert-deftest sheprd-prefix-map-numbers-select-sessions ()
  (let (selected)
    (cl-letf (((symbol-function 'sheprd-switch-session-index)
               (lambda (index) (setq selected index))))
      (call-interactively (lookup-key sheprd-command-map (kbd "3")))
      (should (equal 3 selected)))))

(ert-deftest sheprd-prefix-map-covers-the-herdr-actions ()
  (dolist (binding '(("v" . sheprd-split-right)
                     ("-" . sheprd-split-down)
                     ("h" . sheprd-focus-pane-left)
                     ("l" . sheprd-focus-pane-right)
                     ("H" . sheprd-swap-pane-left)
                     ("z" . sheprd-zoom)
                     ("x" . sheprd-close-pane)
                     ("r" . sheprd-resize-mode)
                     ("[" . sheprd-copy-mode)
                     ("e" . sheprd-edit-scrollback)
                     ("n" . sheprd-next-session)
                     ("p" . sheprd-previous-session)
                     ("w" . sheprd-switch-session)
                     ("B" . sheprd-switch-buffer)
                     ("N" . sheprd-new-session)
                     ("W" . sheprd-rename-session)
                     ("D" . sheprd-kill-session)
                     ("g" . sheprd-goto)
                     ("b" . sheprd-toggle-sidebar)
                     ("o" . sheprd-open-notification-target)
                     ("q" . sheprd-detach)
                     ("?" . sheprd-help)))
    (should (eq (cdr binding) (lookup-key sheprd-command-map (kbd (car binding)))))))

(ert-deftest sheprd-prefix-map-reaches-agents ()
  (dolist (binding '(("a" . sheprd-new-agent)
                     ("A" . sheprd-open-agent)
                     ("!" . sheprd-next-attention)
                     ("@" . sheprd-attention)
                     ("O" . sheprd-board)
                     ("S" . sheprd-stop-agent)
                     ("Z" . sheprd-focus-agent)))
    (should (eq (cdr binding) (lookup-key sheprd-command-map (kbd (car binding))))))
  (dolist (binding '(("n" . sheprd-new-agent)
                     ("x" . sheprd-stop-agent)
                     ("D" . sheprd-sidebar-delete)
                     ("]" . sheprd-next-attention)
                     ("TAB" . sheprd-toggle-session-fold)
                     ("RET" . push-button)))
    (should (eq (cdr binding) (lookup-key sheprd-sidebar-mode-map (kbd (car binding)))))))

(ert-deftest sheprd-agent-map-has-machina-keys ()
  (dolist (binding '(("a" . sheprd-focus-sidebar)
                     ("n" . sheprd-new-agent)
                     ("b" . sheprd-board)
                     ("d" . sheprd-open-agent)
                     ("z" . sheprd-focus-agent)
                     ("]" . sheprd-next-attention)
                     ("[" . sheprd-previous-attention)))
    (let ((command (lookup-key sheprd-agent-map (kbd (car binding))))
          (entry (cdr (assq (aref (kbd (car binding)) 0) (cdr sheprd-agent-map)))))
      (should (eq (cdr binding) command))
      (should (commandp command))
      ;; Labelled for which-key: (LABEL . COMMAND).
      (should (stringp (car-safe entry))))))

(ert-deftest sheprd-prefix-map-has-no-tab-actions ()
  ;; Tabs were removed on purpose; `sheprd-switch-buffer' covers moving around a
  ;; session.  M-digits in particular must not linger as dead keys.
  (dolist (key '("M-1" "M-5" "M-9" "T" "X" "{" "}"))
    (should-not (lookup-key sheprd-command-map (kbd key))))
  (should-not (fboundp 'sheprd-new-tab))
  (should-not (fboundp 'sheprd--tabs)))

(ert-deftest sheprd-switch-buffer-is-perspective-scoped ()
  (let (called)
    (cl-letf (((symbol-function 'persp-switch-to-buffer)
               (lambda (&rest _) (interactive) (setq called t))))
      (sheprd-switch-buffer)
      (should called))))

(ert-deftest sheprd-switch-buffer-without-persp-mode-errors ()
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) (not (eq sym 'persp-switch-to-buffer)))))
    (should-error (sheprd-switch-buffer) :type 'user-error)))

(ert-deftest sheprd-installs-the-global-prefix ()
  (let ((sheprd-prefix-key "C-c s") (sheprd-leader-keys nil))
    (sheprd-install-keys)
    (should (eq sheprd-command-map (lookup-key global-map (kbd "C-c s"))))))

(ert-deftest sheprd-enable-disable-cleans-hooks-and-timers ()
  (let ((sheprd--refresh-timer nil) (sheprd--scheduled-refresh nil)
        (sheprd-mode t))
    (unwind-protect
        (progn
          (sheprd--enable)
          (let ((first sheprd--refresh-timer))
            (sheprd--enable)
            (should-not (memq first timer-list)))
          (sheprd--schedule-refresh)
          (should (timerp sheprd--scheduled-refresh)))
      (sheprd--disable))
    (should-not sheprd--refresh-timer)
    (should-not sheprd--scheduled-refresh)
    (should-not (memq #'sheprd--ghostel-command-start ghostel-command-start-functions))
    (should-not (memq #'sheprd--window-selection-change
                      window-selection-change-functions))))

(ert-deftest sheprd-daemon-frame-initializes ()
  (let ((sheprd-mode nil) (persp-mode t) enabled refreshed)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'sheprd-mode) (lambda (_) (setq enabled t)))
              ((symbol-function 'sheprd-refresh) (lambda () (setq refreshed t))))
      (sheprd--after-make-frame (selected-frame))
      (should enabled) (should refreshed))))

(ert-deftest sheprd-detach-refuses-when-there-is-nothing-to-detach-from ()
  (cl-letf (((symbol-function 'daemonp) (lambda () nil))
            ((symbol-function 'visible-frame-list) (lambda () (list (selected-frame)))))
    (should-error (sheprd-detach) :type 'user-error)))
