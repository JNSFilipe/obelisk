;;; machina-pilish.el --- Pilish as an M-x Machina interface -*- lexical-binding: t; -*-
;; Loaded by machina.el.  Pi agents created with the `pilish' interface run in
;; Pilish (Pi's own Emacs UI over Pi RPC) while Machina keeps their status,
;; focus, stop and resume.  Neither package has an extension point for this:
;; Machina hard-codes its interfaces and Pilish has no embedding API.  The two
;; are joined through advice on internal functions of both (Machina 0.1.0,
;; Pilish 3.4.0); tests/config-test.el pins every one of them, so an upgrade
;; that moves them fails loudly instead of misbehaving.
;;
;; Mapping:
;;   Machina conversation ID   Pi session ID, passed as --session-id.  New
;;                             agents get a fresh UUID; resume passes the same
;;                             ID, and Pilish renders the saved history.  Pi
;;                             writes a session file only after the first
;;                             message, so an agent never prompted resumes
;;                             empty under the same ID (reported, not hidden).
;;   status starting/live      until/after Pi's get_state reports that ID
;;   activity                  working while Pilish is sending, streaming or
;;                             compacting; approval while a Pi extension
;;                             dialog is open; otherwise input
;;   events                    prompt (user message), message/reply-chunk
;;                             (text deltas), turn-ended (agent_end), metadata
;;   stop                      ends the Pi process; buffers remain
;; Agent-to-agent messaging is not supported for Pilish agents: their
;; messages stay queued with a reason.

(require 'map)
(require 'seq)
(require 'subr-x)
(gloom-require 'pilish)

;; As in Doom: keep Pi's saved project trust decisions instead of Pilish's
;; default of approving every project.
(setq pilish-project-trust-policy 'default)

(defvar gloom-machina-pilish-profile
  '((:identifier . pi-pilish) (:mode-line-name . "Pi") (:interface . pilish))
  "Machina profile for Pi in Pilish.
Its agent label matches agent-shell's Pi profile, so creating a Pi agent asks
for the interface: agent-shell (pi-acp) or pilish.")

(defvar gloom-machina-pilish-input-height 8
  "Lines of the Pilish input window below a Machina conversation.")

(defvar gloom-machina-pilish--transports (make-hash-table :test #'eq :weakness 'key)
  "Machina transports whose interface is Pilish.")

(defvar-local gloom-machina-pilish--transport nil
  "Machina transport of this Pilish chat buffer.")
(defvar-local gloom-machina-pilish--resumed nil
  "Non-nil when this run resumes a saved Machina conversation.")
(defvar-local gloom-machina-pilish--asking nil
  "Non-nil while a Pi extension dialog waits for an answer.")
(defvar-local gloom-machina-pilish--activity nil
  "Last activity reported to Machina, to report changes only.")

(defun gloom-machina-pilish--transport-p (transport)
  "Return non-nil when TRANSPORT runs Pilish."
  (gethash transport gloom-machina-pilish--transports))

(defun gloom-machina-pilish--profile-p (profile)
  "Return non-nil when PROFILE (a string) is the Pilish profile."
  (equal profile (symbol-name (alist-get :identifier gloom-machina-pilish-profile))))

(defun gloom-machina-pilish--uuid ()
  "Return a random (version 4) UUID string."
  (let ((bytes (mapcar (lambda (_) (random 256)) (make-list 16 0))))
    (setf (nth 6 bytes) (logior #x40 (logand (nth 6 bytes) #x0f))
          (nth 8 bytes) (logior #x80 (logand (nth 8 bytes) #x3f)))
    (apply #'format (concat "%02x%02x%02x%02x-%02x%02x-%02x%02x-"
                            "%02x%02x-%02x%02x%02x%02x%02x%02x")
           bytes)))


;;; Machina dispatch

(defun gloom-machina-pilish--configs (configs)
  "Add the Pilish profile to Machina's CONFIGS while Pi is installed."
  (if (executable-find (car pilish-executable))
      (append configs (list (copy-tree gloom-machina-pilish-profile)))
    configs))
(advice-add 'mx-machina-backend-configs :filter-return #'gloom-machina-pilish--configs)

(defun gloom-machina-pilish--start (start profile directory conversation callback)
  "Start PROFILE through Pilish, else call Machina's START with all arguments."
  (if (gloom-machina-pilish--profile-p profile)
      (gloom-machina-pilish-start directory conversation callback)
    (funcall start profile directory conversation callback)))
(advice-add 'mx-machina-backend-start :around #'gloom-machina-pilish--start)

(defun gloom-machina-pilish--process (process transport)
  "Return Pilish TRANSPORT's Pi process, else call Machina's PROCESS."
  (if (gloom-machina-pilish--transport-p transport)
      (let ((buffer (mx-machina-transport-buffer transport)))
        (and (buffer-live-p buffer) (buffer-local-value 'pilish--process buffer)))
    (funcall process transport)))
(advice-add 'mx-machina-backend-process :around #'gloom-machina-pilish--process)

(defun gloom-machina-pilish--stop (stop transport)
  "End Pilish TRANSPORT's Pi process, keeping its buffers; else call STOP."
  (if (not (gloom-machina-pilish--transport-p transport))
      (funcall stop transport)
    (setf (mx-machina-transport-stopping transport) t)
    (when-let* ((process (gloom-machina-pilish--process #'ignore transport))
                ((process-live-p process)))
      (delete-process process))))
(advice-add 'mx-machina-backend-stop :around #'gloom-machina-pilish--stop)

(defun gloom-machina-pilish--metadata (metadata transport)
  "Return the model of Pilish TRANSPORT, else call Machina's METADATA."
  (if (not (gloom-machina-pilish--transport-p transport))
      (funcall metadata transport)
    (let ((buffer (mx-machina-transport-buffer transport)))
      (when (buffer-live-p buffer)
        (let ((model (plist-get (buffer-local-value 'pilish--state buffer) :model)))
          (list :model (if (listp model)
                           (or (plist-get model :name) (plist-get model :id))
                         model)))))))
(advice-add 'mx-machina-backend-metadata :around #'gloom-machina-pilish--metadata)

(defun gloom-machina-pilish--hold (_session transport)
  "Hold messages for Pilish TRANSPORT: agent messaging is not supported."
  (when (gloom-machina-pilish--transport-p transport)
    "Agent messaging does not support Pilish agents"))
(with-eval-after-load 'mx-machina-messaging
  (advice-add 'mx-machina-messaging--hold :before-until #'gloom-machina-pilish--hold))


;;; Start and observe

(defun gloom-machina-pilish-start (directory conversation callback)
  "Start Pi in DIRECTORY in a Pilish session for Machina.
CONVERSATION is the saved Pi session ID to resume, or nil for a new agent.
Report observations to CALLBACK.  Return the Machina transport."
  (let* ((id (or conversation (gloom-machina-pilish--uuid)))
         (transport (mx-machina--transport-create :callback callback :conversation id))
         (directory (file-name-as-directory (expand-file-name directory)))
         (default-directory directory)
         (pilish-extra-args (append pilish-extra-args (list "--session-id" id)))
         ;; The session name keeps several agents in one checkout apart.
         (chat (save-window-excursion
                 (pilish--setup-session directory (concat "machina-" (substring id 0 8))))))
    (puthash transport t gloom-machina-pilish--transports)
    (setf (mx-machina-transport-buffer transport) chat)
    (with-current-buffer chat
      (setq-local mx-machina--backend-kind 'pilish
                  gloom-machina-pilish--transport transport
                  gloom-machina-pilish--resumed (and conversation t)
                  gloom-machina-pilish--asking nil
                  gloom-machina-pilish--activity nil))
    (funcall callback "starting" "unknown")
    transport))

(defun gloom-machina-pilish--live-p (transport)
  "Return non-nil when TRANSPORT is ready and neither stopping nor failed."
  (and (mx-machina-transport-ready transport)
       (not (mx-machina-transport-stopping transport))
       (not (mx-machina-transport-failed transport))))

(defun gloom-machina-pilish--report (transport &optional force)
  "Report TRANSPORT's activity to Machina when it changed, or when FORCE."
  (when (gloom-machina-pilish--live-p transport)
    (with-current-buffer (mx-machina-transport-buffer transport)
      (let ((activity (cond (gloom-machina-pilish--asking "approval")
                            ((memq pilish--status '(sending streaming compacting)) "working")
                            (t "input"))))
        (when (or force (not (equal activity gloom-machina-pilish--activity)))
          (setq gloom-machina-pilish--activity activity)
          (funcall (mx-machina-transport-callback transport) "live" activity
                   (mx-machina-transport-conversation transport)))))))

(defun gloom-machina-pilish--ready (chat-buf _response)
  "Mark CHAT-BUF's transport live once Pi reports the expected session ID."
  (when-let* (((buffer-live-p chat-buf))
              (transport (buffer-local-value 'gloom-machina-pilish--transport chat-buf))
              ((not (mx-machina-transport-failed transport)))
              ((not (mx-machina-transport-stopping transport))))
    (with-current-buffer chat-buf
      (let ((reported (plist-get pilish--state :session-id))
            (expected (mx-machina-transport-conversation transport)))
        (cond
         ((mx-machina-transport-ready transport)
          (run-hook-with-args 'mx-machina-backend-event-hook transport 'metadata
                              (mx-machina-backend-metadata transport)))
         ((not reported))
         ((not (equal reported expected))
          (mx-machina--transport-fail transport "Pi reported a different session ID"))
         (t
          (setf (mx-machina-transport-ready transport) t)
          (gloom-machina-pilish--report transport t)
          (run-hook-with-args 'mx-machina-backend-event-hook transport 'metadata
                              (mx-machina-backend-metadata transport))
          (when gloom-machina-pilish--resumed
            (if (and (numberp (plist-get pilish--state :message-count))
                     (> (plist-get pilish--state :message-count) 0))
                (pilish--load-session-history pilish--process nil chat-buf)
              (message "Pi session %s had no saved messages; it continues empty"
                       (substring expected 0 8))))))))))
(advice-add 'pilish--apply-state-response :after #'gloom-machina-pilish--ready)

(defun gloom-machina-pilish--message-text (message)
  "Return the text of Pi MESSAGE, whose content is a string or text blocks."
  (let ((content (plist-get message :content)))
    (if (stringp content)
        content
      (mapconcat (lambda (block)
                   (or (and (equal (plist-get block :type) "text") (plist-get block :text)) ""))
                 (and (sequencep content) content) ""))))

(defun gloom-machina-pilish--emit (transport event)
  "Translate Pi EVENT into Machina's backend events for TRANSPORT."
  (when (gloom-machina-pilish--live-p transport)
    (pcase (plist-get event :type)
      ("message_start"
       (let ((message (plist-get event :message)))
         (when (equal (plist-get message :role) "user")
           (run-hook-with-args
            'mx-machina-backend-event-hook transport 'prompt
            (list :hash (secure-hash 'sha256 (encode-coding-string
                                              (gloom-machina-pilish--message-text message)
                                              'utf-8-unix)))))))
      ("message_update"
       (let ((delta (plist-get event :assistantMessageEvent)))
         (when (equal (plist-get delta :type) "text_delta")
           (run-hook-with-args 'mx-machina-backend-event-hook transport 'message nil)
           (run-hook-with-args 'mx-machina-backend-event-hook transport 'reply-chunk
                               (plist-get delta :delta)))))
      ("agent_end"
       (run-hook-with-args 'mx-machina-backend-event-hook transport 'turn-ended nil)))))

(defun gloom-machina-pilish--handle-event (handle proc event)
  "Call Pilish's HANDLE for PROC's EVENT, reporting it to Machina.
Pilish answers extension dialogs synchronously inside HANDLE, so the agent
shows approval exactly while the dialog is open."
  (let* ((chat (process-get proc 'pilish-chat-buffer))
         (transport (and (buffer-live-p chat)
                         (buffer-local-value 'gloom-machina-pilish--transport chat)))
         (asking (and transport
                      (equal (plist-get event :type) "extension_ui_request")
                      (member (plist-get event :method) '("confirm" "select" "input")))))
    (when asking
      (with-current-buffer chat (setq gloom-machina-pilish--asking t))
      (gloom-machina-pilish--report transport))
    (unwind-protect
        (funcall handle proc event)
      (when transport
        (when asking
          (with-current-buffer chat (setq gloom-machina-pilish--asking nil)))
        (gloom-machina-pilish--emit transport event)
        (gloom-machina-pilish--report transport)))))
(advice-add 'pilish--handle-event :around #'gloom-machina-pilish--handle-event)


;;; Display

(defun gloom-machina-pilish--show-input (&rest _)
  "Show the Pilish input buffer below a displayed Machina Pilish chat."
  (let* ((chat (window-buffer))
         (input (and (buffer-local-value 'gloom-machina-pilish--transport chat)
                     (buffer-local-value 'pilish--input-buffer chat))))
    (when (and (buffer-live-p input) (not (get-buffer-window input)))
      (let ((window (split-window nil (- gloom-machina-pilish-input-height) 'below)))
        (set-window-buffer window input)
        (select-window window)))))
(advice-add 'mx-machina--show-conversation :after #'gloom-machina-pilish--show-input)
(advice-add 'mx-machina-focus :after #'gloom-machina-pilish--show-input)

;; Pilish's input is for typing; its chat is a read-only transcript.
(with-eval-after-load 'meow
  (setq meow-mode-state-list
        (append '((pilish-input-mode . insert) (pilish-chat-mode . motion))
                meow-mode-state-list)))

(provide 'gloom-machina-pilish)
;;; machina-pilish.el ends here
