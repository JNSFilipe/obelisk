;;; sheprd.el --- Herdr-style terminal multiplexer for Emacs -*- lexical-binding: t; -*-

;; This file uses Doom's public workspace helpers and Ghostel's public API, but
;; otherwise stays independent of Doom macros so it can be byte-compiled and
;; tested on its own.  It expects Doom's :ui workspaces module and Ghostel.

;;; Commentary:

;; Sheprd is Herdr (https://herdr.dev) rebuilt inside Emacs.  It maps Herdr's
;; object model onto persp-mode and Ghostel:
;;
;;   Herdr workspace -> Sheprd session  (a persp-mode perspective, numbered)
;;   Herdr pane      -> Sheprd pane     (an Emacs window, normally a terminal)
;;   Herdr agent     -> Sheprd agent    (a recognized coding agent in a pane)
;;
;; Sessions are hermetically sealed: every terminal Sheprd knows about belongs
;; to exactly one session and is unreachable from the others.  There is no
;; shared or "global" terminal of any kind.
;;
;; Herdr's tabs are deliberately absent.  A second layer of saved layouts is
;; clutter when `sheprd-switch-buffer' already moves around inside a session, so
;; the keys tabs would have taken go to sessions instead.
;;
;; Control runs through one command map, `sheprd-command-map', reachable from
;; the `C-c s' prefix and, when Doom's leader exists, from `<leader> TAB'.  Keys
;; follow Herdr's defaults where they do not collide: v and - for splits,
;; h/j/k/l for pane focus, z/x/r for zoom/close/resize, n/p/w/N/W/D for
;; sessions, b/g/q/o/? for sidebar, goto, detach, notifications and help.
;; Departing from Herdr on purpose, 1..9 select *sessions*, because sessions
;; are the numbered thing here.
;;
;; The sidebar is mouse-native: sessions and agents are all buttons.
;; See sheprd.md for the full keymap, limitations and integration checks.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(declare-function +workspace-current-name "ui/workspaces/autoload/workspaces")
(declare-function +workspace-get "ui/workspaces/autoload/workspaces" (name &optional noerror))
(declare-function +workspace-exists-p "ui/workspaces/autoload/workspaces" (name))
(declare-function +workspace-list-names "ui/workspaces/autoload/workspaces")
(declare-function +workspace-buffer-list "ui/workspaces/autoload/workspaces" (&optional persp))
(declare-function +workspace-switch "ui/workspaces/autoload/workspaces" (name &optional auto-create-p))
(declare-function +workspace-new "ui/workspaces/autoload/workspaces" (name))
(declare-function +workspace-rename "ui/workspaces/autoload/workspaces" (name new-name))
(declare-function +workspace-delete "ui/workspaces/autoload/workspaces" (workspace))
(declare-function +workspace/display "ui/workspaces/autoload/workspaces")
(declare-function ghostel "ghostel" (&optional arg))
(declare-function ghostel-copy-mode "ghostel")
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function ghostel-force-redraw "ghostel")
(declare-function ghostel--copy-all-text "ghostel-module" (term))
(declare-function magit-diff-unstaged "magit-diff" (&optional args files))
(declare-function magit-toplevel "magit" (&optional directory))
(declare-function persp-add-buffer "persp-mode" (&optional buffers persp switch interactive))
(declare-function persp-contain-buffer-p "persp-mode" (buffer &optional persp))
(declare-function persp-parameter "persp-mode" (param-name &optional persp))
(declare-function persp-persps "persp-mode" (&optional phash regexp reverse))
(declare-function persp-switch-to-buffer "persp-mode" (buffer-or-name &optional norecord force-same-window))
(declare-function persp-remove-buffer "persp-mode" (&optional buffers persp remove switch killed interactive))
(declare-function set-persp-parameter "persp-mode" (param-name value &optional persp))
(declare-function persp-name "persp-mode" (persp))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function doom/leader "doom-keybinds")

(defvar persp-mode)
(defvar persp-autokill-buffer-on-remove)
(defvar persp-activated-functions)
(defvar persp-names-cache-changed-functions)
(defvar persp-before-switch-functions)
(defvar doom-leader-map)
(defvar ghostel-buffer-name)
(defvar ghostel-command-start-functions)
(defvar ghostel-command-finish-functions)
(defvar ghostel-exit-functions)
(defvar ghostel--process)
(defvar ghostel--term)
(defvar ghostel--command-running)
(defvar ghostel--last-output-time)
(defvar ghostel--pending-redraw)
(defvar ghostel--input-mode)
(defvar ghostel--pid)
(defvar ghostel-identity)
(defvar sheprd-mode)


;;; ---------------------------------------------------------------------------
;;; Customization

(defgroup sheprd nil
  "Herdr-style sessions, panes and coding agents."
  :group 'convenience
  :prefix "sheprd-")

(defcustom sheprd-sidebar-width 32
  "Width, in columns, of the Sheprd sidebar."
  :type 'integer)

(defcustom sheprd-sidebar-side 'left
  "Frame side the Sheprd sidebar is docked to."
  :type '(choice (const left) (const right)))

(defcustom sheprd-refresh-interval 2.0
  "Seconds between agent status refreshes."
  :type 'number)

(defcustom sheprd-agent-probe-interval 10.0
  "Seconds between fallback probes of a terminal whose client is unknown.
Explicitly launched or tracked clients need no process probes.  Other
terminals are revalidated at this interval, including known agents."
  :type 'number)

(defcustom sheprd-status-scan-chars 8000
  "Maximum number of trailing terminal characters inspected per refresh."
  :type 'integer)

(defcustom sheprd-prefix-key "C-c s"
  "Global key sequence bound to `sheprd-command-map'.
This is Sheprd's answer to Herdr's `ctrl+b' prefix.  Ghostel lets `C-c'
through to Emacs, so this prefix also works inside a terminal pane.
Set it before loading Sheprd, or call `sheprd-install-keys' afterwards."
  :type 'string)

(defcustom sheprd-leader-keys '("TAB" "<tab>")
  "Keys in Doom's leader map bound to `sheprd-command-map'.
Both the terminal TAB event and the GUI <tab> event are listed so the
leader prefix behaves identically in either frame type.  Nil installs no
leader bindings."
  :type '(repeat string))

(defcustom sheprd-split-spawns-terminal t
  "Whether splitting a pane starts a fresh terminal in the new pane.
This matches Herdr, where every pane is a terminal.  A prefix argument
inverts the choice for a single split."
  :type 'boolean)

(defcustom sheprd-client-command-regexps
  '((codex . "\\(?:\\`\\|[^[:alnum:]_]\\)codex\\(?:\\'\\|[^[:alnum:]_]\\)")
    (claude . "\\(?:\\`\\|[^[:alnum:]_]\\)claude\\(?:\\'\\|[^[:alnum:]_]\\)"))
  "Regexps used to recognize an agent.
They are matched against executable names from Ghostel exec metadata
and the local terminal process tree.  Add aliases here if, for example,
Claude Code is launched through a wrapper."
  :type '(alist :key-type symbol :value-type regexp))

(defcustom sheprd-agent-commands
  '((codex . ("codex")) (claude . ("claude")))
  "Programs and argument lists available to `sheprd-launch-agent'."
  :type '(alist :key-type symbol :value-type (repeat string)))

(defcustom sheprd-blocked-regexp
  (concat
   "\\(?:do you want to proceed\\|would you like\\|needs? your approval\\|"
   "permission required\\|allow this\\|press enter to confirm\\|"
   "select an option\\|yes, and don't ask again\\|esc to cancel\\|continue\\?\\)")
  "Regexp indicating that an agent is blocked on explicit user input."
  :type 'regexp)

(defcustom sheprd-working-regexp
  (concat
   "\\(?:esc to interrupt\\|ctrl[-+]c to interrupt\\|running tool\\|"
   "^[[:space:]•●✻⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]*"
   "\\(?:working\\|thinking\\|baking\\|processing\\)[ .…(]\\)")
  "Regexp indicating that an agent is actively working."
  :type 'regexp)

(defcustom sheprd-notify-statuses '(blocked done)
  "Agent statuses that raise a notification when an agent enters them."
  :type '(repeat symbol))

(defcustom sheprd-notification-function #'sheprd-default-notify
  "Function called with (BUFFER SESSION CLIENT STATUS) on a status change.
It runs only for statuses listed in `sheprd-notify-statuses'."
  :type 'function)


;;; ---------------------------------------------------------------------------
;;; Faces

(defface sheprd-heading-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for Sheprd sidebar section headings."
  :group 'sheprd)

(defface sheprd-session-face
  '((t (:inherit default)))
  "Face for an inactive Sheprd session."
  :group 'sheprd)

(defface sheprd-session-active-face
  '((t (:inherit mode-line-emphasis :weight bold)))
  "Face for the current Sheprd session."
  :group 'sheprd)

(defface sheprd-agent-working-face
  '((t (:inherit success :weight semi-bold)))
  "Face for an agent that is working."
  :group 'sheprd)

(defface sheprd-agent-blocked-face
  '((t (:inherit warning :weight bold)))
  "Face for an agent waiting for input."
  :group 'sheprd)

(defface sheprd-agent-done-face
  '((t (:inherit font-lock-constant-face :weight semi-bold)))
  "Face for an agent that finished and has not been looked at."
  :group 'sheprd)

(defface sheprd-agent-idle-face
  '((t (:inherit shadow)))
  "Face for an idle agent."
  :group 'sheprd)

(defface sheprd-agent-unknown-face
  '((t (:inherit shadow :slant italic)))
  "Face for an agent whose state cannot be classified."
  :group 'sheprd)

(defface sheprd-group-face
  '((t (:inherit font-lock-comment-face :weight semi-bold)))
  "Face for session group labels in the agents section."
  :group 'sheprd)


;;; ---------------------------------------------------------------------------
;;; State

(defconst sheprd--sidebar-buffer " *sheprd*"
  "Base name of the per-frame sidebar buffer.")

(defvar sheprd--refresh-timer nil)
(defvar sheprd--scheduled-refresh nil)
(defvar sheprd--inhibit-display nil
  "Non-nil while a layout operation must not redisplay the sidebar.")
(defvar sheprd--process-table nil
  "Process table shared by one refresh pass, or nil when it must be built.")
(defvar sheprd--previous-session nil
  "Session left by the most recent session switch.")
(defvar sheprd--notification-target nil
  "Buffer of the agent that raised the most recent notification.")

(defvar-local sheprd--session nil
  "Name of the session that owns this buffer.
Set on every terminal Sheprd creates or adopts; it is what makes
sessions hermetic.")
(put 'sheprd--session 'permanent-local t)

(defvar-local sheprd--tracked-client nil
  "Explicit client identity, independent of shell integration markers.")
(defvar-local sheprd--status-override nil)
(defvar-local sheprd--agent-client nil)
(defvar-local sheprd--agent-probe-time nil)
(defvar-local sheprd--snapshot-key nil)
(defvar-local sheprd--cached-terminal-tail nil)
(defvar-local sheprd--raw-status nil
  "Last raw (seen-independent) status computed for this terminal.")
(defvar-local sheprd--unseen-completion nil
  "Non-nil when this agent finished working and has not been looked at.")
(defvar-local sheprd--notified-status nil
  "Status this terminal was last notified about.")


;;; ---------------------------------------------------------------------------
;;; Sessions

(defun sheprd-session-names ()
  "Return open session names, or nil before perspectives initialize."
  (when (fboundp '+workspace-list-names)
    (ignore-errors (+workspace-list-names))))

(defun sheprd-current-session ()
  "Return the selected frame's current session name."
  (when (fboundp '+workspace-current-name)
    (ignore-errors (+workspace-current-name))))

(defun sheprd--session-persp (&optional name)
  "Return the perspective object for session NAME, or the current one."
  (let ((name (or name (sheprd-current-session))))
    (and name (fboundp '+workspace-get)
         (ignore-errors (+workspace-get name t)))))

(defun sheprd-session-index (&optional name)
  "Return the one-based index of session NAME, or nil."
  (let* ((name (or name (sheprd-current-session)))
         (position (and name (cl-position name (sheprd-session-names) :test #'equal))))
    (and position (1+ position))))

(defun sheprd--session-buffers (&optional name)
  "Return the live buffers owned by session NAME."
  (when-let* ((persp (sheprd--session-persp name)))
    (cl-remove-if-not #'buffer-live-p
                      (ignore-errors (+workspace-buffer-list persp)))))

(defun sheprd--terminal-buffer-p (buffer)
  "Return non-nil when BUFFER is a Ghostel terminal."
  (and (buffer-live-p buffer)
       (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                'ghostel-mode)))

(defun sheprd--claim-buffer (buffer &optional session)
  "Make SESSION, or the current session, the sole owner of BUFFER."
  (when (buffer-live-p buffer)
    (let ((session (or session (sheprd-current-session))))
      (with-current-buffer buffer
        (setq sheprd--session session))
      (when (and session (bound-and-true-p persp-mode))
        (when-let* ((persp (sheprd--session-persp session)))
          (ignore-errors (persp-add-buffer buffer persp nil nil))))
      (sheprd--confine-buffer buffer))
    buffer))

(defun sheprd--confine-buffer (buffer)
  "Remove BUFFER from every perspective except the one that owns it.
Keeping a terminal out of foreign perspectives is what makes a session
hermetic: it cannot be listed, switched to, or displayed elsewhere."
  (when (and (bound-and-true-p persp-mode)
             (buffer-live-p buffer)
             (fboundp 'persp-persps))
    (let* ((owner (buffer-local-value 'sheprd--session buffer))
           (keep (and owner (sheprd--session-persp owner)))
           (persp-autokill-buffer-on-remove nil))
      (when keep
        (dolist (persp (ignore-errors (persp-persps)))
          (when (and persp
                     (not (eq persp keep))
                     (ignore-errors (persp-contain-buffer-p buffer persp)))
            ;; switch=nil: never yank another buffer into a live window.
            (ignore-errors (persp-remove-buffer buffer persp nil nil))))))))

(defun sheprd--enforce-isolation ()
  "Re-seal every Sheprd terminal into the single session that owns it.
New terminals opened outside Sheprd are adopted by the current session."
  (when (bound-and-true-p persp-mode)
    (let ((current (sheprd-current-session)))
      (dolist (buffer (buffer-list))
        (when (sheprd--terminal-buffer-p buffer)
          (let ((owner (buffer-local-value 'sheprd--session buffer)))
            (cond
             ((and owner (sheprd--session-persp owner))
              (sheprd--confine-buffer buffer))
             (current (sheprd--claim-buffer buffer current)))))))))

(defun sheprd--switch-session (name)
  "Switch to the session called NAME.
`+workspace/switch-to' reads an all-digit name as an index, which would
send a session literally named \"2\" somewhere else, so switch by name."
  (let ((current (sheprd-current-session)))
    (unless (equal name current)
      (setq sheprd--previous-session current)))
  ;; persp-mode restores each perspective's own window configuration.
  (+workspace-switch name)
  (sheprd--enforce-isolation)
  (when (fboundp '+workspace/display)
    (ignore-errors (+workspace/display)))
  (sheprd--schedule-refresh))

(defun sheprd-switch-session (name)
  "Switch to session NAME, prompting with completion."
  (interactive
   (list (completing-read "Session: " (sheprd-session-names) nil t)))
  (sheprd--switch-session name))

(defun sheprd-switch-session-index (index)
  "Switch to the one-based session INDEX."
  (interactive "nSession: ")
  (let ((name (nth (1- index) (sheprd-session-names))))
    (unless name
      (user-error "No session at #%s" index))
    (sheprd--switch-session name)))

(defun sheprd-last-session ()
  "Switch back to the session left by the previous switch."
  (interactive)
  (let ((name sheprd--previous-session))
    (unless (and name (member name (sheprd-session-names)))
      (user-error "No previous session"))
    (sheprd--switch-session name)))

(defun sheprd-final-session ()
  "Switch to the last session in the list."
  (interactive)
  (let ((name (car (last (sheprd-session-names)))))
    (unless name (user-error "No sessions are open"))
    (sheprd--switch-session name)))

(defun sheprd--cycle-session (step)
  "Switch STEP sessions away from the current one, wrapping around."
  (let* ((names (sheprd-session-names))
         (position (cl-position (sheprd-current-session) names :test #'equal)))
    (unless names (user-error "No sessions are open"))
    (sheprd--switch-session
     (nth (mod (+ (or position 0) step) (length names)) names))))

(defun sheprd-next-session ()
  "Switch to the next session."
  (interactive)
  (sheprd--cycle-session 1))

(defun sheprd-previous-session ()
  "Switch to the previous session."
  (interactive)
  (sheprd--cycle-session -1))

(defun sheprd-switch-buffer ()
  "Switch buffer within the current session only.
`persp-switch-to-buffer' completes over one perspective at a time, so a
sealed session\='s terminals do not show up while another session is
current — unlike `switch-to-buffer' or `consult-buffer', which see every
buffer in the instance and read straight through the seal.

An active remapping is honored rather than bypassed: Doom remaps that
command to its own workspace-aware picker, and calling it directly would
give this key a different interface from the one `C-x b' offers."
  (interactive)
  (unless (fboundp 'persp-switch-to-buffer)
    (user-error "persp-mode is unavailable"))
  (call-interactively (or (command-remapping #'persp-switch-to-buffer)
                          #'persp-switch-to-buffer)))

(defun sheprd--unique-session-name ()
  "Return an unused default session name."
  (let ((index 1) name)
    (while (progn (setq name (format "session-%d" index))
                  (member name (sheprd-session-names)))
      (setq index (1+ index)))
    name))

(defun sheprd-new-session (name)
  "Create session NAME, switch to it, and start its first terminal."
  (interactive
   (list (read-string "New session: " (sheprd--unique-session-name))))
  (when (member name (sheprd-session-names))
    (user-error "A session called %s already exists" name))
  (+workspace-new name)
  (sheprd--switch-session name)
  (sheprd--in-main-window (lambda () (delete-other-windows) (sheprd--spawn-terminal)))
  (sheprd--schedule-refresh))

(defun sheprd-rename-session (new-name)
  "Rename the current session to NEW-NAME."
  (interactive
   (list (read-string "Rename session to: " (sheprd-current-session))))
  (let ((old (sheprd-current-session)))
    (unless old (user-error "No current session"))
    (+workspace-rename old new-name)
    (dolist (buffer (buffer-list))
      (when (equal (buffer-local-value 'sheprd--session buffer) old)
        (with-current-buffer buffer (setq sheprd--session new-name))))
    (when (equal sheprd--previous-session old)
      (setq sheprd--previous-session new-name))
    (sheprd--schedule-refresh)))

(defun sheprd-kill-session (name)
  "Kill session NAME along with every terminal it owns.
Because sessions are sealed, those terminals exist nowhere else; leaving
them alive would leak a session's processes into the buffer list."
  (interactive
   (list (completing-read "Kill session: " (sheprd-session-names) nil t
                          (sheprd-current-session))))
  (let ((names (sheprd-session-names)))
    (unless (member name names)
      (user-error "No session called %s" name))
    (when (and (= (length names) 1)
               (not (y-or-n-p "This is the only session; kill it anyway? ")))
      (user-error "Nothing killed"))
    (dolist (buffer (sheprd--session-buffers name))
      (when (sheprd--terminal-buffer-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))
    (when-let* ((persp (sheprd--session-persp name)))
      (+workspace-delete persp))
    (when (equal name sheprd--previous-session)
      (setq sheprd--previous-session nil))
    (when (equal name (sheprd-current-session))
      (when-let* ((next (car (sheprd-session-names))))
        (sheprd--switch-session next)))
    (sheprd--schedule-refresh)))


;;; ---------------------------------------------------------------------------
;;; Layout
;;
;; Window states are captured from and applied to the frame root.  Sessions
;; keep their own layouts through persp-mode; these helpers exist for zoom,
;; which has to put a layout back exactly as it was.

(defun sheprd--sidebar-window (&optional frame)
  "Return this frame's live sidebar window, if any."
  (when-let* ((buffer (sheprd--frame-sidebar frame t)))
    (get-buffer-window buffer (or frame (selected-frame)))))

(defmacro sheprd--with-bare-frame (&rest body)
  "Run BODY with the sidebar removed, then put the sidebar back.
Window states are captured from and applied to the frame root, so the
side window must not be part of them."
  (declare (indent 0) (debug t))
  `(let ((sheprd--inhibit-display t)
         (sheprd--had-sidebar (and (sheprd--sidebar-window) t)))
     (when sheprd--had-sidebar
       (let ((ignore-window-parameters t))
         (delete-window (sheprd--sidebar-window))))
     (unwind-protect (progn ,@body)
       (when sheprd--had-sidebar
         (let ((sheprd--inhibit-display nil))
           (sheprd--display))))))

(defun sheprd--capture-layout ()
  "Return the window state of the current frame's editing area."
  (sheprd--with-bare-frame
    (window-state-get (frame-root-window) t)))

(defun sheprd--apply-layout (state)
  "Restore STATE into the current frame's editing area."
  (when state
    (sheprd--with-bare-frame
      (condition-case nil
          (window-state-put state (frame-root-window) 'safe)
        (error nil)))))


;;; ---------------------------------------------------------------------------
;;; Panes

(defun sheprd--pane-p (window)
  "Return non-nil when WINDOW is an ordinary editing pane."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))))

(defun sheprd--panes (&optional frame)
  "Return the editing panes of FRAME in cyclic order."
  (cl-remove-if-not #'sheprd--pane-p (window-list frame 'never)))

(defun sheprd--main-window ()
  "Return a live editing pane on the selected frame."
  (or (cl-find-if #'sheprd--pane-p (window-list nil 'never))
      (selected-window)))

(defun sheprd--in-main-window (function)
  "Select an editing pane and call FUNCTION there."
  (select-window (sheprd--main-window))
  (funcall function))

(defun sheprd--visible-terminals (&optional frame)
  "Return the terminal buffers shown in FRAME's panes."
  (delq nil (mapcar (lambda (window)
                      (let ((buffer (window-buffer window)))
                        (and (sheprd--terminal-buffer-p buffer) buffer)))
                    (sheprd--panes frame))))

(defun sheprd--spawn-terminal (&optional directory)
  "Start a fresh terminal in the selected window and return its buffer.
The terminal is claimed by the current session, so it is invisible to
every other session."
  (require 'ghostel)
  (let* ((default-directory (or directory default-directory))
         (display-buffer-overriding-action
          '((display-buffer-same-window) (inhibit-same-window . nil)))
         ;; A non-numeric prefix makes Ghostel allocate a fresh instance
         ;; instead of reusing an existing slot, so every pane is independent.
         (buffer (ghostel '(4))))
    (sheprd--claim-buffer buffer)
    buffer))

(defun sheprd-new-terminal ()
  "Replace the selected pane with a brand new terminal."
  (interactive)
  (sheprd--in-main-window #'sheprd--spawn-terminal)
  (sheprd--schedule-refresh))

(defun sheprd--split (direction arg)
  "Split the selected pane in DIRECTION and honor ARG for the new buffer."
  (select-window (sheprd--main-window))
  (let ((window (if (eq direction 'right)
                    (split-window-right)
                  (split-window-below))))
    (select-window window)
    (when (if arg (not sheprd-split-spawns-terminal) sheprd-split-spawns-terminal)
      (sheprd--spawn-terminal))
    (sheprd--schedule-refresh)
    window))

(defun sheprd-split-right (&optional arg)
  "Split the selected pane side by side.
A prefix ARG inverts `sheprd-split-spawns-terminal' for this split."
  (interactive "P")
  (sheprd--split 'right arg))

(defun sheprd-split-down (&optional arg)
  "Split the selected pane top and bottom.
A prefix ARG inverts `sheprd-split-spawns-terminal' for this split."
  (interactive "P")
  (sheprd--split 'down arg))

(defun sheprd--window-in-direction (direction)
  "Return the editing pane DIRECTION of the selected one."
  (let ((window (window-in-direction direction)))
    (and (sheprd--pane-p window) window)))

(defun sheprd--focus-pane (direction)
  "Focus the editing pane DIRECTION of the selected one."
  (let ((window (sheprd--window-in-direction direction)))
    (unless window (user-error "No pane %s of here" direction))
    (select-window window)
    (sheprd--seen-here)))

(defun sheprd-focus-pane-left ()
  "Focus the pane to the left."
  (interactive)
  (sheprd--focus-pane 'left))

(defun sheprd-focus-pane-down ()
  "Focus the pane below."
  (interactive)
  (sheprd--focus-pane 'below))

(defun sheprd-focus-pane-up ()
  "Focus the pane above."
  (interactive)
  (sheprd--focus-pane 'above))

(defun sheprd-focus-pane-right ()
  "Focus the pane to the right."
  (interactive)
  (sheprd--focus-pane 'right))

(defun sheprd--swap-pane (direction)
  "Swap the selected pane with the pane DIRECTION of it."
  (let ((window (sheprd--window-in-direction direction)))
    (unless window (user-error "No pane %s of here" direction))
    (window-swap-states (selected-window) window)
    (select-window window)
    (sheprd--schedule-refresh)))

(defun sheprd-swap-pane-left ()
  "Swap the selected pane with the one to its left."
  (interactive)
  (sheprd--swap-pane 'left))

(defun sheprd-swap-pane-down ()
  "Swap the selected pane with the one below it."
  (interactive)
  (sheprd--swap-pane 'below))

(defun sheprd-swap-pane-up ()
  "Swap the selected pane with the one above it."
  (interactive)
  (sheprd--swap-pane 'above))

(defun sheprd-swap-pane-right ()
  "Swap the selected pane with the one to its right."
  (interactive)
  (sheprd--swap-pane 'right))

(defun sheprd--cycle-pane (step)
  "Select the pane STEP positions away in cyclic order."
  (let* ((panes (sheprd--panes))
         (position (cl-position (selected-window) panes)))
    (unless panes (user-error "No panes"))
    (select-window (nth (mod (+ (or position 0) step) (length panes)) panes))
    (sheprd--seen-here)))

(defun sheprd-cycle-pane-next ()
  "Select the next pane."
  (interactive)
  (sheprd--cycle-pane 1))

(defun sheprd-cycle-pane-previous ()
  "Select the previous pane."
  (interactive)
  (sheprd--cycle-pane -1))

(defun sheprd-last-pane ()
  "Return to the pane that was selected before the current one."
  (interactive)
  (let ((window (get-mru-window nil nil t)))
    (unless (sheprd--pane-p window)
      (user-error "No other pane"))
    (select-window window)
    (sheprd--seen-here)))

(defun sheprd-close-pane ()
  "Close the selected pane, ending its terminal.
The terminal only dies with its last pane: a buffer another window is
still showing must survive, or closing one half of a split would kill a
session the other half is still driving."
  (interactive)
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         (only-pane-p (null (cdr (get-buffer-window-list buffer nil t)))))
    (unless (sheprd--pane-p window)
      (user-error "Not in a pane"))
    (when (and (sheprd--terminal-buffer-p buffer)
               only-pane-p
               (or (not (process-live-p (buffer-local-value 'ghostel--process buffer)))
                   (y-or-n-p (format "Kill terminal %s? " (buffer-name buffer)))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (when (and (window-live-p window) (> (length (sheprd--panes)) 1))
      (delete-window window))
    (sheprd--schedule-refresh)))

(defun sheprd-zoom ()
  "Toggle zoom of the selected pane, Herdr style."
  (interactive)
  (let ((saved (frame-parameter nil 'sheprd-zoom)))
    (if saved
        (progn
          (set-frame-parameter nil 'sheprd-zoom nil)
          (sheprd--apply-layout saved))
      (unless (sheprd--pane-p (selected-window))
        (user-error "Not in a pane"))
      (when (= (length (sheprd--panes)) 1)
        (user-error "Only one pane to zoom"))
      (set-frame-parameter nil 'sheprd-zoom (sheprd--capture-layout))
      (delete-other-windows))
    (sheprd--schedule-refresh)))

(defun sheprd-rename-pane (name)
  "Rename the buffer shown in the selected pane to NAME."
  (interactive (list (read-string "Rename pane to: " (buffer-name))))
  (rename-buffer name t)
  (sheprd--schedule-refresh))

(defun sheprd-copy-mode ()
  "Enter Ghostel's copy mode in the selected pane."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Not in a terminal pane"))
  (require 'ghostel)
  (ghostel-copy-mode))

(defun sheprd-edit-scrollback ()
  "Open the selected terminal's scrollback in an ordinary buffer."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Not in a terminal pane"))
  (let* ((source (current-buffer))
         (text (or (sheprd--full-terminal-text source) ""))
         (buffer (get-buffer-create
                  (format "*sheprd scrollback: %s*" (buffer-name source)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-max)))
      (set-buffer-modified-p nil)
      (special-mode))
    (pop-to-buffer buffer)))

(defun sheprd--resize (direction)
  "Grow the selected pane by one step toward DIRECTION."
  (pcase direction
    ('left (shrink-window-horizontally 2))
    ('right (enlarge-window-horizontally 2))
    ('above (shrink-window 1))
    ('below (enlarge-window 1))))

(defvar sheprd-resize-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") (lambda () (interactive) (sheprd--resize 'left)))
    (define-key map (kbd "j") (lambda () (interactive) (sheprd--resize 'below)))
    (define-key map (kbd "k") (lambda () (interactive) (sheprd--resize 'above)))
    (define-key map (kbd "l") (lambda () (interactive) (sheprd--resize 'right)))
    (define-key map (kbd "<left>") (lambda () (interactive) (sheprd--resize 'left)))
    (define-key map (kbd "<down>") (lambda () (interactive) (sheprd--resize 'below)))
    (define-key map (kbd "<up>") (lambda () (interactive) (sheprd--resize 'above)))
    (define-key map (kbd "<right>") (lambda () (interactive) (sheprd--resize 'right)))
    (define-key map (kbd "=") #'balance-windows)
    map)
  "Transient keymap used by `sheprd-resize-mode'.")

(defun sheprd-resize-mode ()
  "Resize the selected pane with h/j/k/l until another key is pressed."
  (interactive)
  (message "Resize: h/j/k/l or arrows, = balances, any other key exits")
  (set-transient-map sheprd-resize-map t
                     (lambda () (message "Resize mode off"))))


;;; ---------------------------------------------------------------------------
;;; Agent detection and status

(defun sheprd--tail (text length)
  "Return at most LENGTH trailing characters from TEXT."
  (if (> (length text) length)
      (substring text (- length))
    text))

(defun sheprd--snapshot-key ()
  "Return a cache key for the current Ghostel buffer's terminal contents.
Ghostel only bumps `ghostel--last-output-time' while the terminal is
displayed; output arriving at a hidden terminal merely sets
`ghostel--pending-redraw'.  Keying on the timestamp alone would freeze
the status of every agent running in another session, so a buffer with
unrendered output is never served from the cache."
  (let ((time (and (boundp 'ghostel--last-output-time)
                   ghostel--last-output-time)))
    (and time
         (not (bound-and-true-p ghostel--pending-redraw))
         (not (memq (bound-and-true-p ghostel--input-mode) '(copy emacs)))
         (list time (buffer-chars-modified-tick)))))

(defun sheprd--full-terminal-text (buffer)
  "Return BUFFER's entire terminal text, scrollback included."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (and (bound-and-true-p ghostel--term)
               (fboundp 'ghostel--copy-all-text)
               (ignore-errors (ghostel--copy-all-text ghostel--term)))
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun sheprd--live-terminal-text ()
  "Return the current Ghostel buffer's terminal text.
Ghostel materializes the whole scrollback into the buffer, so its last
`sheprd-status-scan-chars' characters are authoritative — and cheap —
whenever no redraw is pending.  Only a terminal holding unrendered
output needs the native snapshot, which copies the entire scrollback."
  (or (and (or (bound-and-true-p ghostel--pending-redraw)
               (memq (bound-and-true-p ghostel--input-mode) '(copy emacs)))
           (bound-and-true-p ghostel--term)
           (fboundp 'ghostel--copy-all-text)
           (ignore-errors
             (sheprd--tail (or (ghostel--copy-all-text ghostel--term) "")
                           sheprd-status-scan-chars)))
      (buffer-substring-no-properties
       (max (point-min) (- (point-max) sheprd-status-scan-chars))
       (point-max))))

(defun sheprd--terminal-text (buffer)
  "Return BUFFER's live Ghostel terminal tail without changing its display."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((key (sheprd--snapshot-key)))
        (if (and key
                 (equal key sheprd--snapshot-key)
                 sheprd--cached-terminal-tail)
            sheprd--cached-terminal-tail
          (let ((tail (sheprd--live-terminal-text)))
            (setq sheprd--snapshot-key key
                  sheprd--cached-terminal-tail tail)
            tail))))))

(defun sheprd--client-from-text (text regexps)
  "Return the first client in REGEXPS recognized in TEXT."
  (when text
    (let ((case-fold-search t))
      (cl-loop for (client . regexp) in regexps
               when (string-match-p regexp text)
               return client))))

(defun sheprd--client-from-program (program)
  "Recognize PROGRAM without mistaking its parent directories for a client."
  (when (stringp program)
    (sheprd--client-from-text (file-name-nondirectory program)
                              sheprd-client-command-regexps)))

(defun sheprd--build-process-table ()
  "Return an alist of (PID PARENT-PID . COMMAND) for every visible process."
  (delq nil
        (mapcar (lambda (pid)
                  (when-let* ((attributes (process-attributes pid)))
                    (cons pid
                          (cons (alist-get 'ppid attributes)
                                (or (alist-get 'args attributes)
                                    (alist-get 'comm attributes)
                                    "")))))
                (list-system-processes))))

(defun sheprd--ensure-process-table ()
  "Return the process table, building it at most once per refresh pass."
  (or sheprd--process-table
      (setq sheprd--process-table (sheprd--build-process-table))))

(defun sheprd--descendant-commands (pid)
  "Return the command lines of PID and of every process below it."
  (when pid
    (let ((table (sheprd--ensure-process-table))
          (generation (list pid))
          commands)
      (while generation
        (let (next)
          (pcase-dolist (`(,child ,parent . ,command) table)
            (when (memq parent generation)
              (push command commands)
              (push child next)))
          (setq generation next)))
      commands)))

(defun sheprd--client-from-processes ()
  "Recognize a local agent executable, never arbitrary command arguments."
  (unless (file-remote-p default-directory)
    (let* ((pid (or (bound-and-true-p ghostel--pid)
                    (and (bound-and-true-p ghostel--process)
                         (process-id ghostel--process))))
           (table (and pid (sheprd--ensure-process-table)))
           (commands (and pid (cons (cddr (assq pid table))
                                    (sheprd--descendant-commands pid)))))
      (cl-loop for command in commands
               thereis (sheprd--client-from-program
                        (car (split-string (or command "") "[[:space:]]+" t)))))))

(defun sheprd--question-at-prompt-p (text)
  "Return non-nil when trailing TEXT looks like an agent question at a prompt."
  (let ((case-fold-search t)
        (tail (sheprd--tail text 900)))
    (string-match-p
     "\\?\\(?:.\\|\n\\)\\{0,300\\}\\(?:❯\\|›\\|>\\)[[:space:]]*\\'"
     tail)))

(defun sheprd--last-match (regexp text)
  "Return the start of the final REGEXP match in TEXT."
  (let ((start 0) found)
    (while (and (<= start (length text)) (string-match regexp text start))
      (setq found (match-beginning 0)
            start (max (1+ start) (match-end 0))))
    found))

(defun sheprd--agent-status (text)
  "Infer a raw agent status from the newest evidence in TEXT.
Returns one of `working', `blocked', `idle' or `unknown'.  This is a
heuristic, not an agent protocol; `sheprd-set-status' overrides it."
  (let* ((case-fold-search t)
         (tail (sheprd--tail (or text "") sheprd-status-scan-chars))
         (blocked (sheprd--last-match sheprd-blocked-regexp tail))
         (working (sheprd--last-match sheprd-working-regexp tail))
         (prompt (sheprd--last-match "^[[:blank:]]*[❯›>][[:blank:]]*$" tail)))
    (cond
     ((and working (> working (or blocked -1))
           (> working (or prompt -1))) 'working)
     ((and prompt (> prompt (or working -1))
           (> prompt (or blocked -1)))
      (if (sheprd--question-at-prompt-p tail) 'blocked 'idle))
     (blocked 'blocked)
     ((or working prompt) 'idle)
     (t 'unknown))))

(defun sheprd--ghostel-live-p ()
  "Return non-nil when the current Ghostel buffer has a live process."
  (and (derived-mode-p 'ghostel-mode)
       (boundp 'ghostel--process)
       (process-live-p ghostel--process)))

(defun sheprd--probe-due-p ()
  "Return non-nil when the current buffer's client may be probed again."
  (or (null sheprd--agent-probe-time)
      (> (float-time (time-since sheprd--agent-probe-time))
         sheprd-agent-probe-interval)))

(defun sheprd--buffer-seen-p (buffer)
  "Return non-nil when BUFFER is on display in a visible frame."
  (and (get-buffer-window buffer 'visible) t))

(defun sheprd--seen-here ()
  "Clear the unseen-completion flag of the pane just selected."
  (when (derived-mode-p 'ghostel-mode)
    (setq sheprd--unseen-completion nil)
    (sheprd--schedule-refresh)))

(defun sheprd--apply-seen-state (buffer raw)
  "Fold Herdr's seen tracking into RAW for BUFFER and return the status.
An agent that stops working while nobody is looking reports `done'
until its pane is displayed again."
  (with-current-buffer buffer
    (let ((previous sheprd--raw-status))
      (setq sheprd--raw-status raw)
      (when (and (eq previous 'working)
                 (memq raw '(idle unknown))
                 (not (sheprd--buffer-seen-p buffer)))
        (setq sheprd--unseen-completion t))
      (when (or (eq raw 'working)
                (eq raw 'blocked)
                (sheprd--buffer-seen-p buffer))
        (setq sheprd--unseen-completion nil))
      (if (and sheprd--unseen-completion (memq raw '(idle unknown)))
          'done
        raw))))

(defun sheprd--agent-info (buffer)
  "Return (CLIENT STATUS) when BUFFER hosts a supported live agent."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (sheprd--ghostel-live-p)
        (let* ((screen (or (sheprd--terminal-text buffer) ""))
               (command (alist-get 'command (bound-and-true-p ghostel-identity)))
               (explicit (or sheprd--tracked-client
                             (and (consp command)
                                  (sheprd--client-from-program (car command))))))
          ;; Periodically revalidate even a known client.  Without OSC markers,
          ;; command completion must not leave a stale agent in the sidebar.
          (when (and (not explicit) (sheprd--probe-due-p))
            (setq sheprd--agent-probe-time (current-time)
                  sheprd--agent-client (sheprd--client-from-processes)))
          (when-let* ((client (or explicit sheprd--agent-client)))
            (let ((status (or sheprd--status-override
                              (sheprd--apply-seen-state
                               buffer (sheprd--agent-status screen)))))
              (sheprd--maybe-notify buffer client status)
              (list client status))))))))

(defun sheprd-default-notify (buffer session client status)
  "Announce in the echo area that CLIENT in SESSION reached STATUS."
  (message "Sheprd: %s in %s is %s (%s)"
           client (or session "?") status (buffer-name buffer)))

(defun sheprd--maybe-notify (buffer client status)
  "Notify once when BUFFER's CLIENT enters a notable STATUS."
  (with-current-buffer buffer
    (unless (eq status sheprd--notified-status)
      (setq sheprd--notified-status status)
      (when (memq status sheprd-notify-statuses)
        (setq sheprd--notification-target buffer)
        (when (functionp sheprd-notification-function)
          (ignore-errors
            (funcall sheprd-notification-function
                     buffer sheprd--session client status)))))))

(defun sheprd--ghostel-command-start (buffer)
  "Record a supported agent command starting in Ghostel BUFFER."
  ;; The parser has not rendered the submitted command yet.  Do not snapshot
  ;; or guess identity from old scrollback inside its synchronous callback.
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq sheprd--cached-terminal-tail nil
            sheprd--agent-probe-time nil
            sheprd--agent-client nil
            sheprd--status-override nil
            sheprd--raw-status nil
            sheprd--unseen-completion nil
            sheprd--notified-status nil)))
  (sheprd--schedule-refresh))

(defun sheprd--ghostel-command-finish (buffer &optional event)
  "Clear agent metadata when Ghostel BUFFER's command finishes."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (or (stringp event) (bound-and-true-p ghostel--command-running))
        (setq sheprd--tracked-client nil))
      (setq sheprd--agent-client nil
            sheprd--status-override nil
            sheprd--agent-probe-time nil
            sheprd--cached-terminal-tail nil
            sheprd--raw-status nil
            sheprd--unseen-completion nil
            sheprd--notified-status nil)))
  (sheprd--schedule-refresh))

(defun sheprd--agent-entries ()
  "Return agent entry plists grouped in session order.
There is no ungrouped bucket: a terminal always belongs to exactly one
session, so an entry without a session cannot exist."
  (let ((sheprd--process-table nil))
    (cl-loop
     for session in (sheprd-session-names)
     append
     (cl-loop
      for buffer in (sort (copy-sequence (or (sheprd--session-buffers session) '()))
                          (lambda (a b) (string< (buffer-name a) (buffer-name b))))
      for info = (and (sheprd--terminal-buffer-p buffer)
                      (sheprd--agent-info buffer))
      when info
      collect (list :session session
                    :buffer buffer
                    :client (car info)
                    :status (cadr info))))))

(defun sheprd--session-rollup (entries session)
  "Return a compact status summary string for SESSION from ENTRIES."
  (let ((counts (make-hash-table :test #'eq)))
    (dolist (entry entries)
      (when (equal (plist-get entry :session) session)
        (cl-incf (gethash (plist-get entry :status) counts 0))))
    (string-join
     (delq nil
           (mapcar (lambda (status)
                     (let ((count (gethash status counts 0)))
                       (when (> count 0)
                         (propertize (format "%s%d" (sheprd--status-glyph status) count)
                                     'face (sheprd--status-face status)))))
                   '(blocked working done idle unknown)))
     " ")))


;;; ---------------------------------------------------------------------------
;;; Sidebar

(defun sheprd--status-glyph (status)
  "Return the sidebar glyph for STATUS."
  (pcase status
    ('working "●")
    ('blocked "◍")
    ('done "✓")
    ('idle "○")
    (_ "·")))

(defun sheprd--status-face (status)
  "Return the sidebar face for STATUS."
  (pcase status
    ('working 'sheprd-agent-working-face)
    ('blocked 'sheprd-agent-blocked-face)
    ('done 'sheprd-agent-done-face)
    ('idle 'sheprd-agent-idle-face)
    (_ 'sheprd-agent-unknown-face)))

(defun sheprd--short-buffer-name (buffer)
  "Return a compact human-readable name for terminal BUFFER."
  (let* ((name (buffer-name buffer))
         (name (string-trim name "[ *]+" "[ *]+"))
         (name (replace-regexp-in-string "\\`ghostel:? *" "" name t t))
         (name (replace-regexp-in-string "\\`sheprd:" "" name t t)))
    (if (string-empty-p name) "terminal" name)))

(define-button-type 'sheprd-session-button
  'follow-link t
  'mouse-face 'highlight
  'help-echo "mouse-1/RET: switch session"
  'action #'sheprd--activate-session-button)

(define-button-type 'sheprd-agent-button
  'follow-link t
  'mouse-face 'highlight
  'help-echo "mouse-1/RET: open agent pane; d: unstaged diff"
  'action #'sheprd--activate-agent-button)

(defun sheprd--button-key (button)
  "Return the stable identity of BUTTON."
  (pcase (button-type button)
    ('sheprd-session-button (list 'session (button-get button 'sheprd-session)))
    ('sheprd-agent-button (list 'agent
                                (button-get button 'sheprd-session)
                                (button-get button 'sheprd-buffer)))))

(defun sheprd--button-key-at-point ()
  "Return the stable identity of the button at point, if any."
  (when-let* ((button (button-at (point))))
    (sheprd--button-key button)))

(defun sheprd--restore-button (key)
  "Move point to the button identified by KEY, or the first button."
  (let ((button (next-button (point-min) t))
        found)
    (while (and button (not found))
      (if (equal key (sheprd--button-key button))
          (setq found button)
        ;; Buttons abut one another, so the next button starts exactly where
        ;; this one ends; without COUNT-CURRENT every second button would be
        ;; skipped and its selection lost on each refresh.
        (setq button (next-button (button-end button) t))))
    (when (or found (setq found (next-button (point-min) t)))
      (goto-char (button-start found)))))

(defun sheprd--frame-sidebar (&optional frame noselect)
  "Return FRAME's sidebar buffer, creating it unless NOSELECT."
  (let ((buffer (frame-parameter frame 'sheprd-sidebar)))
    (unless (buffer-live-p buffer)
      (if noselect
          (setq buffer nil)
        (setq buffer (get-buffer-create
                      (generate-new-buffer-name sheprd--sidebar-buffer)))
        (with-current-buffer buffer (sheprd-sidebar-mode))
        (set-frame-parameter frame 'sheprd-sidebar buffer)))
    buffer))

(defun sheprd--render-sessions (entries)
  "Insert the sessions section for ENTRIES."
  (let ((names (sheprd-session-names))
        (current (sheprd-current-session)))
    (insert (propertize " SESSIONS\n" 'face 'sheprd-heading-face))
    (if (null names)
        (insert (propertize "  starting…\n" 'face 'shadow))
      (cl-loop
       for session in names
       for index from 1
       for active = (equal session current)
       for width = (max 6 (- sheprd-sidebar-width 16))
       do
       (insert-text-button
        (format " %s %s %s %s\n"
                (if active "▸" " ")
                (if (< index 10) (number-to-string index) "·")
                (string-pad (truncate-string-to-width session width nil nil "…")
                            width)
                (sheprd--session-rollup entries session))
        'type 'sheprd-session-button
        'sheprd-session session
        'face (if active 'sheprd-session-active-face 'sheprd-session-face))))))

(defun sheprd--render-agents (entries)
  "Insert the agents section for ENTRIES."
  (let ((current (sheprd-current-session))
        (last-session 'none))
    (insert "\n" (propertize " AGENTS\n" 'face 'sheprd-heading-face))
    (if (null entries)
        (insert (propertize "  no agents running\n" 'face 'shadow))
      (dolist (entry entries)
        (let* ((session (plist-get entry :session))
               (buffer (plist-get entry :buffer))
               (client (plist-get entry :client))
               (status (plist-get entry :status))
               (face (sheprd--status-face status)))
          (unless (equal session last-session)
            (unless (eq last-session 'none) (insert "\n"))
            (insert (propertize (format " %s%s\n"
                                        (if (equal session current) "▸ " "  ")
                                        session)
                                'face 'sheprd-group-face))
            (setq last-session session))
          (insert-text-button
           (format "  %s %-7s %s\n     %s"
                   (sheprd--status-glyph status)
                   (symbol-name client)
                   (symbol-name status)
                   (truncate-string-to-width
                    (sheprd--short-buffer-name buffer)
                    (max 8 (- sheprd-sidebar-width 7)) nil nil "…"))
           'type 'sheprd-agent-button
           'sheprd-session session
           'sheprd-buffer buffer
           'sheprd-client client
           'face face)
          (insert "\n"))))
    (insert (propertize "\n RET open · d diff · a launch\n ! blocked · s status · ? help\n"
                        'face 'shadow))))

(defun sheprd--render-sidebar ()
  "Render the whole sidebar into the current buffer."
  (let ((entries (sheprd--agent-entries)))
    (sheprd--render-sessions entries)
    (sheprd--render-agents entries)))

(defun sheprd--configure-window (window)
  "Apply persistent sidebar presentation settings to WINDOW."
  (when (window-live-p window)
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-parameter window 'no-other-window t)
    (set-window-margins window 0 0)
    ;; Preserve only the width.  Preserving the height stops GUI minibuffers
    ;; (notably Vertico) from growing to show their candidate list.
    (window-preserve-size window t t)
    (window-preserve-size window nil nil)))

(defun sheprd--display ()
  "Render and display the Sheprd sidebar on the selected frame."
  (unless sheprd--inhibit-display
    (let ((buffer (sheprd--frame-sidebar)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'sheprd-sidebar-mode)
          (sheprd-sidebar-mode))
        (let ((key (sheprd--button-key-at-point))
              (inhibit-read-only t))
          (erase-buffer)
          (sheprd--render-sidebar)
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (sheprd--restore-button key)
          ;; `erase-buffer' drags the point of every window showing the panel
          ;; back to the top, so hand them the restored position too.
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-point window (point)))))
      (unless (active-minibuffer-window)
        (sheprd--configure-window
         (display-buffer-in-side-window
          buffer
          `((side . ,sheprd-sidebar-side)
            (slot . 0)
            (window-width . ,sheprd-sidebar-width)
            (window-parameters . ((no-delete-other-windows . t)
                                  (no-other-window . t))))))))))

(defun sheprd-refresh (&rest _)
  "Refresh session and agent state in the Sheprd sidebar."
  (interactive)
  (when (and sheprd-mode (bound-and-true-p persp-mode))
    (sheprd--enforce-isolation)
    (dolist (frame (frame-list))
      (when (and (display-graphic-p frame) (frame-visible-p frame))
        (with-selected-frame frame
          (sheprd--display))))))

(defun sheprd--scheduled-refresh-callback ()
  "Run a coalesced Sheprd refresh."
  (setq sheprd--scheduled-refresh nil)
  (sheprd-refresh))

(defun sheprd--schedule-refresh (&rest _)
  "Schedule one coalesced Sheprd refresh after the current event."
  (when (and sheprd-mode (not (timerp sheprd--scheduled-refresh)))
    (setq sheprd--scheduled-refresh
          (run-at-time 0 nil #'sheprd--scheduled-refresh-callback))))

(defun sheprd-toggle-sidebar ()
  "Show or hide the Sheprd sidebar on this frame."
  (interactive)
  (if-let* ((window (sheprd--sidebar-window)))
      (let ((ignore-window-parameters t))
        (delete-window window))
    (unless sheprd-mode (sheprd-mode 1))
    (sheprd--display)))


;;; ---------------------------------------------------------------------------
;;; Sidebar actions

(defun sheprd--activate-session-button (button)
  "Switch to the session stored on BUTTON."
  (sheprd--switch-session (button-get button 'sheprd-session)))

(defun sheprd--show-pane-buffer (session buffer)
  "Switch to SESSION and show BUFFER in an editing pane."
  (unless (buffer-live-p buffer)
    (user-error "That terminal has exited"))
  (unless (equal session (sheprd-current-session))
    (sheprd--switch-session session))
  (if-let* ((window (get-buffer-window buffer (selected-frame))))
      (select-window window)
    (select-window (sheprd--main-window))
    (switch-to-buffer buffer))
  (sheprd--seen-here)
  (when (fboundp 'ghostel-force-redraw)
    (ignore-errors (ghostel-force-redraw)))
  (sheprd--schedule-refresh))

(defun sheprd--activate-agent-button (button)
  "Switch session and show the agent stored on BUTTON."
  (sheprd--show-pane-buffer (button-get button 'sheprd-session)
                            (button-get button 'sheprd-buffer)))

(defun sheprd--agent-button-at-point ()
  "Return the agent button on the current line, if any."
  (let ((button (or (button-at (point))
                    (next-button (line-beginning-position) t))))
    (when (and button
               (eq (button-type button) 'sheprd-agent-button)
               (< (button-start button) (line-end-position)))
      button)))

(defun sheprd-open-agent-diff ()
  "Open the unstaged Magit diff for the agent at point."
  (interactive)
  (let* ((button (or (sheprd--agent-button-at-point)
                     (user-error "Move point to an agent first")))
         (buffer (button-get button 'sheprd-buffer))
         (directory (and (buffer-live-p buffer)
                         (buffer-local-value 'default-directory buffer))))
    (unless (and directory (require 'magit-diff nil t))
      (user-error "Magit diff support is unavailable"))
    (let ((root (ignore-errors (magit-toplevel directory))))
      (unless root
        (user-error "The agent terminal is not inside a Git repository"))
      (sheprd--activate-agent-button button)
      (let ((default-directory root))
        (magit-diff-unstaged)))))

(defun sheprd-next-item ()
  "Move to the next clickable Sheprd item, wrapping at the end."
  (interactive)
  (forward-button 1 t t t))

(defun sheprd-previous-item ()
  "Move to the previous clickable Sheprd item, wrapping at the start."
  (interactive)
  (forward-button -1 t t t))

(defun sheprd-focus-sidebar ()
  "Show the sidebar and move point into it."
  (interactive)
  (unless sheprd-mode (sheprd-mode 1))
  (sheprd--display)
  (if-let* ((window (sheprd--sidebar-window)))
      (progn (select-window window)
             (goto-char (point-min))
             (forward-button 1 t t t))
    (user-error "The Sheprd sidebar is unavailable in this frame")))


;;; ---------------------------------------------------------------------------
;;; Agents, navigation and session-level commands

(defun sheprd-launch-agent (client directory)
  "Launch CLIENT in a fresh terminal in DIRECTORY.
The terminal belongs to the current session and to no other.  Arguments
are passed directly to the executable, without a shell or a startup
keystroke race."
  (interactive
   (list (intern (completing-read "Agent: " sheprd-agent-commands nil t))
         (read-directory-name "Agent directory: "
                              (with-selected-window (sheprd--main-window)
                                default-directory)
                              nil t)))
  (require 'ghostel)
  (unless (fboundp 'ghostel-exec)
    (user-error "This Ghostel version does not provide ghostel-exec"))
  (let* ((command (alist-get client sheprd-agent-commands))
         (session (sheprd-current-session))
         (default-directory (file-name-as-directory (expand-file-name directory))))
    (unless (and command (file-directory-p default-directory))
      (user-error "Choose a configured agent and an existing directory"))
    (unless (or (file-remote-p default-directory) (executable-find (car command)))
      (user-error "Agent executable is unavailable: %s" (car command)))
    (let ((buffer (generate-new-buffer (format "*sheprd:%s*" client))))
      (condition-case err
          (progn
            (with-current-buffer buffer
              (setq default-directory
                    (file-name-as-directory (expand-file-name directory))))
            (ghostel-exec buffer (car command) (cdr command))
            (with-current-buffer buffer
              (setq sheprd--tracked-client client))
            ;; Redisplay-based auto-registration can lag behind the first
            ;; sidebar refresh, especially on hidden or newly created frames.
            (sheprd--claim-buffer buffer session)
            (select-window (sheprd--main-window))
            (switch-to-buffer buffer)
            (sheprd--schedule-refresh)
            buffer)
        (error
         (unless (get-buffer-process buffer) (kill-buffer buffer))
         (signal (car err) (cdr err)))))))

(defun sheprd-track-agent (client)
  "Explicitly identify the current terminal as CLIENT.
Use this for remote sessions or wrappers that process detection cannot
recognize.  Select auto to return to automatic detection."
  (interactive
   (list (intern (completing-read "Track as: "
                                  (cons '(auto) sheprd-agent-commands) nil t))))
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Run this command in a terminal pane"))
  (setq sheprd--tracked-client (unless (eq client 'auto) client)
        sheprd--agent-client nil
        sheprd--agent-probe-time nil)
  (sheprd--schedule-refresh))

(defun sheprd-set-status (status)
  "Override the selected agent's inferred STATUS, or select auto."
  (interactive
   (list (intern (completing-read "Agent status: "
                                  '(auto working blocked done idle unknown) nil t))))
  (let ((buffer (if-let* ((button (sheprd--agent-button-at-point)))
                    (button-get button 'sheprd-buffer)
                  (current-buffer))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ghostel-mode)
        (user-error "Select an agent or run this in its terminal"))
      (setq sheprd--status-override (unless (eq status 'auto) status))))
  (sheprd--schedule-refresh))

(defun sheprd-attention ()
  "Jump to an agent that is blocked or newly done, across all sessions."
  (interactive)
  (let* ((entries (cl-remove-if-not
                   (lambda (entry) (memq (plist-get entry :status) '(blocked done)))
                   (sheprd--agent-entries)))
         (choices (mapcar (lambda (entry)
                            (cons (format "%s / %s / %s"
                                          (plist-get entry :session)
                                          (plist-get entry :status)
                                          (buffer-name (plist-get entry :buffer)))
                                  entry))
                          entries)))
    (unless choices (user-error "No agent needs attention"))
    (let ((entry (cdr (assoc (completing-read "Agent: " choices nil t) choices))))
      (sheprd--show-pane-buffer (plist-get entry :session)
                                (plist-get entry :buffer)))))

(defun sheprd-open-notification-target ()
  "Jump to the agent that raised the most recent notification."
  (interactive)
  (let ((buffer sheprd--notification-target))
    (unless (buffer-live-p buffer)
      (user-error "No notification to open"))
    (sheprd--show-pane-buffer (buffer-local-value 'sheprd--session buffer) buffer)))

(defun sheprd-goto ()
  "Pick any session or pane in one prompt and jump to it."
  (interactive)
  (let (choices)
    (dolist (session (sheprd-session-names))
      (push (cons (format "session  %s" session) (list 'session session)) choices)
      (dolist (buffer (sheprd--session-buffers session))
        (when (sheprd--terminal-buffer-p buffer)
          (push (cons (format "pane     %s / %s" session (buffer-name buffer))
                      (list 'pane session buffer))
                choices))))
    (setq choices (nreverse choices))
    (unless choices (user-error "Nothing to go to"))
    (pcase (cdr (assoc (completing-read "Go to: " choices nil t) choices))
      (`(session ,name) (sheprd--switch-session name))
      (`(pane ,name ,buffer) (sheprd--show-pane-buffer name buffer)))))

(defun sheprd-detach ()
  "Detach this client, leaving the server and every terminal running.
Herdr's `prefix+q' detaches a client from its background server; the
Emacs analogue is closing this frame while the daemon keeps the
processes alive."
  (interactive)
  (cond
   ((or (daemonp) (cdr (visible-frame-list)))
    (delete-frame))
   (t (user-error
       "Nothing to detach from: this is the only frame and Emacs is not a daemon"))))


;;; ---------------------------------------------------------------------------
;;; Keymaps

(defconst sheprd--key-help
  '(("Sessions"
     ("1..9" . "switch to session 1-9")
     ("0" . "switch to the final session")
     ("`" . "switch to the previous session")
     ("n" . "next session")
     ("p" . "previous session")
     ("w" . "session picker")
     ("B" . "switch buffer within this session")
     ("N" . "new session")
     ("W" . "rename session")
     ("D" . "close session and its terminals"))
    ("Panes"
     ("t" . "new terminal in this pane")
     ("v" . "split right")
     ("-" . "split down")
     ("h/j/k/l" . "focus pane left/down/up/right")
     ("H/J/K/L" . "swap pane left/down/up/right")
     ("TAB" . "cycle to the next pane")
     ("S-TAB" . "cycle to the previous pane")
     (";" . "last pane")
     ("x" . "close pane")
     ("z" . "toggle zoom")
     ("r" . "resize mode")
     ("P" . "rename pane")
     ("e" . "edit scrollback")
     ("[" . "copy mode"))
    ("Agents and session"
     ("a" . "launch an agent")
     ("!" . "jump to a blocked or done agent")
     ("o" . "open the last notification")
     ("s" . "override agent status")
     ("d" . "unstaged diff of the agent at point")
     ("g" . "goto picker")
     ("b" . "toggle sidebar")
     ("f" . "focus the sidebar")
     ("G" . "refresh")
     ("q" . "detach this client")
     ("?" . "this help")))
  "Sections and bindings shown by `sheprd-help'.")

(defun sheprd-help ()
  "Show every Sheprd binding, Herdr's `prefix+?'."
  (interactive)
  (let ((buffer (get-buffer-create "*Sheprd Help*"))
        (prefix sheprd-prefix-key))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Sheprd keys — prefix %s%s\n\n"
                        prefix
                        (if (and (boundp 'doom-leader-map) sheprd-leader-keys)
                            (format " or <leader> %s" (car sheprd-leader-keys))
                          "")))
        (dolist (section sheprd--key-help)
          (insert (propertize (format "%s\n" (car section)) 'face 'sheprd-heading-face))
          (dolist (binding (cdr section))
            (insert (format "  %-10s %s\n" (car binding) (cdr binding))))
          (insert "\n")))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buffer)))

(defun sheprd--index-command (function index)
  "Return an interactive closure calling FUNCTION with INDEX."
  (lambda () (interactive) (funcall function index)))

(defvar sheprd-command-map (make-sparse-keymap)
  "Sheprd's prefix map, the Emacs equivalent of Herdr's prefix mode.
Populated by `sheprd--populate-command-map', not by this definition, so
that re-loading sheprd.el refreshes the bindings in place instead of
leaving a stale keymap behind the prefix.")

(defun sheprd--populate-command-map (map)
  "Fill MAP with Sheprd's bindings, discarding whatever it held before.
Clearing first matters on reload: a key that no longer has a command
would otherwise keep firing the one it had in the previous version."
  (setcdr map nil)
  ;; Sessions.  Herdr numbers tabs here; Sheprd numbers sessions, because a
  ;; session is the thing worth reaching in one keystroke.
  (dotimes (index 9)
    (define-key map (number-to-string (1+ index))
                (sheprd--index-command #'sheprd-switch-session-index (1+ index))))
  (define-key map (kbd "0") #'sheprd-final-session)
  (define-key map (kbd "`") #'sheprd-last-session)
  (define-key map (kbd "n") #'sheprd-next-session)
  (define-key map (kbd "p") #'sheprd-previous-session)
  (define-key map (kbd "<") #'sheprd-previous-session)
  (define-key map (kbd ">") #'sheprd-next-session)
  (define-key map (kbd "w") #'sheprd-switch-session)
  (define-key map (kbd "B") #'sheprd-switch-buffer)
  (define-key map (kbd "N") #'sheprd-new-session)
  (define-key map (kbd "W") #'sheprd-rename-session)
  (define-key map (kbd "D") #'sheprd-kill-session)
  ;; Panes.
  (define-key map (kbd "t") #'sheprd-new-terminal)
  (define-key map (kbd "v") #'sheprd-split-right)
  (define-key map (kbd "-") #'sheprd-split-down)
  (define-key map (kbd "h") #'sheprd-focus-pane-left)
  (define-key map (kbd "j") #'sheprd-focus-pane-down)
  (define-key map (kbd "k") #'sheprd-focus-pane-up)
  (define-key map (kbd "l") #'sheprd-focus-pane-right)
  (define-key map (kbd "H") #'sheprd-swap-pane-left)
  (define-key map (kbd "J") #'sheprd-swap-pane-down)
  (define-key map (kbd "K") #'sheprd-swap-pane-up)
  (define-key map (kbd "L") #'sheprd-swap-pane-right)
  (define-key map (kbd "TAB") #'sheprd-cycle-pane-next)
  (define-key map (kbd "<tab>") #'sheprd-cycle-pane-next)
  (define-key map (kbd "<backtab>") #'sheprd-cycle-pane-previous)
  (define-key map (kbd ";") #'sheprd-last-pane)
  (define-key map (kbd "x") #'sheprd-close-pane)
  (define-key map (kbd "z") #'sheprd-zoom)
  (define-key map (kbd "r") #'sheprd-resize-mode)
  (define-key map (kbd "P") #'sheprd-rename-pane)
  (define-key map (kbd "e") #'sheprd-edit-scrollback)
  (define-key map (kbd "[") #'sheprd-copy-mode)
  ;; Agents, navigation and the client itself.
  (define-key map (kbd "a") #'sheprd-launch-agent)
  (define-key map (kbd "!") #'sheprd-attention)
  (define-key map (kbd "o") #'sheprd-open-notification-target)
  (define-key map (kbd "s") #'sheprd-set-status)
  (define-key map (kbd "d") #'sheprd-open-agent-diff)
  (define-key map (kbd "g") #'sheprd-goto)
  (define-key map (kbd "b") #'sheprd-toggle-sidebar)
  (define-key map (kbd "f") #'sheprd-focus-sidebar)
  (define-key map (kbd "G") #'sheprd-refresh)
  (define-key map (kbd "q") #'sheprd-detach)
  (define-key map (kbd "?") #'sheprd-help)
  map)

(sheprd--populate-command-map sheprd-command-map)

(defun sheprd-install-keys ()
  "Bind `sheprd-command-map' to the prefix key and to Doom's leader."
  (interactive)
  (when (and (stringp sheprd-prefix-key) (not (string-empty-p sheprd-prefix-key)))
    (global-set-key (kbd sheprd-prefix-key) sheprd-command-map))
  (when (boundp 'doom-leader-map)
    (dolist (key sheprd-leader-keys)
      (define-key doom-leader-map (kbd key) sheprd-command-map))))


;;; ---------------------------------------------------------------------------
;;; Sidebar mode

(define-derived-mode sheprd-sidebar-mode special-mode "Sheprd"
  "Major mode for the Sheprd sidebar."
  (setq-local truncate-lines t
              mode-line-format nil
              cursor-type 'box
              show-trailing-whitespace nil)
  (setq-local buffer-face-mode-face '(:inherit default))
  (buffer-face-mode 1)
  (hl-line-mode 1))

(defun sheprd--populate-sidebar-map (map)
  "Fill MAP with the sidebar bindings, discarding whatever it held before."
  (setcdr map nil)
  (define-key map (kbd "j") #'sheprd-next-item)
  (define-key map (kbd "n") #'sheprd-next-item)
  (define-key map (kbd "TAB") #'sheprd-next-item)
  (define-key map (kbd "<tab>") #'sheprd-next-item)
  (define-key map (kbd "k") #'sheprd-previous-item)
  (define-key map (kbd "p") #'sheprd-previous-item)
  (define-key map (kbd "<backtab>") #'sheprd-previous-item)
  (define-key map (kbd "g") #'sheprd-refresh)
  (define-key map (kbd "d") #'sheprd-open-agent-diff)
  (define-key map (kbd "a") #'sheprd-launch-agent)
  (define-key map (kbd "B") #'sheprd-switch-buffer)
  (define-key map (kbd "N") #'sheprd-new-session)
  (define-key map (kbd "D") #'sheprd-kill-session)
  (define-key map (kbd "!") #'sheprd-attention)
  (define-key map (kbd "s") #'sheprd-set-status)
  (define-key map (kbd "?") #'sheprd-help)
  (define-key map (kbd "q") #'sheprd-toggle-sidebar)
  (define-key map (kbd "SPC") #'doom/leader)
  (define-key map (kbd "RET") #'push-button)
  (define-key map (kbd "<return>") #'push-button)

  (dotimes (index 9)
    (define-key map (number-to-string (1+ index))
                (sheprd--index-command #'sheprd-switch-session-index (1+ index))))
  (define-key map (kbd "0") #'sheprd-final-session)
  map)

(sheprd--populate-sidebar-map sheprd-sidebar-mode-map)


;;; ---------------------------------------------------------------------------
;;; Lifecycle

(defun sheprd--hide-windows ()
  "Remove Sheprd sidebars and their buffers on every frame."
  (dolist (frame (frame-list))
    (when-let* ((buffer (frame-parameter frame 'sheprd-sidebar)))
      (when (buffer-live-p buffer)
        (dolist (window (get-buffer-window-list buffer nil t))
          (when (window-live-p window)
            (let ((ignore-window-parameters t))
              (delete-window window))))
        (kill-buffer buffer)))
    (set-frame-parameter frame 'sheprd-sidebar nil)))

(defun sheprd--delete-frame (frame)
  "Release the private sidebar buffer belonging to FRAME."
  (when-let* ((buffer (frame-parameter frame 'sheprd-sidebar)))
    (when (buffer-live-p buffer) (kill-buffer buffer))))

(defun sheprd--ghostel-mode-hook ()
  "Claim a terminal opened outside Sheprd for the current session."
  (when (bound-and-true-p persp-mode)
    (sheprd--claim-buffer (current-buffer))))

(defun sheprd--window-selection-change (frame)
  "Clear the done badge of agents newly displayed on FRAME."
  (when sheprd-mode
    (dolist (window (window-list frame 'never))
      (let ((buffer (window-buffer window)))
        (when (and (sheprd--terminal-buffer-p buffer)
                   (buffer-local-value 'sheprd--unseen-completion buffer))
          (with-current-buffer buffer (setq sheprd--unseen-completion nil))
          (sheprd--schedule-refresh))))))

(defun sheprd--enable ()
  "Install Sheprd hooks and begin refreshing."
  (add-hook 'persp-activated-functions #'sheprd--schedule-refresh)
  (add-hook 'persp-names-cache-changed-functions #'sheprd--schedule-refresh)
  (add-hook 'ghostel-command-start-functions #'sheprd--ghostel-command-start)
  (add-hook 'ghostel-command-finish-functions #'sheprd--ghostel-command-finish)
  (add-hook 'ghostel-exit-functions #'sheprd--ghostel-command-finish)
  (add-hook 'ghostel-mode-hook #'sheprd--ghostel-mode-hook)
  (add-hook 'window-selection-change-functions #'sheprd--window-selection-change)
  (when (timerp sheprd--refresh-timer)
    (cancel-timer sheprd--refresh-timer))
  (setq sheprd--refresh-timer
        (run-with-timer 0 sheprd-refresh-interval #'sheprd-refresh)))

(defun sheprd--disable ()
  "Remove Sheprd hooks, timers and windows."
  (remove-hook 'persp-activated-functions #'sheprd--schedule-refresh)
  (remove-hook 'persp-names-cache-changed-functions #'sheprd--schedule-refresh)
  (remove-hook 'ghostel-command-start-functions #'sheprd--ghostel-command-start)
  (remove-hook 'ghostel-command-finish-functions #'sheprd--ghostel-command-finish)
  (remove-hook 'ghostel-exit-functions #'sheprd--ghostel-command-finish)
  (remove-hook 'ghostel-mode-hook #'sheprd--ghostel-mode-hook)
  (remove-hook 'window-selection-change-functions #'sheprd--window-selection-change)
  (when (timerp sheprd--refresh-timer)
    (cancel-timer sheprd--refresh-timer))
  (when (timerp sheprd--scheduled-refresh)
    (cancel-timer sheprd--scheduled-refresh))
  (setq sheprd--refresh-timer nil
        sheprd--scheduled-refresh nil)
  (sheprd--hide-windows))

(define-minor-mode sheprd-mode
  "Herdr-style sessions, panes and coding agents.
The sidebar is available only in graphical Emacs frames."
  :global t
  :group 'sheprd
  (if sheprd-mode
      (if (display-graphic-p)
          (sheprd--enable)
        (setq sheprd-mode nil)
        (user-error "Sheprd's sidebar is available only in graphical Emacs"))
    (sheprd--disable)))

(defun sheprd--start-with-perspectives ()
  "Enable Sheprd when persp-mode becomes active."
  (if (bound-and-true-p persp-mode)
      (when (display-graphic-p) (sheprd-mode 1))
    (when sheprd-mode (sheprd-mode -1))))

(defun sheprd--after-make-frame (frame)
  "Initialize the sidebar in a new graphical FRAME, daemon clients included."
  (with-selected-frame frame
    (when (and (display-graphic-p) (bound-and-true-p persp-mode))
      (unless sheprd-mode (sheprd-mode 1))
      (sheprd-refresh))))

(add-hook 'persp-mode-hook #'sheprd--start-with-perspectives)
(add-hook 'after-make-frame-functions #'sheprd--after-make-frame)
(add-hook 'delete-frame-functions #'sheprd--delete-frame)

(with-eval-after-load 'evil
  (evil-set-initial-state 'sheprd-sidebar-mode 'emacs))

(sheprd-install-keys)

(when (and (display-graphic-p)
           (bound-and-true-p persp-mode))
  (sheprd-mode 1))

(provide 'sheprd)
;;; sheprd.el ends here
