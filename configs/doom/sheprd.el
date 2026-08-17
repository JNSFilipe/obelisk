;;; sheprd.el --- Herdr-like workspaces and agent sidebar -*- lexical-binding: t; -*-

;; This file intentionally uses Doom's public workspace helpers, but otherwise
;; stays independent of Doom macros so it can be byte-compiled and tested on its
;; own.  It expects Doom's :ui workspaces module and Ghostel.

;;; Commentary:

;; Sheprd treats Doom/persp-mode workspaces as Herdr-style spaces.  Two stacked
;; side windows form a persistent left rail: spaces above and active Codex or
;; Claude Code Ghostel sessions below.  Agent state is inferred from Ghostel's
;; live terminal snapshot, including terminals hidden in another perspective.
;;
;; Doom already binds M-1..M-9 and SPC TAB 1..9 to workspace selection.  Inside
;; either Sheprd panel, 1..9 do the same; j/k and TAB move between buttons.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)

(declare-function +workspace-current-name "ui/workspaces/autoload/workspaces")
(declare-function +workspace-get "ui/workspaces/autoload/workspaces" (name &optional noerror))
(declare-function +workspace-list-names "ui/workspaces/autoload/workspaces")
(declare-function +workspace-buffer-list "ui/workspaces/autoload/workspaces" (&optional persp))
(declare-function +workspace/switch-to "ui/workspaces/autoload/workspaces" (index))
(declare-function +workspace/switch-to-final "ui/workspaces/autoload/workspaces")
(declare-function ghostel--copy-all-text "ghostel-module" (term))
(declare-function ghostel-force-redraw "ghostel")
(declare-function magit-diff-unstaged "magit-diff" (&optional args files))
(declare-function magit-toplevel "magit" (&optional directory))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function doom/leader "doom-keybinds")

(defvar persp-mode)
(defvar persp-activated-functions)
(defvar persp-names-cache-changed-functions)
(defvar ghostel-command-start-functions)
(defvar ghostel-command-finish-functions)
(defvar ghostel-exit-functions)
(defvar ghostel--process)
(defvar ghostel--term)
(defvar ghostel--command-running)
(defvar ghostel--last-output-time)
(defvar sheprd-mode)

(defgroup sheprd nil
  "Herdr-like Doom workspaces and coding-agent navigation."
  :group 'convenience
  :prefix "sheprd-")

(defcustom sheprd-sidebar-width 30
  "Width, in columns, of the Sheprd left rail."
  :type 'integer)

(defcustom sheprd-refresh-interval 2.0
  "Seconds between coding-agent status refreshes."
  :type 'number)

(defcustom sheprd-status-scan-chars 8000
  "Maximum number of trailing terminal characters inspected per refresh."
  :type 'integer)

(defcustom sheprd-client-command-regexps
  '((codex . "\\(?:\\`\\|[^[:alnum:]_]\\)codex\\(?:\\'\\|[^[:alnum:]_]\\)")
    (claude . "\\(?:\\`\\|[^[:alnum:]_]\\)claude\\(?:\\'\\|[^[:alnum:]_]\\)"))
  "Regexps used to recognize an agent command submitted in Ghostel.
Add aliases here if, for example, Claude Code is launched through a wrapper."
  :type '(alist :key-type symbol :value-type regexp))

(defcustom sheprd-client-screen-regexps
  '((codex . "\\(?:openai[[:space:]]+codex\\|codex cli\\|context .*used\\)")
    (claude . "\\(?:claude code\\|claude[[:space:]]+v[0-9]\\)"))
  "Fallback regexps used to recognize an already-running agent screen."
  :type '(alist :key-type symbol :value-type regexp))

(defcustom sheprd-waiting-regexp
  (concat
   "\\(?:do you want to proceed\\|would you like\\|needs? your approval\\|"
   "permission required\\|allow this\\|approve\\|press enter to confirm\\|"
   "select an option\\|yes, and don't ask again\\|esc to cancel\\|continue\\?\\)")
  "Regexp indicating that an agent is blocked on explicit user input."
  :type 'regexp)

(defcustom sheprd-thinking-regexp
  (concat
   "\\(?:esc to interrupt\\|ctrl[-+]c to interrupt\\|running tool\\|"
   "^[[:space:]•●✻⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]*"
   "\\(?:working\\|thinking\\|baking\\|processing\\)[ .…(]\\)")
  "Regexp indicating that an agent is actively working."
  :type 'regexp)

(defface sheprd-heading-face
  '((t (:inherit font-lock-keyword-face :weight bold :height 1.05)))
  "Face for Sheprd panel headings."
  :group 'sheprd)

(defface sheprd-workspace-face
  '((t (:inherit default)))
  "Face for an inactive Sheprd workspace."
  :group 'sheprd)

(defface sheprd-workspace-active-face
  '((t (:inherit mode-line-emphasis :weight bold)))
  "Face for the current Sheprd workspace."
  :group 'sheprd)

(defface sheprd-agent-thinking-face
  '((t (:inherit success :weight semi-bold)))
  "Face for an agent that is thinking."
  :group 'sheprd)

(defface sheprd-agent-waiting-face
  '((t (:inherit warning :weight bold)))
  "Face for an agent waiting for input."
  :group 'sheprd)

(defface sheprd-agent-stopped-face
  '((t (:inherit shadow)))
  "Face for an agent that is stopped at its prompt."
  :group 'sheprd)

(defface sheprd-workspace-group-face
  '((t (:inherit font-lock-comment-face :weight semi-bold)))
  "Face for workspace group labels in the agents panel."
  :group 'sheprd)

(defconst sheprd--spaces-buffer " *sheprd-spaces*")
(defconst sheprd--agents-buffer " *sheprd-agents*")

(defvar sheprd--refresh-timer nil)
(defvar sheprd--scheduled-refresh nil)
(defvar-local sheprd--panel-kind nil)
(defvar-local sheprd--agent-client nil)
(defvar-local sheprd--snapshot-output-time nil)
(defvar-local sheprd--cached-terminal-tail nil)

(defun sheprd--tail (text length)
  "Return at most LENGTH trailing characters from TEXT."
  (if (> (length text) length)
      (substring text (- length))
    text))

(defun sheprd--terminal-text (buffer)
  "Return BUFFER's live Ghostel terminal text without changing its display."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((output-time (and (boundp 'ghostel--last-output-time)
                              ghostel--last-output-time)))
        (if (and output-time
                 (equal output-time sheprd--snapshot-output-time)
                 sheprd--cached-terminal-tail)
            sheprd--cached-terminal-tail
          (let* ((text
                  (or (and (bound-and-true-p ghostel--term)
                           (fboundp 'ghostel--copy-all-text)
                           (ignore-errors
                             (ghostel--copy-all-text ghostel--term)))
                      (buffer-substring-no-properties (point-min) (point-max))))
                 (tail (sheprd--tail text sheprd-status-scan-chars)))
            (setq sheprd--snapshot-output-time output-time
                  sheprd--cached-terminal-tail tail)
            tail))))))

(defun sheprd--client-from-text (text regexps)
  "Return the first client in REGEXPS recognized in TEXT."
  (when text
    (let ((case-fold-search t))
      (cl-loop for (client . regexp) in regexps
               when (string-match-p regexp text)
               return client))))

(defun sheprd--question-at-prompt-p (text)
  "Return non-nil when trailing TEXT looks like an agent question at a prompt."
  (let ((case-fold-search t)
        (tail (sheprd--tail text 900)))
    (string-match-p
     "\\?\\(?:.\\|\n\\)\\{0,300\\}\\(?:❯\\|›\\|>\\)[[:space:]]*\\'"
     tail)))

(defun sheprd--agent-status (text)
  "Infer an active agent's status from terminal TEXT."
  (let ((case-fold-search t)
        (tail (sheprd--tail (or text "") sheprd-status-scan-chars)))
    (cond
     ((or (string-match-p sheprd-waiting-regexp tail)
          (sheprd--question-at-prompt-p tail))
      'waiting-input)
     ((string-match-p sheprd-thinking-regexp tail) 'thinking)
     (t 'stopped))))

(defun sheprd--ghostel-live-p ()
  "Return non-nil when the current Ghostel buffer has a live process."
  (and (derived-mode-p 'ghostel-mode)
       (boundp 'ghostel--process)
       (process-live-p ghostel--process)))

(defun sheprd--agent-info (buffer)
  "Return (CLIENT STATUS) when BUFFER hosts an active supported agent."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (sheprd--ghostel-live-p)
                 (bound-and-true-p ghostel--command-running))
        (let* ((text (sheprd--terminal-text buffer))
               (screen (sheprd--tail (or text "") sheprd-status-scan-chars))
               (client (or sheprd--agent-client
                           (sheprd--client-from-text
                            screen sheprd-client-screen-regexps))))
          (when client
            (list client (sheprd--agent-status screen))))))))

(defun sheprd--ghostel-command-start (buffer)
  "Record a supported agent command starting in Ghostel BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq sheprd--cached-terminal-tail nil)
      (setq sheprd--agent-client
            (sheprd--client-from-text
             (sheprd--tail (or (sheprd--terminal-text buffer) "") 1600)
             sheprd-client-command-regexps))))
  (sheprd--schedule-refresh))

(defun sheprd--ghostel-command-finish (buffer &rest _)
  "Clear agent metadata when Ghostel BUFFER's command finishes."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq sheprd--agent-client nil
            sheprd--cached-terminal-tail nil)))
  (sheprd--schedule-refresh))

(defun sheprd--workspace-names ()
  "Return open Doom workspace names, or nil before workspaces initialize."
  (when (fboundp '+workspace-list-names)
    (ignore-errors (+workspace-list-names))))

(defun sheprd--current-workspace-name ()
  "Return the selected frame's current Doom workspace name."
  (when (fboundp '+workspace-current-name)
    (ignore-errors (+workspace-current-name))))

(defun sheprd--agent-entries ()
  "Return agent entry plists grouped in Doom workspace order."
  (cl-loop
   for workspace in (sheprd--workspace-names)
   append
   (when-let* ((persp (ignore-errors (+workspace-get workspace t)))
               (buffers (ignore-errors (+workspace-buffer-list persp)))
               (buffers (cl-remove-if-not #'buffer-live-p buffers)))
     (cl-loop
      for buffer in (sort (copy-sequence buffers)
                          (lambda (a b)
                            (string< (buffer-name a) (buffer-name b))))
      for info = (sheprd--agent-info buffer)
      when info
      collect (list :workspace workspace
                    :buffer buffer
                    :client (car info)
                    :status (cadr info))))))

(defun sheprd--status-display (status)
  "Return (GLYPH LABEL FACE) for STATUS."
  (pcase status
    ('thinking '("●" "thinking" sheprd-agent-thinking-face))
    ('waiting-input '("◉" "waiting input" sheprd-agent-waiting-face))
    (_ '("○" "stopped" sheprd-agent-stopped-face))))

(defun sheprd--short-buffer-name (buffer)
  "Return a compact human-readable name for Ghostel BUFFER."
  (let* ((name (buffer-name buffer))
         (name (string-trim name "[ *]+" "[ *]+"))
         (name (replace-regexp-in-string "\\`ghostel:? *" "" name t t)))
    (if (string-empty-p name) "terminal" name)))

(define-button-type 'sheprd-workspace-button
  'follow-link t
  'mouse-face 'highlight
  'help-echo "mouse-1/RET: switch workspace"
  'action #'sheprd--activate-workspace-button)

(define-button-type 'sheprd-agent-button
  'follow-link t
  'mouse-face 'highlight
  'help-echo "mouse-1/RET: open agent terminal; d: open git diff"
  'action #'sheprd--activate-agent-button)

(defun sheprd--button-key-at-point ()
  "Return the stable identity of the button at point, if any."
  (when-let* ((button (button-at (point))))
    (pcase (button-type button)
      ('sheprd-workspace-button
       (list 'workspace (button-get button 'sheprd-workspace)))
      ('sheprd-agent-button
       (list 'agent
             (button-get button 'sheprd-workspace)
             (button-get button 'sheprd-buffer))))))

(defun sheprd--button-key (button)
  "Return the stable identity of BUTTON."
  (pcase (button-type button)
    ('sheprd-workspace-button
     (list 'workspace (button-get button 'sheprd-workspace)))
    ('sheprd-agent-button
     (list 'agent
           (button-get button 'sheprd-workspace)
           (button-get button 'sheprd-buffer)))))

(defun sheprd--restore-button (key)
  "Move point to the button identified by KEY, or the first button."
  (let ((button (next-button (point-min) t))
        found)
    (while (and button (not found))
      (if (equal key (sheprd--button-key button))
          (setq found button)
        (setq button (next-button (button-end button)))))
    (when (or found (setq found (next-button (point-min) t)))
      (goto-char (button-start found)))))

(defun sheprd--with-rendered-panel (buffer render-function)
  "Erase BUFFER, call RENDER-FUNCTION, and preserve its selected button."
  (with-current-buffer buffer
    (unless (derived-mode-p 'sheprd-panel-mode)
      (sheprd-panel-mode))
    (let ((key (sheprd--button-key-at-point))
          (inhibit-read-only t))
      (erase-buffer)
      (funcall render-function)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (sheprd--restore-button key))))

(defun sheprd--render-spaces ()
  "Render the spaces half of the Sheprd rail."
  (let ((names (sheprd--workspace-names))
        (current (sheprd--current-workspace-name)))
    (insert (propertize " SPACES\n\n" 'face 'sheprd-heading-face))
    (if (null names)
        (insert (propertize "  Workspaces starting…\n" 'face 'shadow))
      (cl-loop
       for workspace in names
       for index from 0
       for active = (equal workspace current)
       for prefix = (if active "●" "○")
       for shortcut = (if (< index 9) (number-to-string (1+ index)) "·")
       do
       (insert-text-button
        (format " %s %s  %s\n" prefix shortcut workspace)
        'type 'sheprd-workspace-button
        'sheprd-workspace workspace
        'face (if active
                  'sheprd-workspace-active-face
                'sheprd-workspace-face))))))

(defun sheprd--render-agents ()
  "Render the coding-agents half of the Sheprd rail."
  (let ((entries (sheprd--agent-entries))
        (current (sheprd--current-workspace-name))
        last-workspace)
    (insert (propertize " AGENTS\n\n" 'face 'sheprd-heading-face))
    (if (null entries)
        (progn
          (insert (propertize "  No active clients\n\n" 'face 'shadow))
          (insert (propertize "  Start codex or claude\n  in a Ghostel terminal.\n"
                              'face 'shadow)))
      (dolist (entry entries)
        (let* ((workspace (plist-get entry :workspace))
               (buffer (plist-get entry :buffer))
               (client (plist-get entry :client))
               (status (plist-get entry :status))
               (display (sheprd--status-display status))
               (face (nth 2 display)))
          (unless (equal workspace last-workspace)
            (when last-workspace (insert "\n"))
            (insert (propertize
                     (format " %s%s\n"
                             (if (equal workspace current) "▸ " "  ")
                             workspace)
                     'face 'sheprd-workspace-group-face))
            (setq last-workspace workspace))
          (insert-text-button
           (format "  %s %-7s %s\n     %s"
                   (car display) (symbol-name client) (cadr display)
                   (truncate-string-to-width
                    (sheprd--short-buffer-name buffer)
                    (max 8 (- sheprd-sidebar-width 7)) nil nil "…"))
           'type 'sheprd-agent-button
           'sheprd-workspace workspace
           'sheprd-buffer buffer
           'sheprd-client client
           'face face)
          (insert "\n")))
      (insert (propertize "\n RET terminal · d diff\n" 'face 'shadow)))))

(defun sheprd--panel-buffer (name kind)
  "Return the initialized Sheprd panel NAME for KIND."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'sheprd-panel-mode)
        (sheprd-panel-mode))
      (setq-local sheprd--panel-kind kind))
    buffer))

(defun sheprd--configure-window (window)
  "Apply persistent sidebar presentation settings to WINDOW."
  (when (window-live-p window)
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-parameter window 'no-other-window t)
    (set-window-margins window 0 0)
    ;; Preserve only the rail width.  Preserving these side windows' heights
    ;; prevents GUI minibuffers (notably Vertico) from growing to show their
    ;; candidate list until the whole frame is resized.
    (window-preserve-size window t t)
    (window-preserve-size window nil nil)))

(defun sheprd--display ()
  "Render and display both halves of the Sheprd left rail."
  (let ((spaces (sheprd--panel-buffer sheprd--spaces-buffer 'spaces))
        (agents (sheprd--panel-buffer sheprd--agents-buffer 'agents)))
    (sheprd--with-rendered-panel spaces #'sheprd--render-spaces)
    (sheprd--with-rendered-panel agents #'sheprd--render-agents)
    (unless (active-minibuffer-window)
      (let ((spaces-window
             (display-buffer-in-side-window
              spaces
              `((side . left)
                (slot . 0)
                (window-width . ,sheprd-sidebar-width)
                (window-height . 0.5)
                (window-parameters . ((no-delete-other-windows . t)
                                      (no-other-window . t))))))
            (agents-window
             (display-buffer-in-side-window
              agents
              `((side . left)
                (slot . 1)
                (window-width . ,sheprd-sidebar-width)
                (window-height . 0.5)
                (window-parameters . ((no-delete-other-windows . t)
                                      (no-other-window . t)))))))
        (sheprd--configure-window spaces-window)
        (sheprd--configure-window agents-window)))))

(defun sheprd-refresh (&rest _)
  "Refresh workspace and agent state in the Sheprd rail."
  (interactive)
  (when (and sheprd-mode (bound-and-true-p persp-mode))
    (sheprd--display)))

(defun sheprd--scheduled-refresh-callback ()
  "Run a coalesced Sheprd refresh."
  (setq sheprd--scheduled-refresh nil)
  (sheprd-refresh))

(defun sheprd--schedule-refresh (&rest _)
  "Schedule one coalesced Sheprd refresh after the current event."
  (when (and sheprd-mode (not (timerp sheprd--scheduled-refresh)))
    (setq sheprd--scheduled-refresh
          (run-at-time 0 nil #'sheprd--scheduled-refresh-callback))))

(defun sheprd--main-window ()
  "Return a live non-side window on the selected frame."
  (or (cl-find-if (lambda (window)
                    (and (window-live-p window)
                         (not (window-minibuffer-p window))
                         (not (window-parameter window 'window-side))))
                  (window-list))
      (selected-window)))

(defun sheprd--activate-workspace-button (button)
  "Switch to the Doom workspace stored on BUTTON."
  (+workspace/switch-to (button-get button 'sheprd-workspace))
  (sheprd--schedule-refresh))

(defun sheprd--activate-agent-button (button)
  "Switch workspace and show the Ghostel agent stored on BUTTON."
  (let ((workspace (button-get button 'sheprd-workspace))
        (buffer (button-get button 'sheprd-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "That agent terminal has exited"))
    (+workspace/switch-to workspace)
    (select-window (sheprd--main-window))
    (switch-to-buffer buffer)
    (when (fboundp 'ghostel-force-redraw)
      (ghostel-force-redraw))
    (sheprd--schedule-refresh)))

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

(defun sheprd--switch-index (index)
  "Switch to zero-based Doom workspace INDEX."
  (+workspace/switch-to index)
  (sheprd--schedule-refresh))

(defun sheprd-focus-spaces ()
  "Focus the spaces half of the Sheprd rail."
  (interactive)
  (unless sheprd-mode (sheprd-mode 1))
  (sheprd-refresh)
  (when-let* ((window (get-buffer-window sheprd--spaces-buffer)))
    (select-window window)
    (goto-char (point-min))
    (forward-button 1 t t t)))

(defun sheprd-focus-agents ()
  "Focus the active coding-agents half of the Sheprd rail."
  (interactive)
  (unless sheprd-mode (sheprd-mode 1))
  (sheprd-refresh)
  (when-let* ((window (get-buffer-window sheprd--agents-buffer)))
    (select-window window)
    (goto-char (point-min))
    (unless (forward-button 1 t t t)
      (user-error "No active Codex or Claude Code clients"))))

(defun sheprd--hide-windows ()
  "Delete all live Sheprd side windows."
  (dolist (buffer-name (list sheprd--spaces-buffer sheprd--agents-buffer))
    (when-let* ((buffer (get-buffer buffer-name)))
      (dolist (window (get-buffer-window-list buffer nil t))
        (when (window-live-p window)
          (ignore-errors (delete-window window)))))))

(defun sheprd--enable ()
  "Install Sheprd hooks and begin refreshing."
  (add-hook 'persp-activated-functions #'sheprd--schedule-refresh)
  (add-hook 'persp-names-cache-changed-functions #'sheprd--schedule-refresh)
  (add-hook 'ghostel-command-start-functions #'sheprd--ghostel-command-start)
  (add-hook 'ghostel-command-finish-functions #'sheprd--ghostel-command-finish)
  (add-hook 'ghostel-exit-functions #'sheprd--ghostel-command-finish)
  (when (timerp sheprd--refresh-timer)
    (cancel-timer sheprd--refresh-timer))
  (setq sheprd--refresh-timer
        (run-with-timer 0 sheprd-refresh-interval #'sheprd-refresh)))

(defun sheprd--disable ()
  "Remove Sheprd hooks, timers, and side windows."
  (remove-hook 'persp-activated-functions #'sheprd--schedule-refresh)
  (remove-hook 'persp-names-cache-changed-functions #'sheprd--schedule-refresh)
  (remove-hook 'ghostel-command-start-functions #'sheprd--ghostel-command-start)
  (remove-hook 'ghostel-command-finish-functions #'sheprd--ghostel-command-finish)
  (remove-hook 'ghostel-exit-functions #'sheprd--ghostel-command-finish)
  (when (timerp sheprd--refresh-timer)
    (cancel-timer sheprd--refresh-timer))
  (when (timerp sheprd--scheduled-refresh)
    (cancel-timer sheprd--scheduled-refresh))
  (setq sheprd--refresh-timer nil
        sheprd--scheduled-refresh nil)
  (sheprd--hide-windows))

(define-derived-mode sheprd-panel-mode special-mode "Sheprd"
  "Major mode for the two Sheprd sidebar panels."
  (setq-local truncate-lines t
              mode-line-format nil
              cursor-type 'box
              show-trailing-whitespace nil)
  (setq-local buffer-face-mode-face '(:inherit default))
  (buffer-face-mode 1)
  (hl-line-mode 1))

(define-key sheprd-panel-mode-map (kbd "j") #'sheprd-next-item)
(define-key sheprd-panel-mode-map (kbd "n") #'sheprd-next-item)
(define-key sheprd-panel-mode-map (kbd "TAB") #'sheprd-next-item)
(define-key sheprd-panel-mode-map (kbd "<tab>") #'sheprd-next-item)
(define-key sheprd-panel-mode-map (kbd "k") #'sheprd-previous-item)
(define-key sheprd-panel-mode-map (kbd "p") #'sheprd-previous-item)
(define-key sheprd-panel-mode-map (kbd "<backtab>") #'sheprd-previous-item)
(define-key sheprd-panel-mode-map (kbd "g") #'sheprd-refresh)
(define-key sheprd-panel-mode-map (kbd "d") #'sheprd-open-agent-diff)
(define-key sheprd-panel-mode-map (kbd "q") #'sheprd-toggle)
(define-key sheprd-panel-mode-map (kbd "SPC") #'doom/leader)
(define-key sheprd-panel-mode-map (kbd "RET") #'push-button)
(define-key sheprd-panel-mode-map (kbd "<return>") #'push-button)

(dotimes (index 9)
  (let ((workspace-index index))
    (define-key sheprd-panel-mode-map (number-to-string (1+ index))
                (lambda ()
                  (interactive)
                  (sheprd--switch-index workspace-index)))))
(define-key sheprd-panel-mode-map (kbd "0") #'+workspace/switch-to-final)

(define-minor-mode sheprd-mode
  "Display Herdr-like Doom workspaces and active Ghostel coding agents."
  :global t
  :group 'sheprd
  (if sheprd-mode
      (sheprd--enable)
    (sheprd--disable)))

(defun sheprd-toggle ()
  "Toggle the Sheprd left rail."
  (interactive)
  (sheprd-mode (if sheprd-mode -1 1)))

(defun sheprd--start-with-workspaces ()
  "Enable Sheprd when Doom's persp-mode workspaces become active."
  (when (bound-and-true-p persp-mode)
    (sheprd-mode 1)))

(add-hook 'persp-mode-hook #'sheprd--start-with-workspaces)

(with-eval-after-load 'evil
  (evil-set-initial-state 'sheprd-panel-mode 'emacs))

(when (bound-and-true-p persp-mode)
  (sheprd-mode 1))

(provide 'sheprd)
;;; sheprd.el ends here
