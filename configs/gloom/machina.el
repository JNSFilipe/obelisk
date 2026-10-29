;;; machina.el --- M-x Machina agents in place of Sheprd -*- lexical-binding: t; -*-
;; Loaded instead of workspaces.el when `gloom-sessions' is nil, so persp-mode
;; and Sheprd stay out of the way.  Agents run Claude Code's terminal UI through
;; EAT.  The SQLite registry defaults to emacs-agents/ under Gloom's state
;; directory (`user-emacs-directory').  Meow steps aside in Machina's views and
;; EAT terminals; see `gloom-meow-native-modes' in editing.el.
(unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
  (error "M-x Machina needs an Emacs built with SQLite"))
(gloom-require 'eat)
(gloom-require 'mx-machina)

;; Interfaces: EAT runs Claude Code; agent-shell (ACP, reused from Doom's
;; store) runs the other agents.  vterm needs a native module Gloom does not
;; build, so it is hidden.
(setq mx-machina-vterm-profiles nil)

(defvar gloom-machina-acp-agents
  '((codex . agent-shell-openai-codex-acp-command)
    (gemini-cli . agent-shell-google-gemini-acp-command)
    (pi . agent-shell-pi-acp-command)
    (antigravity . agent-shell-antigravity-acp-command)
    (claude-code . agent-shell-anthropic-claude-acp-command))
  "agent-shell profiles offered to Machina, with their ACP command variable.
A profile appears only while its ACP command is installed; agent-shell
otherwise lists two dozen agents regardless.")

(defun gloom-machina-installed-acp-agents (configs)
  "Keep agent-shell CONFIGS listed in `gloom-machina-acp-agents' and installed."
  (seq-filter
   (lambda (config)
     (when-let* ((variable (alist-get (map-elt config :identifier)
                                      gloom-machina-acp-agents))
                 ((boundp variable))
                 (command (car (symbol-value variable))))
       (executable-find command)))
   configs))
(advice-add 'mx-machina-agent-shell-configs :filter-return
            #'gloom-machina-installed-acp-agents)

;; New agents always use the chosen checkout: the new-worktree question is
;; skipped and answered no.  Machina asks it with y-or-n-p from its private
;; reader; this narrows the change to that one question.
(defun gloom-machina-skip-worktree (read-session)
  "Call READ-SESSION, declining its new-worktree question without asking."
  (let ((y-or-n (symbol-function 'y-or-n-p)))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (prompt)
                 (unless (string-prefix-p "Create a new worktree" prompt)
                   (funcall y-or-n prompt)))))
      (funcall read-session))))
(advice-add 'mx-machina--read-new-session :around
            #'gloom-machina-skip-worktree)

;; Pi agents can also run in Pilish (the `pilish' interface).
(load (expand-file-name "machina-pilish.el" gloom-config-directory) nil t)

(defvar gloom-machina-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'mx-machina)
    (keymap-set map "n" #'mx-machina-new)
    (keymap-set map "b" #'mx-machina-board)
    (keymap-set map "d" #'mx-machina-dashboard)
    (keymap-set map "z" #'mx-machina-focus)
    (keymap-set map "]" #'mx-machina-next-attention)
    (keymap-set map "[" #'mx-machina-previous-attention)
    map)
  "M-x Machina commands, on the C-c a (SPC a) leader prefix.")
(keymap-set mode-specific-map "a" gloom-machina-map)

;; The sidebar keeps Gloom's explicit window focus keys and SPC leader.  Bound
;; on first use: editing.el, which defines the focus keys, loads after this file.
(defun gloom-machina-sidebar-keys ()
  (keymap-set mx-machina-sidebar-mode-map "SPC" mode-specific-map)
  (dolist (binding gloom-window-keys)
    (keymap-set mx-machina-sidebar-mode-map (car binding) (cdr binding))))
(add-hook 'mx-machina-sidebar-mode-hook #'gloom-machina-sidebar-keys)
(provide 'gloom-machina)
;;; machina.el ends here
