;;; scratch-term.el -*- lexical-binding: t; -*-
;;;
;;; Ghostel helpers:
;;;   `+ghostel/here'           open/switch a Ghostel terminal in the current window
;;;   `+ghostel/scratch-toggle' a full-window, workspace-independent scratch terminal
;;;
;;; The scratch terminal is a single dedicated Ghostel buffer shown full-frame.
;;; Toggling it on saves the current window layout and replaces it; toggling it
;;; off (or ESC) restores that layout without killing the shell, so the next
;;; toggle resumes the same session.  The buffer is detached from every Doom
;;; workspace, so it is identical in each one and owned by none.

;;; --- Open a Ghostel terminal in the current window --------------------------

(defun +ghostel/here (&optional arg)
  "Open (or switch to) a Ghostel terminal in the CURRENT window.
ARG is passed straight to `ghostel': a plain \\[universal-argument] makes a
new buffer, a numeric prefix targets/creates the numbered buffer."
  (interactive "P")
  (require 'ghostel)
  ;; Force same-window display regardless of Ghostel's own display action.
  (let ((display-buffer-overriding-action
         '((display-buffer-same-window) (inhibit-same-window . nil))))
    (ghostel arg)))

;;; --- Full-window scratch terminal -------------------------------------------

(defvar +ghostel-scratch-buffer-name "*scratch-ghostel*"
  "Name of the dedicated Ghostel buffer used by the scratch terminal.")

(defvar +ghostel-scratch--saved-config nil
  "Window configuration to restore when the scratch terminal is toggled off.")

(defvar +ghostel-scratch-mode-map (make-sparse-keymap)
  "Keymap active only in the scratch terminal.
ESC is installed separately via `evil-local-set-key' because evil-ghostel
routes ESC through evil's state maps, which outrank a plain minor-mode map.")

(define-minor-mode +ghostel-scratch-mode
  "Minor mode marking the scratch terminal buffer."
  :keymap +ghostel-scratch-mode-map)

(defun +ghostel-scratch--install-escape ()
  "Bind ESC to toggle the scratch terminal off, overriding evil-ghostel's ESC.
Bound buffer-locally across evil states so ESC always closes the scratch
terminal regardless of the current state."
  (when (fboundp 'evil-local-set-key)
    (dolist (state '(insert normal visual motion emacs))
      (evil-local-set-key state (kbd "<escape>") #'+ghostel/scratch-hide)))
  ;; Fallback for a non-evil session.
  (define-key +ghostel-scratch-mode-map (kbd "<escape>") #'+ghostel/scratch-hide))

(defun +ghostel-scratch--detach-from-workspaces ()
  "Remove the scratch buffer from every workspace so it lives in the global persp.
A free (nil-perspective) buffer is available in every workspace and is not
reaped when a workspace is killed."
  (when (and (bound-and-true-p persp-mode)
             (fboundp 'persp-persps)
             (fboundp 'persp-remove-buffer))
    (let ((buf (get-buffer +ghostel-scratch-buffer-name)))
      (when (buffer-live-p buf)
        (dolist (persp (persp-persps))
          (when (and persp
                     (fboundp 'persp-contain-buffer-p)
                     (persp-contain-buffer-p buf persp))
            ;; switch=nil: don't yank another buffer into the scratch window.
            (persp-remove-buffer buf persp nil nil)))))))

(defun +ghostel-scratch--spawn ()
  "Spawn the scratch Ghostel terminal into the selected window and return it.
Sizing follows the selected window, so call this with the full-frame window
selected."
  (require 'ghostel)
  (let ((ghostel-buffer-name +ghostel-scratch-buffer-name)
        (display-buffer-overriding-action '((display-buffer-same-window))))
    (ghostel))
  (let ((buf (get-buffer +ghostel-scratch-buffer-name)))
    (with-current-buffer buf
      (+ghostel-scratch-mode 1)
      (+ghostel-scratch--install-escape))
    ;; persp-mode re-adds the buffer to the current workspace on the redisplay
    ;; that follows this command, so detach once that has happened.  Nothing
    ;; re-adds it afterwards, so this makes it a permanently free buffer.
    (run-with-idle-timer 0 nil #'+ghostel-scratch--detach-from-workspaces)
    buf))

(defun +ghostel-scratch--showing-p ()
  "Non-nil when the scratch terminal currently fills the selected frame."
  (let ((buf (get-buffer +ghostel-scratch-buffer-name)))
    (and (buffer-live-p buf)
         (eq (window-buffer (selected-window)) buf))))

(defun +ghostel/scratch-show ()
  "Save the window layout and show the scratch terminal full-frame.
Spawns the shell on first use, or resumes the existing session."
  (interactive)
  (setq +ghostel-scratch--saved-config (current-window-configuration))
  ;; `delete-other-windows' cannot run from a side window (e.g. a sidebar), and
  ;; the terminal belongs in the main editing area, so move there first.
  (when (and (window-parameter (selected-window) 'window-side)
             (fboundp 'window-main-window))
    (let ((main (window-main-window)))
      (when (window-live-p main)
        (select-window main))))
  (let ((buf (get-buffer +ghostel-scratch-buffer-name)))
    ;; Keeps any sticky side windows; fills the main area with the terminal.
    (delete-other-windows)
    (if (buffer-live-p buf)
        (switch-to-buffer buf)
      ;; First use: spawn into the now-full window so it is sized right.
      (+ghostel-scratch--spawn))))

(defun +ghostel/scratch-hide ()
  "Restore the saved window layout, leaving the scratch shell running."
  (interactive)
  (if (window-configuration-p +ghostel-scratch--saved-config)
      (progn
        (set-window-configuration +ghostel-scratch--saved-config)
        (setq +ghostel-scratch--saved-config nil))
    ;; No saved layout (e.g. spawned indirectly): just step off the buffer.
    (when (+ghostel-scratch--showing-p)
      (if (one-window-p) (previous-buffer) (delete-window)))))

(defun +ghostel/scratch-toggle ()
  "Toggle the full-window scratch terminal, resuming the same session."
  (interactive)
  (if (+ghostel-scratch--showing-p)
      (+ghostel/scratch-hide)
    (+ghostel/scratch-show)))

(provide 'scratch-term)
;;; scratch-term.el ends here
