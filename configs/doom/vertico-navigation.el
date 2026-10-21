;;; vertico-navigation.el --- Component-wise minibuffer editing -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'vertico)

(defun obelisk-vertico-insert-level ()
  "Insert the current candidate, completing only one new file path level.
Use the first candidate when the prompt itself is selected.  For file and
project-file completion, stop after the next directory separator beyond
those already typed.  Other completion categories insert the full candidate.
Do not exit the minibuffer."
  (interactive)
  (if (not (memq (vertico--metadata-get 'category) '(file project-file)))
      (vertico-insert)
    (when (> vertico--total 0)
      (let* ((base-length (length vertico--base))
             (input (buffer-substring-no-properties (minibuffer-prompt-end) (point)))
             ;; Completion styles may expand abbreviated parent directories.
             ;; Count components after Vertico's immutable completion base,
             ;; rather than assuming input and candidate share a literal path.
             (levels (1+ (cl-count ?/ input :start (min base-length (length input))))))
        (vertico-insert)
        (when-let* ((end (save-excursion
                          (goto-char (+ (minibuffer-prompt-end) base-length))
                          (search-forward "/" nil t levels))))
          (delete-region end (point-max)))))))

;; Cover both terminal character events and graphical keyboard events.
(define-key vertico-map (kbd "TAB") #'obelisk-vertico-insert-level)
(define-key vertico-map [tab] #'obelisk-vertico-insert-level)
(define-key vertico-map (kbd "DEL") #'delete-backward-char)
(define-key vertico-map [backspace] #'delete-backward-char)

(provide 'vertico-navigation)
;;; vertico-navigation.el ends here
