;;; obelisk-ssh.el --- Prompted TRAMP connections -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'tramp)

(defvar obelisk-ssh-user-history nil
  "Previously entered SSH usernames.")
(defvar obelisk-ssh-host-history nil
  "Previously entered SSH IP addresses and hostnames.")

(with-eval-after-load 'savehist
  (dolist (variable '(obelisk-ssh-user-history obelisk-ssh-host-history))
    (add-to-list 'savehist-additional-variables variable)))

(defun obelisk-ssh (user host &optional port)
  "Open USER's home directory on HOST using SSH and TRAMP.
Prompt with completion from previous usernames and hosts.  PORT defaults
to 22.  History persists when `savehist-mode' is enabled, as in Doom.
This opens Dired, not an interactive terminal."
  (interactive
   (list (completing-read "SSH username: " obelisk-ssh-user-history
                          nil nil nil 'obelisk-ssh-user-history)
         (completing-read "SSH IP or hostname: " obelisk-ssh-host-history
                          nil nil nil 'obelisk-ssh-host-history)
         (read-string "SSH port: " nil nil "22")))
  (setq user (string-trim user)
        host (string-trim host)
        port (if (null port) "22" (format "%s" port)))
  ;; Reject TRAMP delimiters so these prompts cannot introduce hops or paths.
  (unless (string-match-p "\\`[[:alnum:]_.-]+\\'" user)
    (user-error "Enter a nonempty SSH username (letters, digits, _, . or -)"))
  (unless (and (not (string-empty-p host))
               (string-match-p "\\`[[:alnum:]_.:%-]+\\'" host))
    (user-error "Enter an IP address or hostname without a port or brackets"))
  (unless (and (string-match-p "\\`[0-9]+\\'" port)
               (<= 1 (string-to-number port) 65535))
    (user-error "SSH port must be an integer between 1 and 65535"))
  (add-to-history 'obelisk-ssh-user-history user)
  (add-to-history 'obelisk-ssh-host-history host)
  (dired
   (tramp-make-tramp-file-name
    (make-tramp-file-name :method "ssh" :user user :host host
                          :port (number-to-string (string-to-number port))
                          :localname "~/"))))

(provide 'obelisk-ssh)
;;; obelisk-ssh.el ends here
