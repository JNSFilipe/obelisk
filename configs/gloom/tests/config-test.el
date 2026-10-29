;;; config-test.el --- Standalone integration scenarios -*- lexical-binding: t; -*-
(require 'ert)
(unless (featurep 'gloom-init)
  (load (expand-file-name "../init.el" (file-name-directory load-file-name)) nil t))

(ert-deftest gloom-startup-without-doom-with-evil ()
  (should evil-mode)
  (should gloom-override-mode)
  (should (featurep 'evil-collection))
  (should persp-mode)
  (should vertico-mode)
  (should marginalia-mode)
  (should (memq 'doom-oxocarbon custom-enabled-themes))
  (should-not (featurep 'doom))
  (should-not (featurep 'meow))
  (should-not (featurep 'perspective))
  (should (eq evil-undo-system 'undo-redo))
  (should (string-prefix-p gloom-state-directory persp-save-dir)))

(ert-deftest gloom-sessions-confine-terminals-and-restore-layouts ()
  (let ((origin (+workspace-current-name))
        (buffer (generate-new-buffer "test-terminal")))
    (unwind-protect
        (progn
          (+workspace-switch "test-a" t)
          (with-current-buffer buffer (setq major-mode 'ghostel-mode))
          (sheprd--claim-buffer buffer)
          (switch-to-buffer buffer)
          (split-window-right)
          (+workspace-switch "test-b" t)
          (should (= 1 (length (window-list))))
          ;; Even an accidental foreign addition is repaired by Sheprd.
          (persp-add-buffer buffer (get-current-persp) nil nil)
          (sheprd--confine-buffer buffer)
          (should-not (memq buffer (+workspace-buffer-list)))
          (let ((*persp-restrict-buffers-to* 0))
            (should-not (memq buffer (persp-buffer-list-restricted))))
          (+workspace-switch "test-a")
          (should (= 2 (length (window-list))))
          (should (memq buffer (+workspace-buffer-list)))
          (sheprd-rename-session "test-renamed")
          (should (equal "test-renamed" (buffer-local-value 'sheprd--session buffer)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (sheprd-kill-session "test-renamed"))
          (should-not (buffer-live-p buffer))
          (should-not (+workspace-exists-p "test-renamed")))
      (+workspace-switch origin t)
      (dolist (name '("test-a" "test-b" "test-renamed"))
        (when (+workspace-exists-p name) (+workspace-delete name)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gloom-evil-special-buffers-keep-native-keys ()
  (require 'ghostel)
  (with-temp-buffer
    (ghostel-mode)
    (should evil-ghostel-mode)
    (should (evil-insert-state-p))
    (should ghostel-ignore-cursor-change)
    (should (eq (key-binding (kbd "C-c s n")) #'sheprd-next-session))
    (should (eq (key-binding (kbd "M-SPC t")) #'sheprd-new-terminal))
    (should (eq (key-binding (kbd "C-s")) #'consult-line)))
  (with-temp-buffer
    (sheprd-sidebar-mode)
    (should (evil-emacs-state-p))
    (should (eq (key-binding (kbd "j")) #'sheprd-next-item))
    (should (eq (key-binding (kbd "SPC t")) #'sheprd-new-terminal)))
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (evil-normal-state-p))
    (should (equal (key-binding (kbd "w")) "viw"))
    (should (eq (key-binding (kbd "SPC t")) #'sheprd-new-terminal))
    (should (eq (key-binding (kbd "SPC ?")) #'gloom-leader-help))
    (should (eq (key-binding (kbd "C-h")) #'gloom-window-left))
    (should (eq (key-binding (kbd "gc")) #'evilnc-comment-operator))
    (should (eq (key-binding (kbd "M-x")) #'execute-extended-command))))

(ert-deftest gloom-workspace-persistence-restores-file-and-layout ()
  (let* ((origin (+workspace-current-name))
         (file (make-temp-file "gloom-workspace-" nil ".txt"))
         (state (make-temp-file "gloom-persp-"))
         buffer)
    (unwind-protect
        (progn
          (+workspace-switch "test-persist" t)
          (setq buffer (find-file-noselect file))
          (switch-to-buffer buffer)
          (split-window-right)
          (persp-save-state-to-file state *persp-hash* nil)
          (+workspace-switch origin)
          (+workspace-delete "test-persist")
          (kill-buffer buffer)
          (persp-load-from-file-by-names state *persp-hash* '("test-persist"))
          (+workspace-switch "test-persist")
          (should (= 2 (length (window-list))))
          (setq buffer (get-file-buffer file))
          (should (buffer-live-p buffer))
          (should (memq buffer (+workspace-buffer-list))))
      (+workspace-switch origin t)
      (when (+workspace-exists-p "test-persist") (+workspace-delete "test-persist"))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-file file)
      (delete-file state))))

(ert-deftest gloom-ghostel-evil-states ()
  (require 'ghostel)
  (save-window-excursion
    (let ((buffer (generate-new-buffer "gloom-native-modes")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (ghostel-mode)
            (setq ghostel--cursor-style 1 ghostel--cursor-blinking t)
            (ghostel--apply-cursor-style)
            (should-not ghostel--cursor-blink-timer)
            (should (evil-insert-state-p))
            (execute-kbd-macro (kbd "<escape>"))
            (should (evil-normal-state-p))
            ;; Normal state roams the terminal; the shell keeps its input mode.
            (should (eq ghostel--input-mode 'semi-char))
            (should (eq (key-binding (kbd "SPC t")) #'sheprd-new-terminal))
            (execute-kbd-macro (kbd "i"))
            (should (evil-insert-state-p))
            (should (eq ghostel--input-mode 'semi-char)))
        (kill-buffer buffer)))))

(ert-deftest gloom-navigation-includes-real-sheprd-terminal-and-sidebar ()
  (require 'ghostel)
  (let ((origin (+workspace-current-name))
        (session (format "gloom-navigation-%s" (float-time)))
        (terminal (generate-new-buffer "gloom-navigation-terminal"))
        (editor (generate-new-buffer "gloom-navigation-editor"))
        (ghostel-query-before-killing nil)
        (ghostel-kill-buffer-on-exit nil)
        (sheprd-sidebar-side 'left)
        (sheprd-sidebar-width 15)
        main right lower-left lower-right sidebar)
    (unwind-protect
        (progn
          (+workspace-switch session t)
          (switch-to-buffer editor)
          (emacs-lisp-mode)
          (setq main (selected-window)
                right (split-window-right)
                lower-left (split-window-below))
          (select-window right)
          (setq lower-right (split-window-below))
          (set-window-buffer lower-left editor)
          (set-window-buffer lower-right editor)
          (ghostel-exec terminal "/bin/sh"
                        '("-c" "printf 'ready\\n'; sleep 1; printf 'live-output\\n'; while IFS= read -r line; do printf 'received:%s\\n' \"$line\"; done"))
          (sheprd--claim-buffer terminal)
          (switch-to-buffer terminal)
          (sheprd--display)
          (setq sidebar (sheprd--sidebar-window))
          (should sidebar)
          (select-window main)
          (execute-kbd-macro (kbd "C-l"))
          (should (eq right (selected-window)))
          (should (evil-insert-state-p))
          (execute-kbd-macro (kbd "<escape>"))
          (should (evil-normal-state-p))
          (execute-kbd-macro (kbd "C-h"))
          (should (eq main (selected-window)))
          (execute-kbd-macro (kbd "C-l C-j"))
          (should (eq lower-right (selected-window)))
          (execute-kbd-macro (kbd "C-h"))
          (should (eq lower-left (selected-window)))
          (execute-kbd-macro (kbd "C-k C-h"))
          (should (eq sidebar (selected-window)))
          (should (eq (key-binding (kbd "j")) #'sheprd-next-item))
          (execute-kbd-macro (kbd "C-l C-l"))
          (should (eq right (selected-window)))
          (should (evil-normal-state-p))
          ;; Normal scrollback navigation must keep agent output streaming.
          (let ((deadline (+ (float-time) 5)))
            (while (and (< (float-time) deadline)
                        (not (string-match-p "live-output" (buffer-string))))
              (accept-process-output nil 0.05)))
          (should (string-match-p "live-output" (buffer-string)))
          (let (copied)
            (goto-char (point-min))
            (search-forward "live-output")
            (goto-char (match-beginning 0))
            (cl-letf (((symbol-function 'gui-set-selection)
                       (lambda (kind text)
                         (when (eq kind 'CLIPBOARD) (setq copied text)))))
              (execute-kbd-macro (kbd "v E y")))
            (should (equal copied "live-output")))
          (should (evil-normal-state-p))
          ;; i returns to the live prompt and types into the shell.
          (execute-kbd-macro (kbd "i gloom-input RET"))
          (should (evil-insert-state-p))
          (should (eq ghostel--input-mode 'semi-char))
          (should (eq (key-binding (kbd "C-k")) #'gloom-ghostel-kill-line))
          (should (eq (key-binding (kbd "C-j")) #'ghostel--send-event))
          (let ((deadline (+ (float-time) 5)))
            (while (and (< (float-time) deadline)
                        (not (string-match-p "received:gloom-input" (buffer-string))))
              (accept-process-output nil 0.05)))
          (should (string-match-p "received:gloom-input" (buffer-string)))
          ;; Layout operations still exclude/protect the side window.
          (should (= 4 (length (sheprd--panes))))
          (should-not (sheprd--pane-p sidebar))
          (should (window-parameter sidebar 'no-other-window))
          (should (window-parameter sidebar 'no-delete-other-windows))
          (select-window main)
          (should-error (gloom-window-up) :type 'user-error)
          (should (eq main (selected-window))))
      (when (buffer-live-p terminal)
        (with-current-buffer terminal
          (when (process-live-p ghostel--process) (delete-process ghostel--process)))
        (kill-buffer terminal))
      (when (window-live-p sidebar) (delete-window sidebar))
      (+workspace-switch origin t)
      (when (+workspace-exists-p session) (+workspace-delete session))
      (when (buffer-live-p editor) (kill-buffer editor)))))

(ert-deftest gloom-dired-and-remote-paths ()
  (with-temp-buffer
    (dired-mode temporary-file-directory)
    (should (eq (key-binding (kbd "h")) #'dired-up-directory))
    (should (eq (key-binding (kbd "l")) #'dired-find-file)))
  ;; Parse protocols/multi-hop without contacting external hosts.
  (dolist (file '("/ssh:host:/tmp/file" "/sudo:root@localhost:/etc/hosts"
                  "/ssh:jump|ssh:target:/tmp/file" "/docker:container:/tmp/file"))
    (should (tramp-tramp-file-p file)))
  (should (assoc "ssh" tramp-methods))
  (should (memq 'tramp-own-remote-path tramp-remote-path)))

(ert-deftest gloom-eglot-uses-remote-path-and-skips-missing-servers ()
  (let (started checked)
    (with-temp-buffer
      (setq major-mode 'rust-mode default-directory "/ssh:test:/repo/")
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (program remote) (setq checked (list program remote)) nil))
                ((symbol-function 'eglot-ensure) (lambda () (setq started t))))
        (gloom-eglot-ensure)
        (should (equal checked '("rust-analyzer" t)))
        (should-not started))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/server"))
                ((symbol-function 'eglot-ensure) (lambda () (setq started t))))
        (gloom-eglot-ensure)
        (should started)))))

(ert-deftest gloom-coding-commands-are-real-and-autoloaded ()
  (dolist (library '(rust-mode go-mode typescript-mode web-mode lua-mode nix-mode
                     yaml-mode json-mode zig-mode terraform-mode dockerfile-mode
                     csv-mode cmake-mode markdown-mode magit consult-eglot
                     makefile-executor vundo avy helpful))
    (require library))
  (dolist (command '(eglot-code-actions eglot-rename eglot-format-buffer
                     flymake-show-buffer-diagnostics consult-eglot-symbols
                     makefile-executor-execute-project-target vundo
                     projectile-find-file yas-insert-snippet consult-ripgrep))
    (should (commandp command))))

(ert-deftest gloom-ghostel-native-agent-output ()
  (load (expand-file-name "tests/sheprd-integration.el" gloom-doom-directory) nil t)
  (should (eq 'passed (sheprd-ghostel-integration-check))))

(ert-deftest gloom-syntax-switches-between-flycheck-and-eglot ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should flycheck-mode)
    (should eros-mode)
    (let ((eglot--managed-mode t))
      (gloom-syntax-setup)
      (should-not flycheck-mode))
    (gloom-syntax-setup)
    (should flycheck-mode)))

(ert-deftest gloom-python-checker-reports-a-real-syntax-error ()
  (let ((file (make-temp-file "gloom-checker-" nil ".py"))
        (flycheck-check-syntax-automatically nil))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (python-mode)
          (insert "def broken(:\n    pass\n")
          (setq-local flycheck-checker 'python-pycompile
                      flycheck-python-pycompile-executable (executable-find "python3"))
          (flycheck-buffer)
          (let ((deadline (+ (float-time) 10)))
            (while (and (flycheck-running-p) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should-not (flycheck-running-p))
          (should (cl-some (lambda (error)
                             (eq 'error (flycheck-error-level error)))
                           flycheck-current-errors)))
      (delete-file file))))

(ert-deftest gloom-language-tools-load-without-doom ()
  (dolist (library '(quickrun macrostep python-pytest gorepl-mode nodejs-repl
                     rustic-cargo consult-yasnippet docker go-tag go-gen-test
                     git-link git-timemachine elisp-refs))
    (require library))
  (dolist (command '(quickrun quickrun-region macrostep-expand python-pytest-dispatch
                     gorepl-run nodejs-repl run-lua rustic-cargo-test
                     rustic-cargo-check rustic-cargo-build consult-yasnippet
                     docker eglot-find-implementation eglot-find-typeDefinition))
    (should (commandp command)))
  (should-not (featurep 'doom)))

(ert-deftest gloom-node-project-tools-available-before-eglot ()
  (let* ((root (make-temp-file "gloom-node-" t))
         (bin (expand-file-name "node_modules/.bin" root))
         (server (expand-file-name "typescript-language-server" bin))
         (file (expand-file-name "example.ts" root))
         buffer started)
    (unwind-protect
        (progn
          (make-directory bin t)
          (with-temp-file (expand-file-name "package.json" root) (insert "{}"))
          (with-temp-file server (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes server #o755)
          (cl-letf (((symbol-function 'eglot-ensure)
                     (lambda ()
                       (setq started (equal (executable-find
                                             "typescript-language-server") server)))))
            (setq buffer (find-file-noselect file)))
          (should started)
          (with-current-buffer buffer
            (should (equal (car exec-path) bin))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest gloom-web-and-completion-extensions ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/gloom-example.jsx")
    (set-auto-mode)
    (should (derived-mode-p 'web-mode 'tsx-ts-mode))
    (when (derived-mode-p 'web-mode) (should emmet-mode)))
  (should (eq (lookup-key vertico-map (kbd "DEL")) #'vertico-directory-delete-char))
  (require 'embark-consult)
  (require 'wgrep)
  (should (commandp 'wgrep-change-to-wgrep-mode))
  (should (commandp 'vertico-repeat))
  (should (commandp 'consult-dir)))

(ert-deftest gloom-new-file-templates-preserve-existing-content ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name (make-temp-name "/tmp/gloom-new-"))
    (gloom-new-file-template)
    (should (string-match-p "lexical-binding: t" (buffer-string)))
    (erase-buffer)
    (insert ";;; existing source\n")
    (gloom-new-file-template)
    (should (equal (buffer-string) ";;; existing source\n"))))

(ert-deftest gloom-org-and-pdf-integrations ()
  (require 'pdf-info)
  (should (cl-find-if (lambda (entry)
                        (and (eq (cdr entry) 'pdf-view-mode)
                             (string-match-p (car entry) "/tmp/example.pdf")))
                      auto-mode-alist))
  (with-temp-buffer
    (org-mode)
    (should toc-org-mode)
    (should (featurep 'ox-gfm))
    (should (assoc "pdf" org-link-parameters))
    ;; Missing native PDF helpers must not break Org's unrelated link handlers.
    (let ((pdf-info-epdfinfo-program "/missing/epdfinfo"))
      (should-not (funcall (org-link-get-parameter "pdf" :store))))
    (insert "#+begin_src go\nfmt.Println(1)\n#+end_src\n")
    (goto-char (point-min))
    (forward-line)
    (gloom-babel-load)
    (should (fboundp 'org-babel-execute:go))))

(ert-deftest gloom-pdf-missing-helper-has-an-actionable-fallback ()
  (with-temp-buffer
    (let ((pdf-info-epdfinfo-program "/missing/epdfinfo")
          activated)
      (gloom-pdf-view (lambda (&rest _) (setq activated t)))
      (should-not activated)
      (should (eq major-mode 'fundamental-mode)))))

(ert-deftest gloom-requirements-mode-does-not-fetch-unprompted ()
  (require 'pip-requirements)
  (let (fetched)
    (cl-letf (((symbol-function 'pip-requirements-fetch-packages)
               (lambda () (setq fetched t))))
      (with-temp-buffer
        (pip-requirements-mode)
        (should-not fetched)))))

(ert-deftest gloom-evil-vim-motions-and-operators ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer "gloom-edit-test")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (emacs-lisp-mode)
            (insert "alpha beta\n  second line\nlast\n")
            (execute-kbd-macro (kbd "gg"))
            (should (= (point) (point-min)))
            ;; Real operator grammar: dw deletes a word.
            (execute-kbd-macro (kbd "dw"))
            (should (string-prefix-p "beta\n" (buffer-string)))
            (execute-kbd-macro (kbd "j 0"))
            (should (= (point) (line-beginning-position)))
            (execute-kbd-macro (kbd "^"))
            (should (= (current-column) 2))
            ;; $ stops at the last non-blank character, as in the Doom config.
            (execute-kbd-macro (kbd "$"))
            (should (= (point) (1- (line-end-position))))
            (execute-kbd-macro (kbd "G"))
            (should (= (line-number-at-pos) (line-number-at-pos (point-max))))
            (execute-kbd-macro (kbd "gg v l"))
            (should (evil-visual-state-p))
            (should (= 2 (- evil-visual-end evil-visual-beginning)))
            (execute-kbd-macro (kbd "<escape>"))
            (should-not (region-active-p))
            ;; w selects the word at point (viw).
            (execute-kbd-macro (kbd "w"))
            (should (evil-visual-state-p))
            (should (equal "beta" (buffer-substring evil-visual-beginning evil-visual-end))))
        (kill-buffer buffer)))))

(ert-deftest gloom-evil-vim-insertion-keys ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer "gloom-insert-test")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (emacs-lisp-mode)
            (insert "  alpha\nbeta\n")
            (goto-char (point-min))
            (execute-kbd-macro (kbd "I X <escape>"))
            (should (string-prefix-p "  Xalpha\n" (buffer-string)))
            (should (evil-normal-state-p))
            (execute-kbd-macro (kbd "A Y <escape>"))
            (should (string-prefix-p "  XalphaY\n" (buffer-string)))
            (execute-kbd-macro (kbd "o Z <escape>"))
            (should (string-match-p "\nZ\nbeta" (buffer-string)))
            (execute-kbd-macro (kbd "O Q <escape>"))
            (should (string-match-p "\nQ\nZ\nbeta" (buffer-string))))
        (kill-buffer buffer)))))

(ert-deftest gloom-evil-y-copies-to-the-kill-ring-and-clipboard ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer "gloom-copy-test"))
          (kill-ring nil)
          copied)
      (unwind-protect
          (let ((interprogram-cut-function (lambda (text &rest _) (setq copied text))))
            (switch-to-buffer buffer)
            (emacs-lisp-mode)
            (insert "alpha beta\n")
            (goto-char (point-min))
            (execute-kbd-macro (kbd "w y"))
            (should (equal copied "alpha"))
            (should (equal (car kill-ring) "alpha"))
            (should (equal (buffer-string) "alpha beta\n"))
            (should (evil-normal-state-p))
            (execute-kbd-macro (kbd "yy"))
            (should (equal copied "alpha beta\n")))
        (kill-buffer buffer)))))

(ert-deftest gloom-evil-maps-have-only-real-commands ()
  (cl-labels ((check-map (map)
                (map-keymap (lambda (key binding)
                              (cond ((eq binding mode-specific-map)) ; the leader
                                    ((eq key 'intercept-state)) ; Evil's marker
                                    ((keymapp binding) (check-map binding))
                                    ((and (symbolp binding) binding)
                                     (should (commandp binding)))))
                            map)))
    (check-map evil-normal-state-map)
    (check-map gloom-override-mode-map)))

(ert-deftest gloom-evil-delete-keeps-window-keys ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer "gloom-delete-test")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (emacs-lisp-mode)
            (insert "alpha beta\n")
            (goto-char 7)
            (execute-kbd-macro (kbd "x"))
            (should (equal (buffer-string) "alpha eta\n"))
            (should (eq (key-binding (kbd "C-k")) #'gloom-window-up)))
        (kill-buffer buffer)))))

(ert-deftest gloom-evil-undo-and-redo ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer "gloom-undo-test")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (emacs-lisp-mode)
            (buffer-enable-undo)
            (insert "alpha")
            (setq buffer-undo-list nil)
            (execute-kbd-macro (kbd "A ! <escape>"))
            (undo-boundary)
            (execute-kbd-macro (kbd "u"))
            (should (equal (buffer-string) "alpha"))
            (execute-kbd-macro (kbd "C-r"))
            (should (equal (buffer-string) "alpha!")))
        (kill-buffer buffer)))))


;;; Sheprd agents end to end: real persp sessions, Ghostel terminals and
;;; Pilish, against offline fake CLIs (configs/doom/tests/fake-*.py).

;; Let-bound below before Pilish loads; they must be dynamic.
(defvar pilish-executable)
(defvar treesit-auto-install-grammar)

(defun gloom-test-wait (predicate &optional seconds)
  "Process events until PREDICATE holds or SECONDS (default 10) pass."
  (let ((end (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defmacro gloom-test-with-agents (&rest body)
  "Run BODY in a fresh session and registry, with every client faked.
Binds `repo', `session' and `log'; `agent-log' returns the logged entries."
  (declare (indent 0))
  `(let* ((root (make-temp-file "gloom-agents-" t))
          (repo (file-name-as-directory (expand-file-name "repo" root)))
          (log (expand-file-name "agents.jsonl" root))
          (fake (lambda (name) (expand-file-name (format "tests/fake-%s.py" name)
                                                 gloom-doom-directory)))
          (python (executable-find "python3"))
          (sheprd-directory (file-name-as-directory (expand-file-name "sheprd" root)))
          (sheprd--db nil) (sheprd--db-file nil)
          (sheprd-agent-commands `((claude . (,python ,(funcall fake "claude")))
                                   (codex . (,python ,(funcall fake "codex")))
                                   (pi . (,python ,(funcall fake "pi")))))
          (pilish-executable (list python (funcall fake "pi")))
          (process-environment (append (list (concat "FAKE_AGENT_LOG=" log)
                                             (concat "FAKE_PI_LOG=" log))
                                       process-environment))
          ;; Pilish's chat mode otherwise asks to install a Markdown grammar.
          (treesit-auto-install-grammar 'never)
          (origin (+workspace-current-name))
          (session (format "agents-%s" (random 100000))))
     (cl-flet ((agent-log ()
                 (with-temp-buffer
                   (when (file-exists-p log) (insert-file-contents log))
                   (mapcar (lambda (line) (json-parse-string line :object-type 'alist
                                                             :array-type 'list))
                           (split-string (buffer-string) "\n" t)))))
       (make-directory repo t)
       (let ((default-directory repo))
         (call-process "git" nil nil nil "init" "-q"))
       (unwind-protect
           (save-window-excursion
             (+workspace-switch session t)
             ,@body)
         (dolist (id (hash-table-keys sheprd--runs))
           (ignore-errors (sheprd-stop-agent id)))
         (dolist (buffer (buffer-list))
           (when (buffer-local-value 'sheprd--agent-id buffer)
             (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))
         (+workspace-switch origin t)
         (when (+workspace-exists-p session) (+workspace-delete session))
         (when sheprd--db (sqlite-close sheprd--db))
         (delete-directory root t)))))

(defun gloom-test-state (id)
  (sheprd--agent-state (sheprd-agent id)))

(defmacro gloom-test-recording-activities (var &rest body)
  "Run BODY collecting, in VAR, every activity Sheprd stores, oldest first.
Polling can apply several client reports at once, so sampling the record
would miss short-lived states."
  (declare (indent 1))
  `(let* ((,var nil)
          (record (lambda (_id &rest changes)
                    (when (plist-member changes :activity)
                      (setq ,var (append ,var (list (plist-get changes :activity))))))))
     (advice-add 'sheprd--agent-set :before record)
     (unwind-protect (progn ,@body)
       (advice-remove 'sheprd--agent-set record))))

(defun gloom-test-wait-for-runs (count log-function)
  "Wait until LOG-FUNCTION reports COUNT client starts; return their argvs."
  (gloom-test-wait (lambda () (>= (length (funcall log-function)) count)))
  (mapcar (lambda (entry) (alist-get 'argv entry)) (funcall log-function)))

(ert-deftest gloom-sheprd-claude-agent-reports-hooks-and-resumes ()
  (gloom-test-with-agents
    (setenv "FAKE_CLAUDE_EVENTS" "UserPromptSubmit Notification:permission PostToolUse Stop")
    (unwind-protect
        (gloom-test-recording-activities seen
          (let* ((id (sheprd-new-agent "planner" 'claude repo))
                 (agent (sheprd-agent id))
                 (conversation (sheprd-agent-conversation agent))
                 (buffer (sheprd--run-buffer id)))
          ;; Sealed into this session only.
          (should (equal session (buffer-local-value 'sheprd--session buffer)))
          (should (memq buffer (+workspace-buffer-list)))
          (should (gloom-test-wait (lambda () (eq 'input (car (last seen))))))
          ;; Starting clears the activity; then the hooks in order: start,
          ;; prompt, permission, tool done, stop.
          (should (equal '(nil input working approval working input) seen))
          (let ((first (car (agent-log))))
            (should (equal "claude" (alist-get 'program first)))
            (should (member "--settings" (alist-get 'argv first)))
            (should (equal (last (alist-get 'argv first) 2) (list "--session-id" conversation))))
          (should (sheprd-agent-launched (sheprd-agent id)))
          ;; Stop keeps the record and the buffer.
          (sheprd-stop-agent id)
          (should (eq 'stopped (gloom-test-state id)))
          (should (buffer-live-p buffer))
          ;; Opening it again resumes the same conversation.
          (setenv "FAKE_CLAUDE_EVENTS" nil)
          (sheprd-open-agent id)
          (should (gloom-test-wait (lambda () (eq (gloom-test-state id) 'input))))
          (should (equal (last (car (last (gloom-test-wait-for-runs 2 #'agent-log))) 2)
                         (list "--resume" conversation)))
          (should (equal conversation (sheprd-agent-conversation (sheprd-agent id))))))
      (setenv "FAKE_CLAUDE_EVENTS" nil))))

(ert-deftest gloom-sheprd-agent-that-dies-at-start-fails ()
  (gloom-test-with-agents
    (setenv "FAKE_CLAUDE_CRASH" "1")
    (unwind-protect
        (let ((id (sheprd-new-agent "crasher" 'claude repo)))
          (should (gloom-test-wait (lambda () (eq (gloom-test-state id) 'failed))))
          (should (string-match-p "exited before starting"
                                  (sheprd-agent-error (sheprd-agent id))))
          ;; Its terminal stays, so the crash output can be read.  Ghostel
          ;; renders a hidden terminal lazily: read the terminal itself.
          (should (cl-some (lambda (buffer)
                             (string-match-p "fake claude crashed"
                                             (or (sheprd--full-terminal-text buffer) "")))
                           (sheprd--agent-buffers id)))
          (should-not (sheprd-agent-launched (sheprd-agent id))))
      (setenv "FAKE_CLAUDE_CRASH" nil))))

(ert-deftest gloom-sheprd-codex-agent-captures-its-thread ()
  (gloom-test-with-agents
    (setenv "FAKE_CODEX_EVENTS" "approval turn:thread-42")
    (unwind-protect
        (let ((id (sheprd-new-agent "coder" 'codex repo)))
          (should (gloom-test-wait
                   (lambda () (equal "thread-42" (sheprd-agent-conversation (sheprd-agent id))))))
          (sheprd-stop-agent id)
          (setenv "FAKE_CODEX_EVENTS" nil)
          (sheprd-open-agent id)
          (should (gloom-test-wait (lambda () (eq (gloom-test-state id) 'input))))
          (should (equal '("resume" "thread-42")
                         (last (car (last (gloom-test-wait-for-runs 2 #'agent-log))) 2))))
      (setenv "FAKE_CODEX_EVENTS" nil))))

(ert-deftest gloom-sheprd-pi-terminal-agent-reads-its-screen ()
  (gloom-test-with-agents
    (let* ((id (sheprd-new-agent "notes" 'pi repo 'ghostel))
           (conversation (sheprd-agent-conversation (sheprd-agent id))))
      (should (gloom-test-wait (lambda () (eq (gloom-test-state id) 'input))))
      (should (equal (list "--session-id" conversation)
                     (car (gloom-test-wait-for-runs 1 #'agent-log)))))))

(ert-deftest gloom-sheprd-pilish-agent-lifecycle ()
  (gloom-test-with-agents
    (let* ((id (sheprd-new-agent "pilot" 'pi repo 'pilish))
           (chat (sheprd--run-buffer id))
           (input (buffer-local-value 'pilish--input-buffer chat))
           (conversation (sheprd-agent-conversation (sheprd-agent id))))
      ;; Both Pilish buffers are sealed into the session; the input is shown.
      (dolist (buffer (list chat input))
        (should (equal session (buffer-local-value 'sheprd--session buffer))))
      (should (get-buffer-window chat))
      (should (eq (window-buffer) input))
      (should (gloom-test-wait (lambda () (eq (gloom-test-state id) 'input))))
      (let ((argv (car (gloom-test-wait-for-runs 1 #'agent-log))))
        (should (equal '("--mode" "rpc") (seq-take argv 2)))
        (should (equal conversation (cadr (member "--session-id" argv))))
        ;; Pi's own saved project trust decisions apply, as in Doom.
        (should-not (member "--approve" argv)))
      ;; A turn shows working, then input.
      (let (seen)
        (with-current-buffer input (erase-buffer) (insert "hello") (pilish-send))
        (should (gloom-test-wait (lambda ()
                                   (cl-pushnew (gloom-test-state id) seen)
                                   (and (memq 'working seen)
                                        (eq (gloom-test-state id) 'input))))))
      ;; A Pi dialog shows approval exactly while it is open.
      (let (during)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (setq during (gloom-test-state id)) t)))
          (with-current-buffer input (erase-buffer) (insert "please ask") (pilish-send))
          (should (gloom-test-wait (lambda () during)))
          (should (gloom-test-wait (lambda () (eq (gloom-test-state id) 'input)))))
        (should (eq 'approval during)))
      ;; Stop, then resume the same Pi session with its history.
      (sheprd-stop-agent id)
      (should (eq 'stopped (gloom-test-state id)))
      (setenv "FAKE_PI_SAVED" conversation)
      (unwind-protect
          (progn
            (sheprd-open-agent id)
            (should (gloom-test-wait (lambda () (eq (gloom-test-state id) 'input))))
            (should (gloom-test-wait
                     (lambda ()
                       (seq-find (lambda (entry)
                                   (equal "get_messages"
                                          (alist-get 'type (alist-get 'command entry))))
                                 (agent-log))))))
        (setenv "FAKE_PI_SAVED" nil)))))

(ert-deftest gloom-sheprd-session-rename-and-kill-keep-agent-records ()
  (gloom-test-with-agents
    (let ((id (sheprd-new-agent "keeper" 'pi repo 'ghostel))
          (renamed (concat session "-renamed")))
      (should (gloom-test-wait (lambda () (eq (gloom-test-state id) 'input))))
      (sheprd-rename-session renamed)
      (setq session renamed)
      (should (equal renamed (sheprd-agent-session (sheprd-agent id))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (sheprd-kill-session renamed))
      (should-not (sheprd--agent-buffers id))
      (should (eq 'stopped (gloom-test-state id)))
      (should (equal renamed (sheprd-agent-session (sheprd-agent id))))
      ;; The sidebar lists it under a closed session.
      (with-temp-buffer
        (sheprd--render-sidebar)
        (should (string-match-p (regexp-quote (format "%s (1)" renamed)) (buffer-string)))))))
