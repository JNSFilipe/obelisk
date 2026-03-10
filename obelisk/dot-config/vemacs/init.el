;;; init.el --- Oblsk: fast, vanilla-first IDE -*- lexical-binding: t; -*-

;;;; -------------------------------------------------------------------
;;;; 0. Minimal setup (Evil + IDO)
;;;; -------------------------------------------------------------------

(defgroup oblsk nil
  "Oblsk: fast, vanilla-first IDE setup."
  :group 'convenience)

;;;; -------------------------------------------------------------------
;;;; 1. Constants / tiny helpers
;;;; -------------------------------------------------------------------

(defconst oblsk/is-macos (eq system-type 'darwin))
(defconst oblsk/is-gui   (display-graphic-p))

(defun oblsk/shell-basename ()
  "Return the basename of the configured shell, if any."
  (file-name-nondirectory
   (or (and (boundp 'explicit-shell-file-name) explicit-shell-file-name)
       (and (boundp 'shell-file-name) shell-file-name)
       "")))

(eval-when-compile
  ;; Silence byte-compiler warnings without affecting runtime bindings.
  (defvar display-line-numbers-type)
  (defvar global-auto-revert-non-file-buffers)
  (defvar auto-revert-avoid-polling)
  (defvar recentf-max-saved-items)
  (defvar recentf-auto-cleanup)
  (defvar doom-themes-enable-bold)
  (defvar doom-themes-enable-italic)
  (defvar savehist-additional-variables)
  (defvar which-key-idle-delay)
  (defvar which-key-idle-secondary-delay)
  (defvar which-key-max-description-length)
  (defvar which-key-allow-imprecise-window-fit)
  (defvar which-key-mode-map)
  (defvar eglot-autoshutdown)
  (defvar eglot-events-buffer-size)
  (defvar dape-configs)
  (defvar eat-kill-buffer-on-exit)
  (defvar evil-want-keybinding)
  (defvar evil-disable-insert-state-bindings)
  (defvar evil-normal-state-cursor)
  (defvar evil-visual-state-cursor)
  (defvar evil-insert-state-cursor)
  (defvar evil-replace-state-cursor)
  (defvar evil-emacs-state-cursor)
  (defvar oblsk-leader-map)
  (defvar oblsk-leader-project-map)
  (defvar oblsk-leader-lsp-map)
  (defvar oblsk-leader-debug-map)
  (defvar oblsk-leader-ai-map)
  (defvar oblsk-leader-flymake-map))

(declare-function which-key-abort "which-key")
(declare-function evil-terminal-cursor-changer-activate "evil-terminal-cursor-changer")
(declare-function undo-tree-visualize "undo-tree")
(declare-function magit-section-forward "magit")
(declare-function magit-section-backward "magit")
(declare-function avy-goto-char "avy")
(declare-function avy-goto-word-occurrence "avy")
(declare-function avy-goto-line "avy")
(declare-function eat "eat")
(declare-function eat-project "eat")
(declare-function eat-other-window "eat")
(declare-function eat-project-other-window "eat")
(declare-function eat-make "eat")
(declare-function dape "dape")
(declare-function dape-restart "dape")
(declare-function dape-kill "dape")
(declare-function dape-pause "dape")
(declare-function dape-next "dape")
(declare-function dape-step-in "dape")
(declare-function dape-step-out "dape")
(declare-function dape-breakpoint-toggle "dape")
(declare-function dape-breakpoint-expression "dape")
(declare-function dape-breakpoint-log "dape")
(declare-function dape-repl "dape")
(declare-function dape-info "dape")
(declare-function dape-evaluate-expression "dape")
(declare-function flymake-goto-next-error "flymake")
(declare-function flymake-goto-prev-error "flymake")
(declare-function flymake-show-buffer-diagnostics "flymake")
(declare-function evil-collection-init "evil-collection")
(declare-function evil-mode "evil")

(defun oblsk/benchmark--startup ()
  "Message a small startup timing summary."
  (let ((gc-delta (- gcs-done (or (bound-and-true-p oblsk--gc-before-init) 0))))
    (message "Emacs ready in %.2fs (%d GCs)"
             (float-time (time-subtract after-init-time before-init-time))
             gc-delta)))
(add-hook 'emacs-startup-hook #'oblsk/benchmark--startup)

(defun oblsk/with-suppressed-message (fn &rest args)
  "Call FN with ARGS suppressing *Messages* spam."
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply fn args)))

;;;; -------------------------------------------------------------------
;;;; 2. Sensible defaults (fast + terminal-friendly)
;;;; -------------------------------------------------------------------

(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      use-dialog-box nil
      visible-bell nil
      confirm-kill-processes t
      use-short-answers t
      load-prefer-newer t)

;; Keep Custom settings out of init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Clipboard integration (yank/kill ↔ system clipboard)
(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t)
(when (boundp 'x-select-enable-clipboard)
  (setq x-select-enable-clipboard t))
(when (boundp 'x-select-enable-primary)
  (setq x-select-enable-primary nil))

(setq read-process-output-max (* 2 1024 1024))

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "autosaves/" user-emacs-directory) t))
      create-lockfiles nil)

(make-directory (expand-file-name "backups/" user-emacs-directory) t)
(make-directory (expand-file-name "autosaves/" user-emacs-directory) t)

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-step 1
      fast-but-imprecise-scrolling t)

(when (and oblsk/is-gui (fboundp 'pixel-scroll-precision-mode))
  (pixel-scroll-precision-mode 1))

(blink-cursor-mode -1)

(unless oblsk/is-gui (xterm-mouse-mode 1))

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(column-number-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-avoid-polling t)
(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)
(transient-mark-mode 1)
(global-set-key (kbd "C-SPC") #'set-mark-command)
(global-set-key (kbd "C-@")   #'set-mark-command)
(global-set-key (kbd "C-<")   #'set-mark-command)
(dolist (map '(minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-completion-map
               minibuffer-local-must-match-map
               minibuffer-local-isearch-map))
  (when (boundp map)
    (define-key (symbol-value map) (kbd "<escape>") #'keyboard-quit)
    (define-key (symbol-value map) (kbd "C-[") #'keyboard-quit)))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "<escape>") #'isearch-cancel))

(defun oblsk/select-line-and-avy ()
  "Select current line, then jump to a line with Avy keeping region active."
  (interactive)
  (let ((start (line-beginning-position)))
    ;; Select the current line first.
    (push-mark start t t)
    (goto-char (line-end-position))
    (activate-mark)
    (setq deactivate-mark nil)
    ;; Jump to a line, then extend region to full line(s).
    (call-interactively #'oblsk/avy-goto-line)
    (goto-char (line-end-position))
    (set-mark start)
    (activate-mark)
    (setq deactivate-mark nil)))

(defun oblsk/kill-region-or-backward-word ()
  "Kill region if active; otherwise jump with Avy."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (call-interactively #'oblsk/avy-goto-word-occurrence)))

(global-set-key (kbd "C-l") #'oblsk/select-line-and-avy)
(global-set-key (kbd "C-w") #'oblsk/kill-region-or-backward-word)

(savehist-mode 1)
(setq history-length 2000)
(recentf-mode 1)
(setq recentf-max-saved-items 2000
      recentf-auto-cleanup 'never)

(when (fboundp 'global-so-long-mode)
  (global-so-long-mode 1))

;;;; -------------------------------------------------------------------
;;;; 3. package.el (minimal installs)
;;;; -------------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("gnu" . 3)
        ("nongnu" . 2)
        ("melpa" . 1)))
(unless (bound-and-true-p package--initialized)
  (oblsk/with-suppressed-message #'package-initialize))
(eval-when-compile
  (require 'package))

(defun oblsk/ensure-package (pkg)
  "Ensure PKG is installed. Refresh archives only if needed."
  (unless (package-installed-p pkg)
    (when (or (null package-archive-contents)
              (not (assoc pkg package-archive-contents)))
      (oblsk/with-suppressed-message #'package-refresh-contents))
    (oblsk/with-suppressed-message #'package-install pkg)))

;;;; -------------------------------------------------------------------
;;;; 4. macOS niceties (PATH + clipboard)
;;;; -------------------------------------------------------------------

(defun oblsk/macos--import-path ()
  "Import PATH from the user's login shell (GUI Emacs on macOS)."
  (when (and oblsk/is-macos oblsk/is-gui (executable-find "sh"))
    (let* ((shell (or (getenv "SHELL") "/bin/zsh"))
           (cmd (format "%s -lc 'printf %%s \"$PATH\"'" shell))
           (path (string-trim (shell-command-to-string cmd))))
      (when (and path (not (string-empty-p path)))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory)))))))
(oblsk/macos--import-path)

(when (and oblsk/is-macos (not oblsk/is-gui)
           (executable-find "pbcopy") (executable-find "pbpaste"))
  (setq interprogram-cut-function
        (lambda (text)
          (let ((process-connection-type nil))
            (let ((proc (start-process "pbcopy" nil "pbcopy")))
              (process-send-string proc text)
              (process-send-eof proc)))))
  (setq interprogram-paste-function
        (lambda () (shell-command-to-string "pbpaste"))))

;;;; -------------------------------------------------------------------
;;;; 5. Theme: doom-themes (doom-lena, Xresources-aware)
;;;; -------------------------------------------------------------------

(oblsk/ensure-package 'doom-themes)
(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic oblsk/is-gui)

(add-to-list 'custom-theme-load-path user-emacs-directory)
(load-theme 'doom-lena t)
(when (require 'doom-themes-ext-visual-bell nil t)
  (doom-themes-visual-bell-config))
(with-eval-after-load 'org
  (when (require 'doom-themes-ext-org nil t)
    (doom-themes-org-config)))

;;;; -------------------------------------------------------------------
;;;; 6. “Bash-like” TAB completion (no dropdown UI)
;;;; -------------------------------------------------------------------

(setq tab-always-indent 'complete
      completion-cycle-threshold 3
      completion-styles '(basic partial-completion flex)
      completions-detailed t)

(defun oblsk/indent-or-complete ()
  "Indent, or complete at point (Bash-like)."
  (interactive)
  (cond
   ((use-region-p) (indent-region (region-beginning) (region-end)))
   (t (indent-for-tab-command)))
  (when (and (not (minibufferp)) (looking-back "[[:word:]_./-]" 1))
    (completion-at-point)))

(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "TAB") #'oblsk/indent-or-complete)))
(add-hook 'text-mode-hook
          (lambda () (local-set-key (kbd "TAB") #'oblsk/indent-or-complete)))

;;;; -------------------------------------------------------------------
;;;; 7. Completion system (IDO)
;;;; -------------------------------------------------------------------

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)

(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-max-prospects 12
      ido-save-directory-list-file (expand-file-name "ido.last" user-emacs-directory))

;; Make all completing-read use ido (including project-find-file)
(oblsk/ensure-package 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq ido-cr+-auto-update-disable-list t)

;; Ido navigation: C-h/C-l and C-b/C-f for prev/next match, ESC to quit
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-h") #'ido-prev-match)
            (define-key ido-completion-map (kbd "C-l") #'ido-next-match)
            (define-key ido-completion-map (kbd "C-b") #'ido-prev-match)
            (define-key ido-completion-map (kbd "C-f") #'ido-next-match)
            (define-key ido-completion-map [escape] #'keyboard-quit)
            (define-key ido-completion-map "\e" #'keyboard-quit)))

(defun oblsk/recentf--index ()
  "Return a hash table mapping recent files to recency rank."
  (let ((tbl (make-hash-table :test 'equal))
        (i 0))
    (dolist (f recentf-list)
      (puthash (expand-file-name f) i tbl)
      (setq i (1+ i)))
    tbl))

(defun oblsk/ido-sort-by-recentf ()
  "Prefer recent files first in Ido file lists."
  (when (and (eq ido-cur-item 'file)
             (boundp 'ido-temp-list)
             (listp ido-temp-list)
             (boundp 'recentf-list)
             recentf-list)
    (let ((rank (oblsk/recentf--index))
          (root ido-current-directory))
      (setq ido-temp-list
            (sort ido-temp-list
                  (lambda (a b)
                    (let ((ra (gethash (expand-file-name a root) rank most-positive-fixnum))
                          (rb (gethash (expand-file-name b root) rank most-positive-fixnum)))
                      (if (/= ra rb)
                          (< ra rb)
                        (string-lessp a b)))))))))

(add-hook 'ido-make-file-list-hook #'oblsk/ido-sort-by-recentf)

;; Use IDO for common entry points
(global-set-key (kbd "C-x C-f") #'ido-find-file)
(global-set-key (kbd "C-x b")   #'ido-switch-buffer)

;;;; M-x with recency/frequency tracking
(defvar oblsk/mx-counts (make-hash-table :test 'equal)
  "Hash table tracking command execution counts.")
(defvar oblsk/mx-last (make-hash-table :test 'equal)
  "Hash table tracking last execution time of commands.")

(add-to-list 'savehist-additional-variables 'oblsk/mx-counts)
(add-to-list 'savehist-additional-variables 'oblsk/mx-last)
(add-to-list 'savehist-additional-variables 'extended-command-history)

(defun oblsk/mx--score (cmd)
  "Return score for CMD as (last-time . count)."
  (cons (or (gethash cmd oblsk/mx-last) 0.0)
        (or (gethash cmd oblsk/mx-counts) 0)))

(defun oblsk/mx--sorted-commands ()
  "Return all commands sorted by recency and frequency."
  (let ((cmds (all-completions "" obarray #'commandp)))
    (sort cmds
          (lambda (a b)
            (let* ((sa (oblsk/mx--score a))
                   (sb (oblsk/mx--score b)))
              (or (> (car sa) (car sb))
                  (and (= (car sa) (car sb)) (> (cdr sa) (cdr sb)))
                  (and (= (car sa) (car sb))
                       (= (cdr sa) (cdr sb))
                       (string-lessp a b))))))))

(defun oblsk/mx--record (cmd)
  "Record execution of CMD for frequency/recency tracking."
  (puthash cmd (float-time) oblsk/mx-last)
  (puthash cmd (1+ (gethash cmd oblsk/mx-counts 0)) oblsk/mx-counts))

(defun oblsk/mx ()
  "Enhanced M-x with recency/frequency ordering."
  (interactive)
  (let* ((choices (oblsk/mx--sorted-commands))
         (picked  (ido-completing-read "M-x " choices nil t nil 'extended-command-history))
         (sym     (intern picked)))
    (oblsk/mx--record picked)
    (call-interactively sym)))

(global-set-key (kbd "M-x") #'oblsk/mx)

;;;; -------------------------------------------------------------------
;;;; 8. Projects + lightweight per-project PATH isolation
;;;; -------------------------------------------------------------------

(require 'project)

;; Scan these directories for projects
(defvar oblsk/project-directories
  '("~/Documents/GitHub" "~/Documents/Probe")
  "Directories to scan for projects.")

(defun oblsk/project-remember-directories ()
  "Remember all projects under `oblsk/project-directories'."
  (dolist (dir oblsk/project-directories)
    (let ((expanded (expand-file-name dir)))
      (when (file-directory-p expanded)
        (project-remember-projects-under expanded)))))

;; Scan project directories on startup (deferred to avoid slowing init)
(run-with-idle-timer 1 nil #'oblsk/project-remember-directories)

(defun oblsk/project-root ()
  "Return current project root, or default-directory."
  (if-let* ((p (project-current nil)))
      (expand-file-name (project-root p))
    (expand-file-name default-directory)))

(defun oblsk/project--existing-dirs (root rels)
  "Return absolute dirs under ROOT for RELS that exist."
  (let (out)
    (dolist (r rels (nreverse out))
      (let ((p (expand-file-name r root)))
        (when (file-directory-p p) (push p out))))))

(defun oblsk/project-apply-exec-path ()
  "Add common project-local bin dirs to exec-path (buffer-locally)."
  (let* ((root (oblsk/project-root))
         (bins (oblsk/project--existing-dirs
                root '(".venv/bin" "venv/bin" "node_modules/.bin" "bin"))))
    (when bins
      (setq-local exec-path (append bins exec-path))
      (setq-local process-environment (copy-sequence process-environment))
      (let ((cur (or (getenv "PATH") "")))
        (setenv "PATH" (concat (mapconcat #'identity bins path-separator)
                               path-separator cur))))))

(add-hook 'find-file-hook #'oblsk/project-apply-exec-path)

(defun oblsk/find-file ()
  "Find file: use `project-find-file' in a project, else `ido-find-file'."
  (interactive)
  (if (project-current nil)
      (call-interactively #'project-find-file)
    (call-interactively #'ido-find-file)))

(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p b") #'project-switch-to-buffer)
(global-set-key (kbd "C-c p d") #'project-dired)
(global-set-key (kbd "C-c p s") #'project-shell)
(global-set-key (kbd "C-c p c") #'project-compile)

;;;; -------------------------------------------------------------------
;;;; 9. Which-key (discoverable keybindings)
;;;; -------------------------------------------------------------------

(run-with-idle-timer
 0.6 nil
 (lambda ()
   (oblsk/ensure-package 'which-key)
   (require 'which-key)
   (which-key-mode 1)))

(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.05
        which-key-max-description-length 32
        which-key-allow-imprecise-window-fit t)
  (define-key which-key-mode-map (kbd "<escape>") #'which-key-abort)

  ;; Leader key prefix labels
  (which-key-add-key-based-replacements
    "SPC p" "project"
    "SPC l" "lsp"
    "SPC d" "debug"
    "SPC a" "ai"
    "SPC e" "errors"
    ;; Project bindings
    "SPC p p" "switch project"
    "SPC p f" "find file"
    "SPC p b" "switch buffer"
    "SPC p d" "dired"
    "SPC p s" "shell"
    "SPC p c" "compile"
    ;; Top level bindings
    "SPC SPC" "M-x"
    "SPC f" "find file"
    "SPC b" "switch buffer"
    "SPC w" "save"
    "SPC s" "split right"
    "SPC S" "split below"
    "SPC x" "close other windows"
    "SPC o" "dired"
    "SPC G" "magit status"
    "SPC h" "diff file"
    "SPC u" "undo tree"
    "SPC m" "project compile"
    "SPC M" "compile"
    "SPC g" "grep"
    "SPC t" "terminal"
    "SPC T" "terminal other window"
    "SPC H" "man"
    ;; LSP bindings
    "SPC l i" "install hints"
    "SPC l r" "rename"
    "SPC l a" "code actions"
    "SPC l f" "format"
    ;; Debug bindings
    "SPC d d" "start debug"
    "SPC d l" "restart"
    "SPC d t" "kill"
    "SPC d p" "pause"
    "SPC d o" "next"
    "SPC d i" "step in"
    "SPC d O" "step out"
    "SPC d b" "toggle breakpoint"
    "SPC d B" "conditional breakpoint"
    "SPC d L" "log breakpoint"
    "SPC d r" "repl"
    "SPC d h" "info"
    "SPC d e" "eval expression"
    ;; AI bindings
    "SPC a c" "claude"
    "SPC a g" "gemini"
    "SPC a x" "codex"
    "SPC a a" "copilot"
    ;; Error bindings
    "SPC e n" "next error"
    "SPC e p" "prev error"
    "SPC e l" "list errors"))

;;;; -------------------------------------------------------------------
;;;; 10. Languages: Eglot + Flymake (built-in) + minimal external modes
;;;; -------------------------------------------------------------------

(require 'cl-lib)
(defvar eglot-server-programs nil)
(autoload 'eglot-ensure "eglot" nil t)
(autoload 'eglot-rename "eglot" nil t)
(autoload 'eglot-code-actions "eglot" nil t)
(autoload 'eglot-format "eglot" nil t)
(autoload 'eglot-inlay-hints-mode "eglot" nil t)

(defvar oblsk/eglot--failed (make-hash-table :test 'eq)
  "Track major modes where Eglot failed to start.")
(defvar oblsk/eglot-error-buffer "*oblsk/eglot-errors*"
  "Buffer name for logging Eglot startup errors.")

(defun oblsk/eglot--log-error (mode err)
  "Log Eglot startup ERR for MODE."
  (let ((buf (get-buffer-create oblsk/eglot-error-buffer)))
    (with-current-buffer buf
      (read-only-mode -1)
      (goto-char (point-max))
      (insert (format "[%s] %s: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      mode
                      (error-message-string err)))
      (read-only-mode 1))))

(defun oblsk/python-lsp-command ()
  "Return the Ty server command for Eglot."
  '("ty" "server"))

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (setq major-mode-remap-alist
        (append
         '((python-mode . python-ts-mode)
           (js-mode     . js-ts-mode)
           (typescript-mode . typescript-ts-mode)
           (c-mode      . c-ts-mode)
           (c++-mode    . c++-ts-mode)
           (css-mode    . css-ts-mode)
           (json-mode   . json-ts-mode))
         major-mode-remap-alist)))

;; External modes are autoloaded to keep startup fast.
(unless (locate-library "markdown-mode")
  (oblsk/ensure-package 'markdown-mode))
(autoload 'markdown-mode "markdown-mode" nil t)

(unless (locate-library "zig-mode")
  (oblsk/ensure-package 'zig-mode))
(autoload 'zig-mode "zig-mode" nil t)

(unless (locate-library "tuareg")
  (oblsk/ensure-package 'tuareg))
(autoload 'tuareg-mode "tuareg" nil t)

(unless (fboundp 'rust-ts-mode)
  (unless (locate-library "rust-mode")
    (oblsk/ensure-package 'rust-mode))
  (autoload 'rust-mode "rust-mode" nil t))

(setq eglot-server-programs
      `(
        ((python-ts-mode python-mode) . (lambda (&rest _) (oblsk/python-lsp-command)))
        ((c-ts-mode c++-ts-mode c-mode c++-mode) . ("clangd"))
        ((rust-ts-mode rust-mode) . ("rust-analyzer"))
        ((sh-mode bash-ts-mode) . ("bash-language-server" "start"))
        (lua-mode . ("lua-language-server"))
        ((js-ts-mode typescript-ts-mode tsx-ts-mode js-mode typescript-mode)
         . ("typescript-language-server" "--stdio"))
        (html-mode . ("vscode-html-language-server" "--stdio"))
        ((css-ts-mode css-mode) . ("vscode-css-language-server" "--stdio"))
        ((json-ts-mode json-mode) . ("vscode-json-language-server" "--stdio"))
        (zig-mode . ("zls"))
        ((latex-mode tex-mode) . ("texlab"))
        (markdown-mode . ("marksman"))
        (tuareg-mode . ("ocamllsp"))
        ))

(defun oblsk/eglot-maybe ()
  "Start Eglot for supported buffers."
  (when (derived-mode-p 'prog-mode 'text-mode)
    (condition-case err
        (progn
          (eglot-ensure)
          (remhash major-mode oblsk/eglot--failed))
      (error
       (unless (gethash major-mode oblsk/eglot--failed)
         (puthash major-mode t oblsk/eglot--failed)
         (oblsk/eglot--log-error major-mode err)
         (message "Eglot disabled for %s: %s (see %s)"
                  major-mode (error-message-string err)
                  oblsk/eglot-error-buffer))))))

(dolist (hook '(python-mode-hook python-ts-mode-hook
                c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook
                rust-mode-hook
                sh-mode-hook
                lua-mode-hook
                js-mode-hook js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook
                html-mode-hook css-mode-hook css-ts-mode-hook json-mode-hook json-ts-mode-hook
                zig-mode-hook
                latex-mode-hook tex-mode-hook
                markdown-mode-hook
                tuareg-mode-hook))
  (add-hook hook #'oblsk/eglot-maybe))

(global-set-key (kbd "C-c ! n") #'flymake-goto-next-error)
(global-set-key (kbd "C-c ! p") #'flymake-goto-prev-error)
(global-set-key (kbd "C-c ! l") #'flymake-show-buffer-diagnostics)

(global-set-key (kbd "C-c l r") #'eglot-rename)
(global-set-key (kbd "C-c l a") #'eglot-code-actions)
(global-set-key (kbd "C-c l f") #'eglot-format)

(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (fboundp 'eglot-inlay-hints-mode)
                (eglot-inlay-hints-mode 1)))))

;;;; -------------------------------------------------------------------
;;;; 11. LSP install hints (missing server helper)
;;;; -------------------------------------------------------------------

(defconst oblsk/lsp-server-install-hints
  '(("ty"
     :why "Python type checker/LSP"
     :mac ("brew install astral-sh/ty/ty   # or follow Ty install docs")
     :generic ("See Ty docs; ensure `ty` is on PATH"))
    ("clangd"
     :why "C/C++ LSP"
     :mac ("brew install llvm  # provides clangd (ensure PATH)")
     :generic ("Install clangd (often via LLVM packages)"))
    ("rust-analyzer"
     :why "Rust LSP"
     :mac ("brew install rust-analyzer")
     :generic ("rustup component add rust-analyzer || install from distro"))
    ("bash-language-server"
     :why "Shell LSP"
     :mac ("npm i -g bash-language-server")
     :generic ("npm i -g bash-language-server"))
    ("lua-language-server"
     :why "Lua LSP"
     :mac ("brew install lua-language-server")
     :generic ("Install lua-language-server from your OS or upstream releases"))
    ("typescript-language-server"
     :why "JS/TS LSP"
     :mac ("npm i -g typescript typescript-language-server")
     :generic ("npm i -g typescript typescript-language-server"))
    ("vscode-html-language-server"
     :why "HTML LSP"
     :mac ("npm i -g vscode-langservers-extracted")
     :generic ("npm i -g vscode-langservers-extracted"))
    ("vscode-css-language-server"
     :why "CSS LSP"
     :mac ("npm i -g vscode-langservers-extracted")
     :generic ("npm i -g vscode-langservers-extracted"))
    ("vscode-json-language-server"
     :why "JSON LSP"
     :mac ("npm i -g vscode-langservers-extracted")
     :generic ("npm i -g vscode-langservers-extracted"))
    ("zls"
     :why "Zig LSP"
     :mac ("brew install zls")
     :generic ("Install zls (often from Zig community packages)"))
    ("texlab"
     :why "LaTeX LSP"
     :mac ("brew install texlab")
     :generic ("Install texlab from your OS packages or releases"))
    ("marksman"
     :why "Markdown LSP"
     :mac ("brew install marksman")
     :generic ("Install marksman (binary release or package manager)"))
    ("ocamllsp"
     :why "OCaml LSP"
     :mac ("opam install ocaml-lsp-server")
     :generic ("opam install ocaml-lsp-server")))
  "Install suggestions for language servers.")

(defun oblsk/lsp--command->exe (cmd)
  "Extract an executable name from CMD (eglot server spec)."
  (cond
   ((and (listp cmd) (stringp (car cmd))) (car cmd))
   ((functionp cmd)
    (let ((v (ignore-errors (funcall cmd))))
      (when (and (listp v) (stringp (car v))) (car v))))
   (t nil)))

(defun oblsk/lsp--server-for-current-buffer ()
  "Return the Eglot server executable name for current buffer (if known)."
  (let* ((mm major-mode)
         (entry (cl-find-if
                 (lambda (it)
                   (let ((modes (car it)))
                     (cond
                      ((symbolp modes) (eq mm modes))
                      ((listp modes) (memq mm modes))
                      (t nil))))
                 eglot-server-programs)))
    (when entry
      (oblsk/lsp--command->exe (cdr entry)))))

(defun oblsk/lsp-install-hints ()
  "Show missing language server(s) and install hints."
  (interactive)
  (require 'cl-lib)
  (let* ((buf (get-buffer-create "*oblsk/lsp-install-hints*"))
         (root (oblsk/project-root))
         (cur  (oblsk/lsp--server-for-current-buffer))
         (servers
          (delete-dups
           (delq nil
                 (mapcar (lambda (it) (oblsk/lsp--command->exe (cdr it)))
                         eglot-server-programs)))))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Project: %s\n\n" root))
      (when cur
        (insert (format "Current buffer server: %s  (%s)\n\n"
                        cur (if (executable-find cur) "FOUND" "MISSING"))))
      (insert "Servers configured in eglot-server-programs:\n\n")
      (dolist (s servers)
        (insert (format "- %s: %s\n" s (if (executable-find s) "FOUND" "MISSING")))
        (when (not (executable-find s))
          (let* ((info (assoc s oblsk/lsp-server-install-hints))
                 (why  (plist-get (cdr info) :why))
                 (cmds (if oblsk/is-macos
                           (plist-get (cdr info) :mac)
                         (plist-get (cdr info) :generic))))
            (when why (insert (format "    why: %s\n" why)))
            (when cmds
              (insert "    install:\n")
              (dolist (c cmds) (insert (format "      %s\n" c))))
            (insert "    note: ensure executable is on PATH/exec-path (restart Emacs if needed).\n")))
        (insert "\n"))
      (goto-char (point-min))
      (view-mode 1))
    (pop-to-buffer buf)))

(global-set-key (kbd "C-c l i") #'oblsk/lsp-install-hints)

;;;; -------------------------------------------------------------------
;;;; 12. Debugging: Dape (external, minimal)
;;;; -------------------------------------------------------------------

(defun oblsk/dape--call (fn)
  "Ensure Dape is available, then call FN interactively."
  (oblsk/ensure-package 'dape)
  (require 'dape)
  (call-interactively fn))

(defun oblsk/dape () (interactive) (oblsk/dape--call #'dape))
(defun oblsk/dape-restart () (interactive) (oblsk/dape--call #'dape-restart))
(defun oblsk/dape-kill () (interactive) (oblsk/dape--call #'dape-kill))
(defun oblsk/dape-pause () (interactive) (oblsk/dape--call #'dape-pause))
(defun oblsk/dape-next () (interactive) (oblsk/dape--call #'dape-next))
(defun oblsk/dape-step-in () (interactive) (oblsk/dape--call #'dape-step-in))
(defun oblsk/dape-step-out () (interactive) (oblsk/dape--call #'dape-step-out))
(defun oblsk/dape-breakpoint-toggle () (interactive) (oblsk/dape--call #'dape-breakpoint-toggle))
(defun oblsk/dape-breakpoint-expression () (interactive) (oblsk/dape--call #'dape-breakpoint-expression))
(defun oblsk/dape-breakpoint-log () (interactive) (oblsk/dape--call #'dape-breakpoint-log))
(defun oblsk/dape-repl () (interactive) (oblsk/dape--call #'dape-repl))
(defun oblsk/dape-info () (interactive) (oblsk/dape--call #'dape-info))
(defun oblsk/dape-evaluate-expression () (interactive) (oblsk/dape--call #'dape-evaluate-expression))

(when (fboundp 'repeat-mode) (repeat-mode 1))
(global-set-key (kbd "C-c d d") #'oblsk/dape)

(with-eval-after-load 'dape
  (setq dape-configs
        `(
          (py
           modes (python-mode python-ts-mode)
           command "python"
           command-args ("-m" "debugpy.adapter")
           :type "python" :request "launch" :name "Python :: Launch"
           :program "${file}" :cwd "${workspaceFolder}")
          (cpp-lldb
           modes (c-mode c-ts-mode c++-mode c++-ts-mode)
           :type "lldb" :request "launch" :name "C/C++ :: LLDB Launch"
           :program "${file}" :cwd "${workspaceFolder}")
          (rust-lldb
           modes (rust-mode rust-ts-mode)
           :type "lldb" :request "launch" :name "Rust :: LLDB Launch"
           :program "${file}" :cwd "${workspaceFolder}")
          (node
           modes (js-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
           :type "pwa-node" :request "launch" :name "Node :: Launch file"
           :program "${file}" :cwd "${workspaceFolder}")
          )))

;;;; -------------------------------------------------------------------
;;;; 13. AI CLIs: Claude Code / Codex / Gemini
;;;; -------------------------------------------------------------------

(defun oblsk/ai--term (name program &rest args)
  "Open an Eat terminal in a vertical split running PROGRAM ARGS in project root."
  (unless (executable-find program)
    (user-error "Executable not found on PATH: %s" program))
  (let* ((default-directory (oblsk/project-root))
         (_ (oblsk/eat--ensure))
         (buf (apply #'eat-make name program nil args)))
    (when buf
      (split-window-right)
      (other-window 1)
      (switch-to-buffer buf))))

(defun oblsk/ai-claude   () (interactive) (oblsk/ai--term "claude"   "claude"))
(defun oblsk/ai-gemini   () (interactive) (oblsk/ai--term "gemini"   "gemini"))
(defun oblsk/ai-codex    () (interactive) (oblsk/ai--term "codex"    "codex"))
(defun oblsk/ai-copilot  () (interactive) (oblsk/ai--term "copilot"  "github-copilot-cli"))

(global-set-key (kbd "C-c a c") #'oblsk/ai-claude)
(global-set-key (kbd "C-c a g") #'oblsk/ai-gemini)
(global-set-key (kbd "C-c a x") #'oblsk/ai-codex)
(global-set-key (kbd "C-c a a") #'oblsk/ai-copilot)

(defun oblsk/ai--region-string ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun oblsk/ai--run-headless (name program args input)
  (unless (executable-find program)
    (user-error "Executable not found on PATH: %s" program))
  (let* ((default-directory (oblsk/project-root))
         (buf (get-buffer-create (format "*%s*" name))))
    (with-current-buffer buf (read-only-mode -1) (erase-buffer))
    (let ((exit
           (with-temp-buffer
             (insert input)
             (apply #'call-process-region
                    (point-min) (point-max)
                    program nil buf nil args))))
      (with-current-buffer buf (goto-char (point-min)) (view-mode 1))
      (pop-to-buffer buf)
      (message "%s exited %s" name exit))))

(defun oblsk/ai-claude-region () (interactive)
  (oblsk/ai--run-headless "claude(headless)" "claude" '("-p" "-")
                          (oblsk/ai--region-string)))

(defun oblsk/ai-codex-region () (interactive)
  (oblsk/ai--run-headless "codex(exec)" "codex"
                          (list "exec" "-" "--cd" (oblsk/project-root))
                          (oblsk/ai--region-string)))

(defun oblsk/ai-gemini-region () (interactive)
  (oblsk/ai--run-headless "gemini(headless)" "gemini" '()
                          (oblsk/ai--region-string)))

(global-set-key (kbd "C-c a C") #'oblsk/ai-claude-region)
(global-set-key (kbd "C-c a G") #'oblsk/ai-gemini-region)
(global-set-key (kbd "C-c a X") #'oblsk/ai-codex-region)

;;;; -------------------------------------------------------------------
;;;; 13b. Terminal emulation: eat
;;;; -------------------------------------------------------------------

(defun oblsk/eat--ensure ()
  "Ensure Eat is installed and loaded."
  (oblsk/ensure-package 'eat)
  (require 'eat))

(with-eval-after-load 'eat
  (setq eat-kill-buffer-on-exit t)
  ;; Set env var so zsh can disable features that cause artifacts (e.g., autosuggestions)
  (add-to-list 'eat-term-shell-integration-env "INSIDE_EMACS_EAT=1"))

(defvar oblsk/eat-backspace-sends-del
  (not (string-match-p "zsh" (oblsk/shell-basename)))
  "When non-nil, send DEL (C-?) for backspace in Eat.")

(with-eval-after-load 'eat
  (defun oblsk/eat-backspace ()
    "Send backspace to Eat, matching common shell expectations."
    (interactive)
    (eat-input-char (if oblsk/eat-backspace-sends-del ?\C-? ?\C-h) 1))
  (defun oblsk/eat--stty-erase (&rest _)
    "Sync stty erase with the key Eat sends for backspace."
    (when (bound-and-true-p eat-terminal)
      (eat-term-send-string
       eat-terminal
       (if oblsk/eat-backspace-sends-del
           "stty erase '^?'\n"
         "stty erase '^H'\n"))))
  (add-hook 'eat-exec-hook #'oblsk/eat--stty-erase)
  (dolist (map '(eat-semi-char-mode-map
                 eat-char-mode-map
                 eat-eshell-semi-char-mode-map
                 eat-eshell-char-mode-map))
    (when (boundp map)
      (define-key (symbol-value map) (kbd "<backspace>") #'oblsk/eat-backspace)
      (define-key (symbol-value map) (kbd "DEL") #'oblsk/eat-backspace))))

(defun oblsk/eat (&optional arg)
  "Open Eat in the project root when available.

With prefix ARG, forward it to Eat to create/switch sessions."
  (interactive "P")
  (oblsk/eat--ensure)
  (if (project-current nil)
      (eat-project arg)
    (eat nil arg)))

(defun oblsk/eat-other-window (&optional arg)
  "Open Eat in a vertical split, preferring the project root."
  (interactive "P")
  (oblsk/eat--ensure)
  (split-window-right)
  (other-window 1)
  (if (project-current nil)
      (eat-project arg)
    (eat nil arg)))

;;;; -------------------------------------------------------------------
;;;; 14. File associations
;;;; -------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?\\'" . tuareg-mode))
(if (and (fboundp 'treesit-available-p)
         (treesit-available-p)
         (fboundp 'rust-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;;;; -------------------------------------------------------------------
;;;; 15. Avy navigation (local modified version, lazy-loaded)
;;;; -------------------------------------------------------------------

(defvar oblsk/avy--loaded nil)

(defun oblsk/avy--ensure ()
  "Load local avy implementation on demand."
  (unless oblsk/avy--loaded
    (load-file (expand-file-name "avy.el" user-emacs-directory))
    (setq oblsk/avy--loaded t)))

(defun oblsk/avy-goto-char ()
  (interactive)
  (oblsk/avy--ensure)
  (call-interactively #'avy-goto-char))

(defun oblsk/avy-goto-word-occurrence ()
  (interactive)
  (oblsk/avy--ensure)
  (call-interactively #'avy-goto-word-occurrence))

(defun oblsk/avy-goto-line ()
  (interactive)
  (oblsk/avy--ensure)
  (call-interactively #'avy-goto-line))

;;;; -------------------------------------------------------------------
;;;; 16. Evil keybindings
;;;; -------------------------------------------------------------------

(setq evil-want-keybinding nil
      evil-disable-insert-state-bindings t)
(setq evil-normal-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-insert-state-cursor 'bar
      evil-replace-state-cursor 'hbar
      evil-emacs-state-cursor 'box)
(defvar evil-mode nil)
(defvar evil-mode-buffers nil)
(oblsk/ensure-package 'evil)
(require 'evil)

(with-eval-after-load 'evil
  (when (not (display-graphic-p))
    (oblsk/ensure-package 'evil-terminal-cursor-changer)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)))

;; Lazy-load heavier integrations.
(autoload 'global-undo-tree-mode "undo-tree" nil t)
(run-with-idle-timer
 0.5 nil
 (lambda ()
   (oblsk/ensure-package 'undo-tree)
   (global-undo-tree-mode 1)))
(setq evil-undo-system 'undo-tree)

(defun oblsk/setup-leader-bindings ()
  "Set up the SPC leader map bindings for Evil."
  (define-key evil-normal-state-map (kbd "SPC") 'oblsk-leader-map)
  (define-key evil-visual-state-map (kbd "SPC") 'oblsk-leader-map)
  (define-key evil-motion-state-map (kbd "SPC") 'oblsk-leader-map))

(run-with-idle-timer
 0.8 nil
 (lambda ()
   (oblsk/ensure-package 'evil-collection)
   (require 'evil-collection)
   (evil-collection-init)
   ;; Re-apply leader bindings after evil-collection-init since it can
   ;; override SPC in some state maps.
   (oblsk/setup-leader-bindings)))

(evil-mode 1)

;; Magit wrappers to avoid loading at startup.
(autoload 'magit-status "magit" nil t)
(autoload 'magit-diff-buffer-file "magit" nil t)
(defun oblsk/magit-status ()
  "Open Magit status, ensuring Magit is installed."
  (interactive)
  (oblsk/ensure-package 'magit)
  (require 'magit)
  (call-interactively #'magit-status))

(defun oblsk/magit-diff-buffer-file ()
  "Magit diff for current file, ensuring Magit is installed."
  (interactive)
  (oblsk/ensure-package 'magit)
  (require 'magit)
  (call-interactively #'magit-diff-buffer-file))

(defun oblsk/evil-select-inner-word ()
  "Select inner word (like viw in vim)."
  (interactive)
  (evil-visual-char)
  (evil-inner-word))

(defun oblsk/evil-move-line-down ()
  "Move visual selection down."
  (interactive)
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (goto-char end)
      (forward-line 1)
      (transpose-regions start end (point) (point))
      (evil-visual-restore))))

(defun oblsk/evil-move-line-up ()
  "Move visual selection up."
  (interactive)
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (goto-char start)
      (forward-line -1)
      (transpose-regions (point) (point) start end)
      (evil-visual-restore))))

(defun oblsk/evil-visual-line-j ()
  "Move down by visual line if no count, otherwise by actual line."
  (interactive)
  (if (or current-prefix-arg (> (prefix-numeric-value current-prefix-arg) 1))
      (evil-next-line (prefix-numeric-value current-prefix-arg))
    (evil-next-visual-line)))

(defun oblsk/evil-visual-line-k ()
  "Move up by visual line if no count, otherwise by actual line."
  (interactive)
  (if (or current-prefix-arg (> (prefix-numeric-value current-prefix-arg) 1))
      (evil-previous-line (prefix-numeric-value current-prefix-arg))
    (evil-previous-visual-line)))

;; Custom remaps
(evil-define-key 'normal 'global (kbd "f") #'oblsk/avy-goto-char)
(evil-define-key 'normal 'global (kbd "w") #'oblsk/avy-goto-word-occurrence)
(evil-define-key 'normal 'global (kbd "W") #'oblsk/evil-select-inner-word)

(evil-define-key 'visual 'global (kbd "J") #'oblsk/evil-move-line-down)
(evil-define-key 'visual 'global (kbd "K") #'oblsk/evil-move-line-up)

(evil-define-key 'insert 'global (kbd "C-c") #'evil-normal-state)
(evil-define-key 'normal 'global (kbd "Q") #'ignore)

;; ESC handling for terminal
(setq evil-esc-delay 0)

;; In minibuffer, use emacs state so ESC works to quit
(add-hook 'minibuffer-setup-hook #'evil-emacs-state)
(define-key evil-emacs-state-map [escape] #'keyboard-quit)

;; Window resizing with arrow keys
(evil-define-key 'normal 'global (kbd "<C-up>") (lambda () (interactive) (enlarge-window -2)))
(evil-define-key 'normal 'global (kbd "<C-down>") (lambda () (interactive) (enlarge-window 2)))
(evil-define-key 'normal 'global (kbd "<C-left>") (lambda () (interactive) (enlarge-window-horizontally -2)))
(evil-define-key 'normal 'global (kbd "<C-right>") (lambda () (interactive) (enlarge-window-horizontally 2)))

;; j/k move by visual line when no count
(evil-define-key 'normal 'global (kbd "j") #'oblsk/evil-visual-line-j)
(evil-define-key 'normal 'global (kbd "k") #'oblsk/evil-visual-line-k)

;; Window navigation (C-h/j/k)
(evil-define-key 'normal 'global (kbd "C-h") #'evil-window-left)
(evil-define-key 'normal 'global (kbd "C-j") #'evil-window-down)
(evil-define-key 'normal 'global (kbd "C-k") #'evil-window-up)

;; C-l: select line + avy (match vanilla)
(evil-define-key 'normal 'global (kbd "C-l") #'oblsk/select-line-and-avy)

;; Close window
(evil-define-key 'normal 'global (kbd "C-q") #'delete-window)

;; Git hunks navigation (requires magit)
(with-eval-after-load 'magit
  (evil-define-key 'normal 'global (kbd "ç") #'magit-section-forward)
  (evil-define-key 'normal 'global (kbd "Ç") #'magit-section-backward))

;;;; -------------------------------------------------------------------
;;;; Perspectives/Sessions (GUI only, like tmux)
;;;; -------------------------------------------------------------------

(when oblsk/is-gui
  (oblsk/ensure-package 'perspective)
  (require 'perspective)

  (setq persp-mode-prefix-key (kbd "C-c M-p")
        persp-state-default-file (expand-file-name "persp-state" user-emacs-directory)
        persp-suppress-no-prefix-key-warning t)

  (persp-mode 1)

  ;; Save sessions on exit, restore on startup
  (add-hook 'kill-emacs-hook #'persp-state-save)

  ;; Helper functions for perspective management
  (defun oblsk/persp-switch-or-create ()
    "Switch to a perspective or create a new one."
    (interactive)
    (let* ((names (persp-names))
           (name (ido-completing-read "Perspective: " names nil nil)))
      (if (member name names)
          (persp-switch name)
        (persp-switch name))))

  (defun oblsk/persp-kill-current ()
    "Kill current perspective and switch to another."
    (interactive)
    (let ((name (persp-current-name)))
      (when (yes-or-no-p (format "Kill perspective '%s'? " name))
        (persp-kill name))))

  (defun oblsk/persp-rename ()
    "Rename current perspective."
    (interactive)
    (let ((new-name (read-string "New name: " (persp-current-name))))
      (persp-rename new-name)))

  ;; Session-related which-key labels (GUI only)
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "SPC p" "project/session"
      "SPC p z" "sessions"
      "SPC p n" "next session"
      "SPC p N" "prev session"
      "SPC p k" "kill session"
      "SPC p r" "rename session"
      "SPC p a" "add buffer"
      "SPC p x" "remove buffer"
      "SPC p S" "save sessions"
      "SPC p L" "load sessions")))

;; Leader map on SPC
(define-prefix-command 'oblsk-leader-map)
(oblsk/setup-leader-bindings)

;; Commands (M-x)
(define-key oblsk-leader-map (kbd "SPC") #'oblsk/mx)

;; Git
(define-key oblsk-leader-map (kbd "G") #'oblsk/magit-status)
(define-key oblsk-leader-map (kbd "h") #'oblsk/magit-diff-buffer-file)

;; Undo tree
(define-key oblsk-leader-map (kbd "u") #'undo-tree-visualize)

;; Files and buffers
(define-key oblsk-leader-map (kbd "f") #'oblsk/find-file)
(define-key oblsk-leader-map (kbd "b") #'switch-to-buffer)
(define-key oblsk-leader-map (kbd "H") #'man)
(define-key oblsk-leader-map (kbd "g") #'rgrep)

;; Window management
(define-key oblsk-leader-map (kbd "s") #'split-window-right)
(define-key oblsk-leader-map (kbd "S") #'split-window-below)
(define-key oblsk-leader-map (kbd "x") #'delete-other-windows)

;; Compile and make
(define-key oblsk-leader-map (kbd "m") #'project-compile)
(define-key oblsk-leader-map (kbd "M") #'compile)
(define-key oblsk-leader-map (kbd "r") #'rgrep)

;; Save and eval
(define-key oblsk-leader-map (kbd "w") #'save-buffer)
(define-key oblsk-leader-map (kbd ";") #'eval-expression)

;; File explorer (dired)
(define-key oblsk-leader-map (kbd "o") #'dired)

;; Project bindings
(define-prefix-command 'oblsk-leader-project-map)
(define-key oblsk-leader-map (kbd "p") 'oblsk-leader-project-map)
(define-key oblsk-leader-project-map (kbd "p") #'project-switch-project)
(define-key oblsk-leader-project-map (kbd "f") #'project-find-file)
(define-key oblsk-leader-project-map (kbd "b") #'project-switch-to-buffer)
(define-key oblsk-leader-project-map (kbd "d") #'project-dired)
(define-key oblsk-leader-project-map (kbd "s") #'project-shell)
(define-key oblsk-leader-project-map (kbd "c") #'project-compile)

;; Perspective/Session bindings under SPC p (GUI only)
(when oblsk/is-gui
  (define-key oblsk-leader-project-map (kbd "z") #'oblsk/persp-switch-or-create)
  (define-key oblsk-leader-project-map (kbd "n") #'persp-next)
  (define-key oblsk-leader-project-map (kbd "N") #'persp-prev)
  (define-key oblsk-leader-project-map (kbd "k") #'oblsk/persp-kill-current)
  (define-key oblsk-leader-project-map (kbd "r") #'oblsk/persp-rename)
  (define-key oblsk-leader-project-map (kbd "a") #'persp-add-buffer)
  (define-key oblsk-leader-project-map (kbd "x") #'persp-remove-buffer)
  (define-key oblsk-leader-project-map (kbd "S") #'persp-state-save)
  (define-key oblsk-leader-project-map (kbd "L") #'persp-state-load)
  (define-key oblsk-leader-project-map (kbd "1") (lambda () (interactive) (persp-switch-by-number 1)))
  (define-key oblsk-leader-project-map (kbd "2") (lambda () (interactive) (persp-switch-by-number 2)))
  (define-key oblsk-leader-project-map (kbd "3") (lambda () (interactive) (persp-switch-by-number 3)))
  (define-key oblsk-leader-project-map (kbd "4") (lambda () (interactive) (persp-switch-by-number 4)))
  (define-key oblsk-leader-project-map (kbd "5") (lambda () (interactive) (persp-switch-by-number 5))))

;; LSP bindings
(define-prefix-command 'oblsk-leader-lsp-map)
(define-key oblsk-leader-map (kbd "l") 'oblsk-leader-lsp-map)
(define-key oblsk-leader-lsp-map (kbd "i") #'oblsk/lsp-install-hints)
(define-key oblsk-leader-lsp-map (kbd "r") #'eglot-rename)
(define-key oblsk-leader-lsp-map (kbd "a") #'eglot-code-actions)
(define-key oblsk-leader-lsp-map (kbd "f") #'eglot-format)

;; DAP/Dape bindings
(define-prefix-command 'oblsk-leader-debug-map)
(define-key oblsk-leader-map (kbd "d") 'oblsk-leader-debug-map)
(define-key oblsk-leader-debug-map (kbd "d") #'oblsk/dape)
(define-key oblsk-leader-debug-map (kbd "l") #'oblsk/dape-restart)
(define-key oblsk-leader-debug-map (kbd "t") #'oblsk/dape-kill)
(define-key oblsk-leader-debug-map (kbd "p") #'oblsk/dape-pause)
(define-key oblsk-leader-debug-map (kbd "o") #'oblsk/dape-next)
(define-key oblsk-leader-debug-map (kbd "i") #'oblsk/dape-step-in)
(define-key oblsk-leader-debug-map (kbd "O") #'oblsk/dape-step-out)
(define-key oblsk-leader-debug-map (kbd "b") #'oblsk/dape-breakpoint-toggle)
(define-key oblsk-leader-debug-map (kbd "B") #'oblsk/dape-breakpoint-expression)
(define-key oblsk-leader-debug-map (kbd "L") #'oblsk/dape-breakpoint-log)
(define-key oblsk-leader-debug-map (kbd "r") #'oblsk/dape-repl)
(define-key oblsk-leader-debug-map (kbd "h") #'oblsk/dape-info)
(define-key oblsk-leader-debug-map (kbd "e") #'oblsk/dape-evaluate-expression)

;; AI bindings
(define-prefix-command 'oblsk-leader-ai-map)
(define-key oblsk-leader-map (kbd "a") 'oblsk-leader-ai-map)
(define-key oblsk-leader-ai-map (kbd "c") #'oblsk/ai-claude)
(define-key oblsk-leader-ai-map (kbd "g") #'oblsk/ai-gemini)
(define-key oblsk-leader-ai-map (kbd "x") #'oblsk/ai-codex)
(define-key oblsk-leader-ai-map (kbd "a") #'oblsk/ai-copilot)

;; Terminal
(define-key oblsk-leader-map (kbd "t") #'oblsk/eat)
(define-key oblsk-leader-map (kbd "T") #'oblsk/eat-other-window)

;; Flymake bindings
(define-prefix-command 'oblsk-leader-flymake-map)
(define-key oblsk-leader-map (kbd "e") 'oblsk-leader-flymake-map)
(define-key oblsk-leader-flymake-map (kbd "n") #'flymake-goto-next-error)
(define-key oblsk-leader-flymake-map (kbd "p") #'flymake-goto-prev-error)
(define-key oblsk-leader-flymake-map (kbd "l") #'flymake-show-buffer-diagnostics)

(provide 'init)
;;; init.el ends here
