;;; codex-ide.el --- Codex CLI integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: phelrine
;; Keywords: tools, ai, codex
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (vterm "0.0.1") (project "0.9.8"))

;;; Commentary:

;; This package provides Codex CLI integration for Emacs, offering
;; session management, terminal integration, and development assistance
;; similar to claude-code-ide.

;;; Code:

(require 'vterm)
(require 'project)
(require 'cl-lib)

;;; Customization

(defgroup codex-ide nil
  "Codex CLI integration for Emacs."
  :group 'tools
  :prefix "codex-ide-")

(defcustom codex-ide-command "codex"
  "Command to run Codex CLI."
  :type 'string
  :group 'codex-ide)

(defcustom codex-ide-default-model nil
  "Default model for Codex sessions."
  :type '(choice (const :tag "Use CLI default" nil)
                 (string :tag "Model name"))
  :group 'codex-ide)

(defcustom codex-ide-sandbox-mode nil
  "Enable sandbox mode by default."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-extra-args nil
  "Extra arguments to pass to Codex CLI."
  :type '(repeat string)
  :group 'codex-ide)

(defcustom codex-ide-auto-switch-to-buffer t
  "Automatically switch to Codex buffer when starting a session."
  :type 'boolean
  :group 'codex-ide)

;;; Internal Variables

(defvar codex-ide--processes (make-hash-table :test 'equal)
  "Hash table mapping project roots to Codex process information.")

(defvar codex-ide--recent-buffer nil
  "Most recently used Codex buffer.")

(defvar codex-ide--debug-buffer "*codex-ide-debug*"
  "Buffer name for debug information.")

;;; Process Management Structures

(cl-defstruct codex-ide-session
  "Structure representing a Codex IDE session."
  process
  buffer
  project-root
  model
  sandbox
  args)

;;; Utility Functions

(defun codex-ide--project-root ()
  "Get the current project root."
  (or (project-root (project-current))
      default-directory))

(defun codex-ide--session-buffer-name (project-root)
  "Generate buffer name for PROJECT-ROOT."
  (format "*codex:%s*" (file-name-nondirectory (directory-file-name project-root))))

(defun codex-ide--get-session (&optional project-root)
  "Get Codex session for PROJECT-ROOT."
  (let* ((root (or project-root (codex-ide--project-root)))
         (session (gethash root codex-ide--processes)))
    (when session
      ;; Check if session is still alive
      (if (codex-ide--session-alive-p session)
          session
        ;; Clean up dead session
        (codex-ide--remove-session root)
        nil))))

(defun codex-ide--set-session (session &optional project-root)
  "Set Codex SESSION for PROJECT-ROOT."
  (let ((root (or project-root (codex-ide--project-root))))
    (puthash root session codex-ide--processes)))

(defun codex-ide--remove-session (&optional project-root)
  "Remove Codex session for PROJECT-ROOT."
  (let ((root (or project-root (codex-ide--project-root))))
    (remhash root codex-ide--processes)))

(defun codex-ide--session-alive-p (session)
  "Check if SESSION is still alive."
  (let ((process (codex-ide-session-process session))
        (buffer (codex-ide-session-buffer session)))
    (and process
         (process-live-p process)
         (buffer-live-p buffer))))

(defun codex-ide--cleanup-session (project-root)
  "Clean up dead session for PROJECT-ROOT."
  (let ((session (gethash project-root codex-ide--processes)))
    (when session
      (codex-ide--log-debug "Cleaning up session for %s" project-root)
      (codex-ide--remove-session project-root))))

(defun codex-ide--cleanup-dead-sessions ()
  "Clean up all dead sessions from the process hash table."
  (let ((dead-roots '()))
    (maphash (lambda (root session)
               (unless (codex-ide--session-alive-p session)
                 (push root dead-roots)))
             codex-ide--processes)
    (dolist (root dead-roots)
      (codex-ide--log-debug "Removing dead session for %s" root)
      (codex-ide--remove-session root))))

(defun codex-ide--build-command-args (project-root &optional model sandbox extra-args resume)
  "Build command arguments for Codex CLI."
  (let ((args (list "--cd" project-root)))
    (when resume
      (setq args (append args '("resume" "--last"))))
    (when model
      (setq args (append args (list "--model" model))))
    (when sandbox
      (setq args (append args '("--sandbox"))))
    (when extra-args
      (setq args (append args extra-args)))
    args))

(defun codex-ide--log-debug (message &rest args)
  "Log debug MESSAGE with ARGS to debug buffer."
  (with-current-buffer (get-buffer-create codex-ide--debug-buffer)
    (goto-char (point-max))
    (insert (format "[%s] %s\n"
                    (format-time-string "%Y-%m-%d %H:%M:%S")
                    (apply #'format message args)))))

(defun codex-ide--setup-finished-buffer-keys ()
  "Set up key bindings for finished Codex buffer."
  (local-set-key (kbd "q") #'codex-ide-quit-buffer)
  (local-set-key (kbd "r") #'codex-ide-restart-session)
  (message "Process finished. Press 'q' to close buffer, 'r' to restart session."))

(defun codex-ide-quit-buffer ()
  "Quit the current Codex buffer."
  (interactive)
  (quit-window t))

(defun codex-ide-restart-session ()
  "Restart Codex session in current buffer."
  (interactive)
  (let ((project-root (codex-ide--project-root)))
    (kill-buffer (current-buffer))
    (codex-ide-start)))

;;; Session Management

(defun codex-ide--start-process (project-root &optional model sandbox extra-args resume)
  "Start Codex process for PROJECT-ROOT."
  (let* ((buffer-name (codex-ide--session-buffer-name project-root))
         (existing-buffer (get-buffer buffer-name))
         (args (codex-ide--build-command-args project-root model sandbox extra-args resume)))

    (codex-ide--log-debug "Starting Codex process with args: %s" args)

    ;; Kill existing buffer if it exists to avoid vterm mode conflicts
    (when existing-buffer
      (kill-buffer existing-buffer))

    (let* ((buffer (generate-new-buffer buffer-name))
           (default-directory project-root)
           (command-line (format "%s %s" codex-ide-command (string-join args " ")))
           (vterm-shell command-line)
           (vterm-kill-buffer-on-exit t)) ; Kill buffer when process exits

      (with-current-buffer buffer
        (vterm-mode))

      (let* ((process (get-buffer-process buffer))
             (session (make-codex-ide-session
                       :process process
                       :buffer buffer
                       :project-root project-root
                       :model model
                       :sandbox sandbox
                       :args extra-args)))

        ;; Set up process sentinel for automatic cleanup
        (when process
          (set-process-sentinel process
                                (lambda (proc event)
                                  (when (memq (process-status proc) '(exit signal))
                                    (let ((proc-buffer (process-buffer proc)))
                                      (codex-ide--cleanup-session project-root)
                                      (codex-ide--log-debug "Process %s finished: %s" proc (string-trim event))
                                      ;; Handle buffer based on exit status
                                      (when (buffer-live-p proc-buffer)
                                        (with-current-buffer proc-buffer
                                          (goto-char (point-max))
                                          (let ((inhibit-read-only t))
                                            (insert (format "\n\n[Process finished: %s]" (string-trim event))))
                                          ;; Keep buffer alive but read-only for user to see results
                                          (setq buffer-read-only t)
                                          ;; Override vterm's kill behavior
                                          (setq-local vterm-kill-buffer-on-exit nil)
                                          ;; Set up key binding for closing buffer
                                          (codex-ide--setup-finished-buffer-keys))))))))

        (codex-ide--set-session session project-root)
        (setq codex-ide--recent-buffer buffer)
        session))))

;;; Interactive Commands

;;;###autoload
(defun codex-ide-start (&optional model sandbox)
  "Start a new Codex session."
  (interactive)
  (let* ((project-root (codex-ide--project-root))
         (existing-session (codex-ide--get-session project-root)))

    (when existing-session
      (user-error "Codex session already exists for this project. Use `codex-ide-resume' to continue"))

    (unless (executable-find codex-ide-command)
      (user-error "Codex CLI not found. Please install Codex CLI"))

    (let ((session (codex-ide--start-process
                    project-root
                    (or model codex-ide-default-model)
                    (or sandbox codex-ide-sandbox-mode)
                    codex-ide-extra-args)))

      (when codex-ide-auto-switch-to-buffer
        (switch-to-buffer (codex-ide-session-buffer session)))

      (message "Started Codex session for %s" project-root))))

;;;###autoload
(defun codex-ide-resume ()
  "Resume the last Codex session for current project."
  (interactive)
  (let* ((project-root (codex-ide--project-root))
         (existing-session (codex-ide--get-session project-root)))

    (when existing-session
      (user-error "Codex session already active for this project"))

    (unless (executable-find codex-ide-command)
      (user-error "Codex CLI not found. Please install Codex CLI"))

    (let ((session (codex-ide--start-process
                    project-root
                    codex-ide-default-model
                    codex-ide-sandbox-mode
                    codex-ide-extra-args
                    t))) ; resume = t

      (when codex-ide-auto-switch-to-buffer
        (switch-to-buffer (codex-ide-session-buffer session)))

      (message "Resumed Codex session for %s" project-root))))

;;;###autoload
(defun codex-ide-resume-with-selection ()
  "Resume a Codex session with conversation selection."
  (interactive)
  (let* ((project-root (codex-ide--project-root))
         (existing-session (codex-ide--get-session project-root)))

    (when existing-session
      (user-error "Codex session already active for this project"))

    (unless (executable-find codex-ide-command)
      (user-error "Codex CLI not found. Please install Codex CLI"))

    ;; Start interactive resume (codex will show conversation list)
    (let ((session (codex-ide--start-process
                    project-root
                    codex-ide-default-model
                    codex-ide-sandbox-mode
                    (append codex-ide-extra-args '("resume"))
                    nil))) ; resume = nil, use extra-args instead

      (when codex-ide-auto-switch-to-buffer
        (switch-to-buffer (codex-ide-session-buffer session)))

      (message "Started Codex resume with selection for %s" project-root))))

;;;###autoload
(defun codex-ide-stop ()
  "Stop the Codex session for current project."
  (interactive)
  (let* ((project-root (codex-ide--project-root))
         (session (codex-ide--get-session project-root)))

    (unless session
      (user-error "No active Codex session for this project"))

    (when (codex-ide-session-process session)
      (delete-process (codex-ide-session-process session)))

    (when (buffer-live-p (codex-ide-session-buffer session))
      (kill-buffer (codex-ide-session-buffer session)))

    (codex-ide--remove-session project-root)
    (message "Stopped Codex session for %s" project-root)))

;;;###autoload
(defun codex-ide-switch-to-buffer ()
  "Switch to the Codex buffer for current project."
  (interactive)
  (let* ((project-root (codex-ide--project-root))
         (session (codex-ide--get-session project-root)))

    (if session
        (switch-to-buffer (codex-ide-session-buffer session))
      (if codex-ide--recent-buffer
          (switch-to-buffer codex-ide--recent-buffer)
        (user-error "No active Codex session for this project")))))

;;;###autoload
(defun codex-ide-toggle ()
  "Toggle visibility of Codex buffer in side window."
  (interactive)
  (let* ((project-root (codex-ide--project-root))
         (session (codex-ide--get-session project-root)))

    (if session
        (let ((buffer (codex-ide-session-buffer session))
              (window (get-buffer-window (codex-ide-session-buffer session))))
          (if window
              (quit-window nil window)
            (display-buffer-in-side-window buffer '((side . right) (window-width . 0.5)))))
      (user-error "No active Codex session for this project"))))

;;;###autoload
(defun codex-ide-list-sessions ()
  "List all active Codex sessions and switch to selected one."
  (interactive)
  (codex-ide--cleanup-dead-sessions)
  (let ((sessions '()))
    (maphash (lambda (root session)
               (when (codex-ide--session-alive-p session)
                 (push (cons (abbreviate-file-name root) root) sessions)))
             codex-ide--processes)
    (if sessions
        (let ((choice (completing-read "Switch to Codex session: " sessions nil t)))
          (when choice
            (let* ((root (alist-get choice sessions nil nil #'string=))
                   (session (codex-ide--get-session root)))
              (if (and session (buffer-live-p (codex-ide-session-buffer session)))
                  (switch-to-buffer (codex-ide-session-buffer session))
                (user-error "Buffer for session %s no longer exists" choice)))))
      (message "No active Codex sessions"))))

;;;###autoload
(defun codex-ide-show-debug ()
  "Show debug buffer."
  (interactive)
  (display-buffer codex-ide--debug-buffer))

;;;###autoload
(defun codex-ide-version ()
  "Show Codex CLI version."
  (interactive)
  (if (executable-find codex-ide-command)
      (let ((version (shell-command-to-string (format "%s --version" codex-ide-command))))
        (message "Codex CLI: %s" (string-trim version)))
    (message "Codex CLI not found")))

;;; Setup

;;;###autoload
(defun codex-ide-setup ()
  "Setup Codex IDE integration."
  (interactive)
  (codex-ide--log-debug "Codex IDE setup completed"))

(provide 'codex-ide)

;;; codex-ide.el ends here