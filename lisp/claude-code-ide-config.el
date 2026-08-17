;;; claude-code-ide-config.el --- Configuration for claude-code-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Shogo Nagasaka
;; Keywords: tools

;;; Commentary:

;; Custom configuration for claude-code-ide including:
;; - C-o keybinding to other-window (consistent with global binding)
;; - C-c o keybinding to send C-o to terminal (for verbose toggle)
;; - Prompt input through the CLI's own external-editor key, composed in
;;   Emacs and handed straight back
;; - Per-repository environment (Claude account, tool versions) via mise

;;; Code:

(require 'claude-code-ide)
(require 'claude-code-ide-mcp)
(require 'claude-code-ide-transient)
(require 'posframe)
(require 'mise-env)
;; For `server-edit' and `server-window': the CLI's external-editor key
;; reaches Emacs through the server, and we take over how it displays.
(require 'server)

;;; C-o Terminal Keybinding

(defun claude-code-ide-config--send-ctrl (letter)
  "Send LETTER with the control modifier to the current terminal buffer.
LETTER is a one-character lowercase string such as \"o\".  Emacs binds
most control keys itself, so reaching the CLI's own bindings means
handing the key to the backend rather than letting the command loop
see it."
  (cond
   ((eq claude-code-ide-terminal-backend 'vterm)
    (when (fboundp 'vterm-send-string)
      (vterm-send-string (string (- (aref letter 0) ?a -1)))))
   ((eq claude-code-ide-terminal-backend 'eat)
    (when (and (boundp 'eat-terminal)
               eat-terminal
               (fboundp 'eat-term-send-string))
      (eat-term-send-string eat-terminal (string (- (aref letter 0) ?a -1)))))
   ((eq claude-code-ide-terminal-backend 'ghostel)
    (when (fboundp 'ghostel-send-key)
      (ghostel-send-key letter "ctrl")))))

(defun claude-code-ide-send-c-o ()
  "Send C-o directly to the terminal in Claude Code IDE buffer."
  (interactive)
  (claude-code-ide-config--send-ctrl "o"))

;;; Prompt Input

;; Define a minor mode to ensure our keybindings take precedence
(defvar claude-code-ide-prompt-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x j") #'claude-code-ide-send-prompt)
    map)
  "Keymap for `claude-code-ide-prompt-input-mode'.")

(define-minor-mode claude-code-ide-prompt-input-mode
  "Minor mode binding the prompt input command in a Claude session buffer.
This mode's keymap takes precedence over the terminal's local bindings."
  :lighter nil
  :keymap claude-code-ide-prompt-input-mode-map)

;;; External Editor Input

;; The CLI's `chat:externalEditor' key (C-g) writes its input line to a
;; temp file, runs $EDITOR on it, and reads the file back afterwards.
;; `claude-code-ide-config--export-editor' points $EDITOR at this Emacs,
;; so pressing it hands composing over here -- exactly, including
;; whatever was already typed on the CLI side, with no screen scraping.

(defcustom claude-code-ide-config-external-prompt-send-delay 0.4
  "Seconds to wait before pressing RET for `\\[claude-code-ide-external-prompt-finish-and-send]'.
The CLI reads the temp file back only after `emacsclient' exits, so
submitting immediately would send an input line the CLI has not
refreshed yet."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-config-external-prompt-display 'posframe
  "How the prompt file the CLI hands to Emacs is presented.

`posframe' floats it in a child frame over whatever you were looking at,
keeping the session's own window visible underneath.  `window' opens it
along the bottom of the current frame instead.

Nothing blocks in `recursive-edit' either way: the child frame shows an
ordinary file buffer, so an async MCP handler stealing focus mid-edit
costs nothing."
  :type '(choice (const :tag "Floating posframe" posframe)
                 (const :tag "Ordinary window" window))
  :group 'claude-code-ide)

(defcustom claude-code-ide-config-external-prompt-size '(80 . 12)
  "Width and height in characters of the posframe prompt."
  :type '(cons integer integer)
  :group 'claude-code-ide)

(defconst claude-code-ide-config--external-prompt-file-regexp
  "/claude-prompt-[^/]*\\.md\\'"
  "Match the temp file the CLI hands to $EDITOR for its input line.")

(defvar claude-code-ide-external-prompt--pending-session nil
  "Session whose external edit we are waiting for Emacs to be handed.
Set when C-g is pressed and consumed by the temp file's buffer, which
has no other way to know which terminal it came from.")

(defvar-local claude-code-ide-external-prompt--session nil
  "Session this temp file's contents belong to.")

(defvar claude-code-ide-external-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-code-ide-external-prompt-finish-and-send)
    (define-key map (kbd "C-c C-t") #'claude-code-ide-external-prompt-finish)
    map)
  "Keymap for `claude-code-ide-external-prompt-mode'.")

(define-derived-mode claude-code-ide-external-prompt-mode text-mode "Claude-Ext"
  "Major mode for the prompt file the Claude CLI hands to Emacs."
  (when (fboundp 'skk-mode)
    (skk-mode 1))
  (setq claude-code-ide-external-prompt--session
        (prog1 claude-code-ide-external-prompt--pending-session
          (setq claude-code-ide-external-prompt--pending-session nil)))
  (claude-code-ide-external-prompt--claim-server-window)
  (setq header-line-format
        (substitute-command-keys
         (concat "\\<claude-code-ide-external-prompt-mode-map>"
                 "\\[claude-code-ide-external-prompt-finish-and-send]: send    "
                 "\\[claude-code-ide-external-prompt-finish]: back to CLI unsent"))))

(defvar claude-code-ide-external-prompt--saved-server-window nil
  "Value of `server-window' displaced by the current handoff.")

(defun claude-code-ide-external-prompt--claim-server-window ()
  "Arrange for us, not the server, to display this buffer.
`server-switch-buffer' consults `server-window' only after the file has
been visited, so the major mode is the last moment we can intercept it.
The override is undone the instant it fires, so other `emacsclient'
uses keep whatever display the user configured."
  (unless (eq server-window #'claude-code-ide-external-prompt--display)
    (setq claude-code-ide-external-prompt--saved-server-window server-window))
  (setq server-window #'claude-code-ide-external-prompt--display))

(defun claude-code-ide-external-prompt--show-window (buffer)
  "Show BUFFER along the bottom of the current frame and select it."
  (when-let ((win (display-buffer buffer
                                  '((display-buffer-in-side-window)
                                    (side . bottom)
                                    (slot . 1)
                                    (window-height . 12)))))
    (select-window win)))

(defun claude-code-ide-external-prompt--show-posframe (buffer)
  "Float BUFFER in a focused child frame, caret after the existing draft."
  (let ((end (with-current-buffer buffer (point-max))))
    (posframe-show buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :width (car claude-code-ide-config-external-prompt-size)
                   :height (cdr claude-code-ide-config-external-prompt-size)
                   :border-width 2
                   :border-color "#4CAF50"
                   :left-fringe 8
                   :right-fringe 8
                   :background-color (face-background 'default nil t)
                   :foreground-color (face-foreground 'default nil t)
                   :accept-focus t
                   ;; Posframe drops both by default.  They are the child
                   ;; frame's only status readouts: the key help lives in
                   ;; the header line, SKK's input mode in the mode line.
                   :respect-header-line t
                   :respect-mode-line t
                   ;; Posframe hides the cursor and pins the window to
                   ;; position 0 unless asked otherwise, which would leave
                   ;; you typing with nothing to aim at, behind the draft.
                   :cursor 'box
                   :window-point end)
    (when-let ((frame (posframe--find-existing-posframe buffer)))
      (select-frame-set-input-focus frame)
      (select-window (frame-first-window frame))
      (with-current-buffer buffer (goto-char end)))))

(defun claude-code-ide-external-prompt--display (buffer)
  "Display BUFFER as `claude-code-ide-config-external-prompt-display' asks.
Installed as `server-window' for the duration of one handoff; restoring
it first means an error below cannot strand the override."
  (setq server-window claude-code-ide-external-prompt--saved-server-window)
  (if (eq claude-code-ide-config-external-prompt-display 'posframe)
      (claude-code-ide-external-prompt--show-posframe buffer)
    (claude-code-ide-external-prompt--show-window buffer)))

(defun claude-code-ide-external-prompt--hide (buffer)
  "Take BUFFER's child frame down and hand focus back to the main frame."
  (when (and (eq claude-code-ide-config-external-prompt-display 'posframe)
             (fboundp 'posframe-delete))
    (posframe-delete buffer)
    (when-let ((main (seq-find (lambda (frame)
                                 (not (frame-parameter frame 'parent-frame)))
                               (frame-list))))
      (select-frame-set-input-focus main))))

(defun claude-code-ide-external-prompt--ensure-editable ()
  "Exempt the CLI's prompt file from the global read-only-by-default rule.
`init.el' puts freshly visited files into `read-only-mode' (and hence
`view-mode') unless their major mode is on its exempt list, where
`git-commit-mode' already sits for exactly this reason: a file another
program handed us to type into.  Runs appended to `find-file-hook' so
it lands after the rule it overrides."
  (when (derived-mode-p 'claude-code-ide-external-prompt-mode)
    (read-only-mode -1)))

(defun claude-code-ide-external-prompt-finish ()
  "Hand this file back to the CLI, leaving it unsubmitted in the input line."
  (interactive)
  ;; The CLI re-reads the file from disk once `emacsclient' exits, and
  ;; `server-edit' only auto-saves buffers matching
  ;; `server-temp-file-regexp' -- which this path does not.  Without
  ;; this the CLI would read back the text the user just replaced.
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer))
  ;; Down before the handover: the CLI redraws as soon as the client is
  ;; released, and would do it underneath a stale child frame.
  (claude-code-ide-external-prompt--hide (current-buffer))
  (server-edit))

(defun claude-code-ide-external-prompt-finish-and-send ()
  "Hand this file back to the CLI and submit it."
  (interactive)
  (let ((buffer (when-let ((session claude-code-ide-external-prompt--session))
                  (claude-code-ide-mcp-session-buffer session))))
    (claude-code-ide-external-prompt-finish)
    (when (buffer-live-p buffer)
      (run-at-time claude-code-ide-config-external-prompt-send-delay nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (claude-code-ide--terminal-send-return))))))))

(defun claude-code-ide-send-prompt-externally (orig-fun &optional prompt session)
  "Compose an interactive prompt through the CLI's own external-editor key.
PROMPT non-nil means a programmatic call, which goes to ORIG-FUN
unchanged.  SESSION defaults to the one owning the current buffer."
  (if prompt
      (funcall orig-fun prompt session)
    (if-let* ((target (or session
                          (claude-code-ide--buffer-session (current-buffer))))
              (buffer (claude-code-ide-mcp-session-buffer target)))
        (progn
          (setq claude-code-ide-external-prompt--pending-session target)
          (with-current-buffer buffer
            (claude-code-ide-config--send-ctrl "g")))
      (user-error "No Claude Code session for this buffer"))))

(defun claude-code-ide-config--setup-session-buffer (buffer-and-process)
  "Set up keybindings in the freshly created Claude session buffer.
BUFFER-AND-PROCESS is `claude-code-ide--create-terminal-session's
return value, passed through unchanged.  Terminal mode hooks are too
early for this: `claude-code-ide--session-buffer-p' relies on the
session backpointer, which is only set after the terminal buffer is
created."
  (when-let ((buf (car-safe buffer-and-process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (local-set-key (kbd "C-o") #'other-window)
        (local-set-key (kbd "C-c o") #'claude-code-ide-send-c-o)
        (claude-code-ide-prompt-input-mode 1))))
  buffer-and-process)

;;; Per-Repository Environment

(defun claude-code-ide-config--project-env (dir)
  "Return (PROCESS-ENVIRONMENT . EXEC-PATH) for DIR as mise reports it.
Returns nil when the lookup fails, so callers keep the ambient
environment rather than starting Claude with a half-built one."
  (condition-case err
      (with-temp-buffer
        (setq default-directory dir)
        (mise-env-update)
        (cons process-environment exec-path))
    (error
     (claude-code-ide-debug "mise environment lookup failed for %s: %s"
                            dir (error-message-string err))
     nil)))

(defun claude-code-ide-config--editor-command ()
  "Return a command that opens a file in this Emacs, or nil if none can be.
The CLI runs `$EDITOR' on its input line for \\`C-g' (its
`chat:externalEditor' action) and reads the file back afterwards, so the
command must block until the user is done -- hence plain `emacsclient'
with no `-n'.  The CLI inherits Emacs' `TMPDIR', so it finds the server
socket without further help.

Resolved from `invocation-directory' before `exec-path': a session
started early in Emacs' startup runs before `exec-path-from-shell', when
a PATH lookup finds nothing and the override would be skipped without a
word.  The client shipped alongside this Emacs also matches its version."
  (seq-find (lambda (file)
              (and (file-executable-p file)
                   (not (file-directory-p file))))
            (delq nil
                  (list (expand-file-name "bin/emacsclient" invocation-directory)
                        (expand-file-name "emacsclient" invocation-directory)
                        (executable-find "emacsclient")))))

(defun claude-code-ide-config--export-editor ()
  "Point EDITOR at this Emacs for everything Emacs spawns.
Emacs inherits no EDITOR of its own on macOS, so nothing is overridden.

Written to the global value rather than with `setenv\='.  `mise-env\='
gives every prog-mode buffer a buffer-local `process-environment\=', and
the terminal is spawned from a different buffer again, so a `setenv\='
here would be discarded before the CLI ever started."
  (when-let ((editor (claude-code-ide-config--editor-command)))
    (setq-default process-environment
                  (append (list (concat "EDITOR=" editor)
                                (concat "VISUAL=" editor))
                          (seq-remove
                           (lambda (entry)
                             (or (string-prefix-p "EDITOR=" entry)
                                 (string-prefix-p "VISUAL=" entry)))
                           (default-value 'process-environment))))))

(defun claude-code-ide-config--with-project-env (orig-fun buffer-name working-dir &rest args)
  "Advice around `claude-code-ide--create-terminal-session'.
Start the session under WORKING-DIR's mise environment so a repository's
`mise.toml' [env] entries reach the Claude process.  The one that matters
most is CLAUDE_CONFIG_DIR: it points Claude at a different config
directory, which carries its own credentials, so a repository can run
under a different account.  Without this advice the session would
inherit whatever environment Emacs itself was started with.

EDITOR is deliberately not set here.  This binding would not survive the
buffer switch `claude-code-ide--create-terminal-session\=' makes before
spawning, so `claude-code-ide-config--export-editor\=' handles it
globally instead and a project\='s own EDITOR is left to win."
  (let* ((env (claude-code-ide-config--project-env working-dir))
         (process-environment (if env (car env) process-environment))
         (exec-path (if env (cdr env) exec-path)))
    (apply orig-fun buffer-name working-dir args)))

;;; Session Tiling

(defvar claude-code-ide-config--saved-window-configuration nil
  "Saved window configuration before tiling.")

(defvar claude-code-ide-config--tiled-p nil
  "Non-nil when sessions are currently tiled.")

(defun claude-code-ide-config--live-sessions ()
  "Return list of sessions with live terminal buffers."
  (let (result)
    (maphash
     (lambda (_id session)
       (when-let ((buf (claude-code-ide-mcp-session-buffer session)))
         (when (buffer-live-p buf)
           (push session result))))
     claude-code-ide-mcp--sessions)
    (nreverse result)))

(defun claude-code-ide-tile-sessions ()
  "Tile Claude Code session buffers in a grid layout."
  (interactive)
  (let* ((sessions (claude-code-ide-config--live-sessions))
         (n (length sessions)))
    (cond
     ((= n 0)
      (message "No matching Claude Code sessions found."))
     (t
      (setq claude-code-ide-config--saved-window-configuration
            (current-window-configuration))
      (setq claude-code-ide-config--tiled-p t)
      ;; Delete claude-code-ide side windows first, since they have
      ;; no-delete-other-windows parameter and survive delete-other-windows
      (dolist (win (window-list))
        (when (and (window-parameter win 'window-side)
                   (claude-code-ide--session-buffer-p (window-buffer win)))
          (delete-window win)))
      (delete-other-windows)
      (let* ((cols (ceiling (sqrt n)))
             (rows (ceiling (/ (float n) cols)))
             (windows '()))
        ;; Split into rows
        (dotimes (r (1- rows))
          (let ((size (/ (window-total-height) (- rows r))))
            (split-window-below size)))
        ;; Split each row into columns
        (let ((row-windows '()))
          ;; Collect the first window of each row
          (let ((w (frame-first-window)))
            (dotimes (_ rows)
              (push w row-windows)
              (setq w (ignore-errors (window-in-direction 'below w))))
            (setq row-windows (nreverse row-windows)))
          ;; Split each row window into columns
          (let ((session-idx 0))
            (dotimes (r rows)
              (let* ((row-win (nth r row-windows))
                     (cols-in-row (min cols (- n session-idx))))
                (select-window row-win)
                (dotimes (c (1- cols-in-row))
                  (let ((size (/ (window-total-width row-win) (- cols-in-row c))))
                    (split-window-right size)))
                ;; Assign buffers to windows in this row
                (let ((w row-win))
                  (dotimes (_ cols-in-row)
                    (push w windows)
                    (setq w (ignore-errors (window-in-direction 'right w)))))
                (cl-incf session-idx cols-in-row)))))
        ;; Assign session buffers to windows
        (setq windows (nreverse windows))
        (cl-loop for session in sessions
                 for win in windows
                 do (set-window-buffer win (claude-code-ide-mcp-session-buffer session))))
      (message "Tiled %d Claude Code session(s)." n)))))

(defun claude-code-ide-untile-sessions ()
  "Restore window configuration from before tiling."
  (interactive)
  (if (not claude-code-ide-config--tiled-p)
      (message "Sessions are not currently tiled.")
    (set-window-configuration claude-code-ide-config--saved-window-configuration)
    (setq claude-code-ide-config--saved-window-configuration nil)
    (setq claude-code-ide-config--tiled-p nil)
    (message "Restored previous window layout.")))

(transient-define-prefix claude-code-ide-tile-menu ()
  "Tile Claude Code session buffers."
  ["Tile Sessions"
   ("t" "Tile all sessions" claude-code-ide-tile-sessions)
   ("u" "Untile (restore)" claude-code-ide-untile-sessions)])

;;; Side Window Fix

(defun claude-code-ide-config--delete-other-windows-advice (orig-fun &optional window interactive)
  "Handle `delete-other-windows' when the target is a side window.
When called from a side window, toggle side windows off, display the
buffer in the main area, then delete other windows normally."
  (let ((win (or window (selected-window))))
    (if (not (window-parameter win 'window-side))
        (funcall orig-fun window interactive)
      (let ((buf (window-buffer win)))
        (window-toggle-side-windows)
        (set-window-buffer (selected-window) buf)
        (funcall orig-fun nil interactive)))))

(defun claude-code-ide-config-setup ()
  "Setup claude-code-ide custom configuration."
  ;; Keybindings and posframe input for new session buffers.  Advice on
  ;; session creation rather than terminal mode hooks so every backend
  ;; (vterm/eat/ghostel) is covered without per-mode registration.
  (advice-add 'claude-code-ide--create-terminal-session :filter-return
              #'claude-code-ide-config--setup-session-buffer)

  ;; Hand the CLI's external-editor key back to this Emacs.
  (claude-code-ide-config--export-editor)

  ;; Per-repository environment, including which Claude account to use.
  (advice-add 'claude-code-ide--create-terminal-session :around
              #'claude-code-ide-config--with-project-env)

  ;; C-x j presses the CLI's own external-editor key for us
  (advice-add 'claude-code-ide-send-prompt :around
              #'claude-code-ide-send-prompt-externally)

  ;; Claim the temp file the CLI hands to $EDITOR.  Needed even without
  ;; the advice: C-g works from the terminal on its own.
  (add-to-list 'auto-mode-alist
               (cons claude-code-ide-config--external-prompt-file-regexp
                     #'claude-code-ide-external-prompt-mode))
  (add-hook 'find-file-hook #'claude-code-ide-external-prompt--ensure-editable t)

  ;; Fix "Cannot make side window the only window" error
  (advice-add 'delete-other-windows :around
              #'claude-code-ide-config--delete-other-windows-advice)

  ;; Add tile menu to Navigation group in transient menu
  (transient-append-suffix 'claude-code-ide-menu '(1 1 -1)
    '("T" "Tile sessions" claude-code-ide-tile-menu)))

(provide 'claude-code-ide-config)
;;; claude-code-ide-config.el ends here
