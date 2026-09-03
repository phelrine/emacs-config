;;; claude-code-ide-config-test.el --- Tests for claude-code-ide-config -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs --batch -L lisp $(for d in straight/build/*/; do printf -- "-L %s " "$d"; done) \
;;     -l ert -l claude-code-ide-config \
;;     -l lisp/test/claude-code-ide-config-test.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Sections follow `claude-code-ide-config.el': terminal keys, the
;; external-editor prompt handoff, then the per-repository environment.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'claude-code-ide-config)

;;; Helpers

(defmacro claude-code-ide-config-test--with-session (&rest body)
  "Run BODY with a stub session's terminal buffer current.
Everything the terminal would receive is captured instead of sent.
Bound for BODY:
  `session'     a `claude-code-ide-mcp-session'
  `term-buffer' its terminal buffer
  `sent'        what reached the terminal, newest first, `return' marking a RET
  `keys'        (KEY MODS) pairs handed to the backend, newest first"
  (declare (indent 0))
  `(let* ((term-buffer (generate-new-buffer "*claude-code[test]*"))
          (session (make-claude-code-ide-mcp-session :buffer term-buffer))
          (claude-code-ide-terminal-backend 'ghostel)
          (sent nil)
          (keys nil))
     (cl-letf (((symbol-function 'claude-code-ide--terminal-send-string)
                (lambda (string) (push string sent)))
               ((symbol-function 'claude-code-ide--terminal-send-return)
                (lambda () (push 'return sent)))
               ((symbol-function 'ghostel-send-key)
                (lambda (key &optional mods) (push (list key mods) keys))))
       (unwind-protect
           (progn
             ;; Anaphoric: not every test reads every binding.
             (ignore session term-buffer sent keys)
             (with-current-buffer term-buffer ,@body))
         (when (buffer-live-p term-buffer) (kill-buffer term-buffer))))))

(defmacro claude-code-ide-config-test--with-server-handshake (&rest body)
  "Run BODY with the `server-edit' handshake stubbed.
Binds `released' non-nil once the client has been handed back, and runs
scheduled timers inline so the delayed RET needs no waiting."
  (declare (indent 0))
  `(let (released)
     (cl-letf (((symbol-function 'server-edit) (lambda (&rest _) (setq released t)))
               ((symbol-function 'run-at-time)
                (lambda (_time _repeat fn &rest args) (apply fn args))))
       (ignore released)
       ,@body)))

(defmacro claude-code-ide-config-test--with-stub-mise (&rest body)
  "Run BODY with `mise-env-update' stubbed to add marker entries.
Binds `mise-dir' to the `default-directory' the stub observed."
  (declare (indent 0))
  `(let ((mise-dir nil))
     (cl-letf (((symbol-function 'mise-env-update)
                (lambda ()
                  (setq mise-dir default-directory)
                  (setq-local process-environment
                              (cons "CLAUDE_CONFIG_DIR=/stub/config" process-environment))
                  (setq-local exec-path (cons "/stub/bin" exec-path)))))
       (ignore mise-dir)
       ,@body)))

(defmacro claude-code-ide-config-test--with-only-executable (path-form &rest body)
  "Run BODY with PATH-FORM's value, bound to `only', as the sole executable.
`executable-find' keeps answering, so BODY can tell resolution order
from mere availability."
  (declare (indent 1))
  `(let ((only ,path-form))
     (cl-letf (((symbol-function 'file-executable-p) (lambda (f) (equal f only)))
               ((symbol-function 'file-directory-p) (lambda (_) nil))
               ((symbol-function 'executable-find)
                (lambda (&rest _) "/opt/homebrew/bin/emacsclient")))
       ,@body)))

;;; Terminal Keys

(ert-deftest claude-code-ide-config-test-send-ctrl-reaches-backend ()
  "Control keys reach the terminal through the active backend."
  (claude-code-ide-config-test--with-session
    (claude-code-ide-config--send-ctrl "g")
    (should (equal (reverse keys) '(("g" "ctrl"))))))

(ert-deftest claude-code-ide-config-test-send-c-o-sends-control-o ()
  "The verbose-toggle binding keeps sending C-o after the refactor."
  (claude-code-ide-config-test--with-session
    (claude-code-ide-send-c-o)
    (should (equal (reverse keys) '(("o" "ctrl"))))))

;;; External Editor Input

;; The CLI's C-g writes its input line to a temp file, runs $EDITOR on
;; it, and reads the file back once the editor exits.

(ert-deftest claude-code-ide-config-test-external-presses-ctrl-g ()
  "The external style hands composing to the CLI rather than opening a buffer."
  (claude-code-ide-config-test--with-session
    (claude-code-ide-send-prompt-externally #'ignore nil session)
    (should (equal (reverse keys) '(("g" "ctrl"))))
    (should (eq claude-code-ide-external-prompt--pending-session session))))

(ert-deftest claude-code-ide-config-test-external-delegates-programmatic ()
  "A programmatic prompt never goes through the CLI's editor key."
  (claude-code-ide-config-test--with-session
    (let (received)
      (claude-code-ide-send-prompt-externally
       (lambda (&rest args) (setq received args)) "hello" session)
      (should (equal received (list "hello" session)))
      (should-not keys))))

(ert-deftest claude-code-ide-config-test-external-mode-claims-session ()
  "The temp file Emacs is handed adopts the session that asked for it,
so finishing knows which terminal to press RET in."
  (let ((claude-code-ide-external-prompt--pending-session 'the-session))
    (with-temp-buffer
      (claude-code-ide-external-prompt-mode)
      (should (eq claude-code-ide-external-prompt--session 'the-session))
      ;; Cleared, so the next unrelated file does not inherit it.
      (should-not claude-code-ide-external-prompt--pending-session))))

(ert-deftest claude-code-ide-config-test-external-mode-turns-skk-on ()
  "The handed-over buffer is ready for Japanese input too."
  (let (skk-arg)
    (cl-letf (((symbol-function 'skk-mode) (lambda (&optional arg) (setq skk-arg arg))))
      (with-temp-buffer
        (claude-code-ide-external-prompt-mode)
        (should (equal skk-arg 1))))))

(ert-deftest claude-code-ide-config-test-external-file-is-editable ()
  "The prompt file escapes the global read-only-by-default rule.
It exists to be typed into, like a commit message buffer."
  (with-temp-buffer
    (claude-code-ide-external-prompt-mode)
    (read-only-mode 1)
    (claude-code-ide-external-prompt--ensure-editable)
    (should-not buffer-read-only)))

(ert-deftest claude-code-ide-config-test-external-leaves-other-files-alone ()
  "Ordinary files keep whatever the global rule decided for them."
  (with-temp-buffer
    (text-mode)
    (read-only-mode 1)
    (claude-code-ide-external-prompt--ensure-editable)
    (should buffer-read-only)))

(ert-deftest claude-code-ide-config-test-external-mode-shows-key-help ()
  "The buffer says how to get out of it: there is no other affordance
telling you that C-c C-c is what sends."
  (with-temp-buffer
    (claude-code-ide-external-prompt-mode)
    (should (string-match-p "C-c C-c" header-line-format))
    (should (string-match-p "C-c C-t" header-line-format))))

(ert-deftest claude-code-ide-config-test-external-posframe-keeps-header-line ()
  "Ask posframe to keep the header and mode lines.
It drops both by default, which would hide the key help and SKK's input
mode -- the only two status readouts the child frame has."
  (let (args)
    (cl-letf (((symbol-function 'posframe-show)
               (lambda (_buffer &rest rest) (setq args rest) nil))
              ((symbol-function 'posframe--find-existing-posframe) #'ignore))
      (with-temp-buffer
        (claude-code-ide-external-prompt--show-posframe (current-buffer))))
    (should (plist-get args :respect-header-line))
    (should (plist-get args :respect-mode-line))))

(ert-deftest claude-code-ide-config-test-external-posframe-shows-cursor ()
  "Ask posframe for a cursor, and put it after the draft.
It hides the cursor and pins the window to position 0 by default, which
leaves you typing Japanese with nothing to aim at and the caret behind
whatever the CLI already had."
  (let (args)
    (cl-letf (((symbol-function 'posframe-show)
               (lambda (_buffer &rest rest) (setq args rest) nil))
              ((symbol-function 'posframe--find-existing-posframe) #'ignore))
      (with-temp-buffer
        (insert "CLI側にあった下書き")
        (claude-code-ide-external-prompt--show-posframe (current-buffer))
        (should (plist-get args :cursor))
        (should (equal (plist-get args :window-point) (point-max)))))))

(ert-deftest claude-code-ide-config-test-external-mode-leaves-server-window-alone ()
  "Entering the mode must not touch global server state.
A prompt file visited outside a handoff -- from `recentf', say -- would
otherwise leave every later `emacsclient' file displaying our way."
  (let ((server-window 'previous-value))
    (with-temp-buffer
      (claude-code-ide-external-prompt-mode)
      (should (eq server-window 'previous-value)))))

(ert-deftest claude-code-ide-config-test-external-switch-buffer-takes-prompt-buffers ()
  "The `server-switch-buffer' advice displays prompt files itself."
  (let (shown orig-called)
    (cl-letf (((symbol-function 'claude-code-ide-external-prompt--display)
               (lambda (buffer) (setq shown buffer))))
      (with-temp-buffer
        (claude-code-ide-external-prompt-mode)
        (claude-code-ide-external-prompt--switch-buffer
         (lambda (&rest _) (setq orig-called t))
         (current-buffer) nil '(3 . 4) nil)
        (should (eq shown (current-buffer)))
        (should-not orig-called)))))

(ert-deftest claude-code-ide-config-test-external-switch-buffer-leaves-others-alone ()
  "Any other buffer, or none, goes to the stock implementation unchanged."
  (let (shown seen)
    (cl-letf (((symbol-function 'claude-code-ide-external-prompt--display)
               (lambda (buffer) (setq shown buffer))))
      (with-temp-buffer
        (text-mode)
        (claude-code-ide-external-prompt--switch-buffer
         (lambda (&rest args) (setq seen args))
         (current-buffer) nil '(3 . 4) nil)
        (should (equal seen (list (current-buffer) nil '(3 . 4) nil)))
        (should-not shown))
      (claude-code-ide-external-prompt--switch-buffer
       (lambda (&rest args) (setq seen (or args 'called))))
      (should (null (car-safe seen)))
      (should-not shown))))

(ert-deftest claude-code-ide-config-test-external-display-honours-style ()
  "`claude-code-ide-config-external-prompt-display' picks the presentation."
  (let (shown)
    (cl-letf (((symbol-function 'claude-code-ide-external-prompt--show-posframe)
               (lambda (&rest _) (setq shown 'posframe)))
              ((symbol-function 'claude-code-ide-external-prompt--show-window)
               (lambda (&rest _) (setq shown 'window))))
      (with-temp-buffer
        (let ((claude-code-ide-config-external-prompt-display 'posframe)
              (server-window nil))
          (claude-code-ide-external-prompt--display (current-buffer))
          (should (eq shown 'posframe)))
        (let ((claude-code-ide-config-external-prompt-display 'window)
              (server-window nil))
          (claude-code-ide-external-prompt--display (current-buffer))
          (should (eq shown 'window)))))))

(ert-deftest claude-code-ide-config-test-external-finish-hides-before-releasing ()
  "Take the posframe down before handing back, so the CLI is never left
redrawing underneath a stale child frame."
  (let (order)
    (cl-letf (((symbol-function 'claude-code-ide-external-prompt--hide)
               (lambda (&rest _) (push 'hide order)))
              ((symbol-function 'server-edit) (lambda (&rest _) (push 'release order))))
      (with-temp-buffer
        (claude-code-ide-external-prompt-finish)))
    (should (equal (reverse order) '(hide release)))))

(ert-deftest claude-code-ide-config-test-external-finish-saves-before-releasing ()
  "The CLI re-reads the file from disk once emacsclient exits, so an
unsaved buffer would hand back the text the user just replaced."
  (let (order)
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) (push 'save order)))
              ((symbol-function 'server-edit) (lambda (&rest _) (push 'release order))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/claude-501/claude-prompt-x.md")
        (insert "edited")
        (set-buffer-modified-p t)
        (claude-code-ide-external-prompt-finish)
        (set-buffer-modified-p nil)))
    (should (equal (reverse order) '(save release)))))

(ert-deftest claude-code-ide-config-test-external-finish-sends-no-return ()
  "Plain finish leaves the text in the CLI's input line, unsubmitted."
  (claude-code-ide-config-test--with-session
    (claude-code-ide-config-test--with-server-handshake
      (with-temp-buffer
        (setq claude-code-ide-external-prompt--session session)
        (claude-code-ide-external-prompt-finish))
      (should released))
    (should-not sent)))

(ert-deftest claude-code-ide-config-test-external-finish-and-send-presses-return ()
  "Finish-and-send submits by pressing RET in the session's terminal."
  (claude-code-ide-config-test--with-session
    (claude-code-ide-config-test--with-server-handshake
      (with-temp-buffer
        (setq claude-code-ide-external-prompt--session session)
        (claude-code-ide-external-prompt-finish-and-send)))
    (should (equal sent '(return)))))

(ert-deftest claude-code-ide-config-test-external-finish-survives-dead-session ()
  "Finishing still works when the session died while the file was open."
  (claude-code-ide-config-test--with-session
    (claude-code-ide-config-test--with-server-handshake
      (kill-buffer term-buffer)
      (with-temp-buffer
        (setq claude-code-ide-external-prompt--session session)
        (claude-code-ide-external-prompt-finish-and-send))
      (should released))
    (should-not sent)))

;;; Editor Resolution

(ert-deftest claude-code-ide-config-test-editor-prefers-bundled-client ()
  "Resolve `emacsclient' without consulting `exec-path'.
A session started early in Emacs' startup runs before
`exec-path-from-shell', when a PATH lookup finds nothing and the
override would be skipped without a word."
  (claude-code-ide-config-test--with-only-executable
      (expand-file-name "bin/emacsclient" invocation-directory)
    (should (equal (claude-code-ide-config--editor-command) only))))

(ert-deftest claude-code-ide-config-test-editor-accepts-sibling-client ()
  "A client sitting beside the Emacs binary counts too."
  (claude-code-ide-config-test--with-only-executable
      (expand-file-name "emacsclient" invocation-directory)
    (should (equal (claude-code-ide-config--editor-command) only))))

(ert-deftest claude-code-ide-config-test-editor-falls-back-to-path ()
  "With nothing shipped alongside, a PATH lookup still counts."
  (claude-code-ide-config-test--with-only-executable "/opt/homebrew/bin/emacsclient"
    (should (equal (claude-code-ide-config--editor-command) only))))

(ert-deftest claude-code-ide-config-test-editor-nil-when-absent ()
  "No client anywhere means no EDITOR override."
  (cl-letf (((symbol-function 'file-executable-p) (lambda (_) nil))
            ((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (should-not (claude-code-ide-config--editor-command))))

(defmacro claude-code-ide-config-test--with-saved-global-env (&rest body)
  "Run BODY, restoring the global `process-environment' afterwards."
  (declare (indent 0))
  `(let ((saved (default-value 'process-environment)))
     (unwind-protect (progn ,@body)
       (setq-default process-environment saved))))

(ert-deftest claude-code-ide-config-test-export-editor-sets-global-environment ()
  "Point EDITOR at this Emacs for everything Emacs spawns."
  (claude-code-ide-config-test--with-saved-global-env
    (cl-letf (((symbol-function 'claude-code-ide-config--editor-command)
               (lambda () "/usr/local/bin/emacsclient")))
      (claude-code-ide-config--export-editor)
      (should (equal (getenv-internal "EDITOR" (default-value 'process-environment))
                     "/usr/local/bin/emacsclient"))
      (should (equal (getenv-internal "VISUAL" (default-value 'process-environment))
                     "/usr/local/bin/emacsclient")))))

(ert-deftest claude-code-ide-config-test-export-editor-ignores-buffer-local-env ()
  "Write the global value even when the current buffer has its own.
`mise-env' gives every prog-mode buffer a buffer-local
`process-environment', and the terminal spawns from a different buffer
again -- a plain `setenv' here would be discarded before the CLI ever
started."
  (claude-code-ide-config-test--with-saved-global-env
    (cl-letf (((symbol-function 'claude-code-ide-config--editor-command)
               (lambda () "/usr/local/bin/emacsclient")))
      (with-temp-buffer
        (setq-local process-environment (cons "MARKER=local" process-environment))
        (claude-code-ide-config--export-editor))
      (should (equal (getenv-internal "EDITOR" (default-value 'process-environment))
                     "/usr/local/bin/emacsclient")))))

(ert-deftest claude-code-ide-config-test-export-editor-replaces-stale-value ()
  "Exporting twice leaves one EDITOR entry, not a growing stack."
  (claude-code-ide-config-test--with-saved-global-env
    (cl-letf (((symbol-function 'claude-code-ide-config--editor-command)
               (lambda () "/usr/local/bin/emacsclient")))
      (claude-code-ide-config--export-editor)
      (claude-code-ide-config--export-editor)
      (should (= 1 (seq-count (lambda (e) (string-prefix-p "EDITOR=" e))
                              (default-value 'process-environment)))))))

(ert-deftest claude-code-ide-config-test-export-editor-noop-without-client ()
  "With no client to point at, leave the inherited EDITOR alone."
  (claude-code-ide-config-test--with-saved-global-env
    (setq-default process-environment
                  (cons "EDITOR=nano" (default-value 'process-environment)))
    (cl-letf (((symbol-function 'claude-code-ide-config--editor-command) #'ignore))
      (claude-code-ide-config--export-editor)
      (should (equal (getenv-internal "EDITOR" (default-value 'process-environment))
                     "nano")))))

;;; Per-Repository Environment

(ert-deftest claude-code-ide-config-test-project-env-runs-mise-in-dir ()
  "`--project-env' runs mise with `default-directory' set to DIR."
  (claude-code-ide-config-test--with-stub-mise
    (claude-code-ide-config--project-env "/tmp/some-project/")
    (should (equal mise-dir "/tmp/some-project/"))))

(ert-deftest claude-code-ide-config-test-project-env-returns-env-and-path ()
  "`--project-env' returns the environment mise produced."
  (claude-code-ide-config-test--with-stub-mise
    (let ((env (claude-code-ide-config--project-env "/tmp/some-project/")))
      (should (member "CLAUDE_CONFIG_DIR=/stub/config" (car env)))
      (should (member "/stub/bin" (cdr env))))))

(ert-deftest claude-code-ide-config-test-with-project-env-applies-env ()
  "The wrapped function sees the project's environment."
  (claude-code-ide-config-test--with-stub-mise
    (let (observed-env observed-path)
      (claude-code-ide-config--with-project-env
       (lambda (&rest _)
         (setq observed-env process-environment
               observed-path exec-path))
       "*buf*" "/tmp/some-project/" 1234 nil nil "sid")
      (should (member "CLAUDE_CONFIG_DIR=/stub/config" observed-env))
      (should (member "/stub/bin" observed-path)))))

(ert-deftest claude-code-ide-config-test-with-project-env-passes-args-through ()
  "All arguments reach the wrapped function unchanged."
  (claude-code-ide-config-test--with-stub-mise
    (let (received)
      (claude-code-ide-config--with-project-env
       (lambda (&rest args) (setq received args) 'return-value)
       "*buf*" "/tmp/some-project/" 1234 t nil "sid")
      (should (equal received '("*buf*" "/tmp/some-project/" 1234 t nil "sid"))))))

(ert-deftest claude-code-ide-config-test-with-project-env-returns-orig-value ()
  "The wrapper is transparent to the wrapped function's return value."
  (claude-code-ide-config-test--with-stub-mise
    (should (eq (claude-code-ide-config--with-project-env
                 (lambda (&rest _) 'return-value)
                 "*buf*" "/tmp/some-project/" 1234 nil nil "sid")
                'return-value))))

(ert-deftest claude-code-ide-config-test-with-project-env-does-not-leak ()
  "The project environment does not outlive the call."
  (claude-code-ide-config-test--with-stub-mise
    (let ((env-before process-environment)
          (path-before exec-path))
      (claude-code-ide-config--with-project-env
       #'ignore "*buf*" "/tmp/some-project/" 1234 nil nil "sid")
      (should (equal process-environment env-before))
      (should (equal exec-path path-before)))))

(ert-deftest claude-code-ide-config-test-with-project-env-survives-mise-failure ()
  "A failing mise lookup falls back to the ambient environment."
  (cl-letf (((symbol-function 'mise-env-update)
             (lambda () (error "mise exploded")))
            )
    (let (observed)
      (claude-code-ide-config--with-project-env
       (lambda (&rest _) (setq observed process-environment))
       "*buf*" "/tmp/some-project/" 1234 nil nil "sid")
      (should (equal observed process-environment)))))

(provide 'claude-code-ide-config-test)
;;; claude-code-ide-config-test.el ends here
