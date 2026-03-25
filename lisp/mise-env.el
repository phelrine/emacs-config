;;; mise-env.el --- Automatic mise environment integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatically load mise environment based on project's .tool-versions or .mise.toml.
;; No need for .envrc files or direnv - works directly with mise.

;;; Code:

(require 'json)

(defgroup mise-env nil
  "Mise environment integration."
  :group 'environment)

(defcustom mise-env-executable "mise"
  "Path to mise executable."
  :type 'string
  :group 'mise-env)

(defvar-local mise-env--cache nil
  "Cached mise environment for current buffer.")

(defvar mise-env--project-cache (make-hash-table :test 'equal)
  "Cache of mise environments per project root.")

(defun mise-env--project-root ()
  "Find nearest project root with .tool-versions or .mise.toml.
When both exist in different ancestor directories, return the closest one."
  (let ((tv (locate-dominating-file default-directory ".tool-versions"))
        (mt (locate-dominating-file default-directory ".mise.toml")))
    (cond
     ((and tv mt)
      (if (>= (length (expand-file-name mt))
              (length (expand-file-name tv)))
          mt tv))
     (tv tv)
     (mt mt))))

(defun mise-env--get-env (dir)
  "Get mise environment for DIR as alist."
  (let ((cached (gethash dir mise-env--project-cache)))
    (if cached
        cached
      (let ((result (mise-env--fetch-env dir)))
        (when result
          (puthash dir result mise-env--project-cache))
        result))))

(defun mise-env--clean-path ()
  "Return PATH with mise install directories removed.
This prevents a previously loaded project's tool paths from
contaminating `mise env --json' output for a different project."
  (let ((mise-installs (expand-file-name "~/.local/share/mise/installs/")))
    (string-join
     (seq-remove (lambda (p) (string-prefix-p mise-installs p))
                 (parse-colon-path (getenv "PATH")))
     ":")))

(defun mise-env--fetch-env (dir)
  "Fetch mise environment for DIR."
  (when (and dir (executable-find mise-env-executable))
    (let* ((default-directory dir)
           (process-environment
            (cons (concat "PATH=" (mise-env--clean-path))
                  (seq-remove (lambda (e) (string-prefix-p "PATH=" e))
                              process-environment))))
      (with-temp-buffer
        (when (zerop (call-process mise-env-executable nil t nil "env" "--json"))
          (goto-char (point-min))
          (condition-case nil
              (json-read)
            (error nil)))))))

(defun mise-env--prioritize-path (path)
  "Reorder PATH so mise install directories come first.
`mise env' appends install paths after existing PATH entries when
called outside an activated shell.  This ensures they are prepended
so that mise-managed tools take priority over system ones."
  (let (mise-entries other-entries)
    (dolist (entry (parse-colon-path path))
      (if (string-prefix-p (expand-file-name "~/.local/share/mise/installs/") entry)
          (push entry mise-entries)
        (push entry other-entries)))
    (string-join (append (nreverse mise-entries) (nreverse other-entries)) ":")))

(defun mise-env--apply-env (env)
  "Apply mise ENV as buffer-local environment.
Each buffer gets its own `process-environment' and `exec-path',
so different projects can use different tool versions simultaneously."
  (when env
    (let* ((path (cdr (assq 'PATH env)))
           (path (when path (mise-env--prioritize-path path)))
           (new-env (copy-sequence process-environment)))
      (dolist (pair env)
        (let* ((name (symbol-name (car pair)))
               (value (if (string= name "PATH") path (cdr pair)))
               (prefix (concat name "=")))
          (setq new-env (cons (concat prefix value)
                              (seq-remove (lambda (e) (string-prefix-p prefix e))
                                          new-env)))))
      (setq-local process-environment new-env)
      (when path
        (setq-local exec-path (parse-colon-path path))))))

(defun mise-env-update ()
  "Update current buffer's environment from mise."
  (interactive)
  (let* ((root (mise-env--project-root))
         (env (when root (mise-env--get-env root))))
    (when env
      (setq mise-env--cache env)
      (mise-env--apply-env env)
      (message "mise-env: loaded environment from %s" root))))

(defun mise-env-clear-cache ()
  "Clear mise environment cache."
  (interactive)
  (clrhash mise-env--project-cache)
  (message "mise-env: cache cleared"))

(defun mise-env--maybe-update ()
  "Update mise environment if in a mise project."
  (when (mise-env--project-root)
    (mise-env-update)))

;;;###autoload
(define-minor-mode mise-env-mode
  "Minor mode for automatic mise environment loading."
  :lighter " mise"
  :global nil
  (if mise-env-mode
      (mise-env--maybe-update)
    (setq mise-env--cache nil)))

;;;###autoload
(define-globalized-minor-mode global-mise-env-mode
  mise-env-mode
  (lambda ()
    (when (and buffer-file-name
               (mise-env--project-root))
      (mise-env-mode 1))))

(provide 'mise-env)
;;; mise-env.el ends here
