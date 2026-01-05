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
  "Find project root with .tool-versions or .mise.toml."
  (or (locate-dominating-file default-directory ".tool-versions")
      (locate-dominating-file default-directory ".mise.toml")))

(defun mise-env--get-env (dir)
  "Get mise environment for DIR as alist."
  (let ((cached (gethash dir mise-env--project-cache)))
    (if cached
        cached
      (let ((result (mise-env--fetch-env dir)))
        (when result
          (puthash dir result mise-env--project-cache))
        result))))

(defun mise-env--fetch-env (dir)
  "Fetch mise environment for DIR."
  (when (and dir (executable-find mise-env-executable))
    (let ((default-directory dir))
      (with-temp-buffer
        (when (zerop (call-process mise-env-executable nil t nil "env" "--json"))
          (goto-char (point-min))
          (condition-case nil
              (json-read)
            (error nil)))))))

(defun mise-env--apply-env (env)
  "Apply mise ENV to current environment."
  (when env
    (let ((path (cdr (assq 'PATH env))))
      ;; Update process-environment
      (dolist (pair env)
        (let ((name (symbol-name (car pair)))
              (value (cdr pair)))
          (setenv name value)))
      ;; Update exec-path from PATH
      (when path
        (setq exec-path (parse-colon-path path))))))

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
