;;; mise-env.el --- Automatic mise environment integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatically load mise environment based on project's .tool-versions or .mise.toml.
;; Sets buffer-local `process-environment' and `exec-path' so different projects
;; can use different tool versions simultaneously.
;;
;; Usage: (add-hook 'prog-mode-hook #'mise-env-update)

;;; Code:

(require 'json)
(require 'seq)

(defgroup mise-env nil
  "Mise environment integration."
  :group 'environment)

(defcustom mise-env-executable "mise"
  "Path to mise executable."
  :type 'string
  :group 'mise-env)

(defvar mise-env--project-cache (make-hash-table :test 'equal)
  "Cache of mise environments per project root.")

(defconst mise-env--installs-prefix
  (expand-file-name "~/.local/share/mise/installs/")
  "Path prefix for mise tool installations.")

(defun mise-env--project-root ()
  "Find nearest project root with .tool-versions or .mise.toml."
  (let ((tv (locate-dominating-file default-directory ".tool-versions"))
        (mt (locate-dominating-file default-directory ".mise.toml")))
    (cond
     ((and tv mt)
      (if (>= (length (expand-file-name mt))
              (length (expand-file-name tv)))
          mt tv))
     (tv tv)
     (mt mt))))

(defun mise-env--fetch-env (dir)
  "Fetch mise environment for DIR by running `mise env --json'.
Uses a clean PATH without mise install dirs to avoid contamination
from previously loaded projects."
  (let* ((default-directory dir)
         (process-environment
          (cons (concat "PATH="
                        (string-join
                         (seq-remove (lambda (p) (string-prefix-p mise-env--installs-prefix p))
                                     (parse-colon-path (getenv "PATH")))
                         ":"))
                (seq-remove (lambda (e) (string-prefix-p "PATH=" e))
                            process-environment))))
    (with-temp-buffer
      (when (zerop (call-process mise-env-executable nil t nil "env" "--json"))
        (goto-char (point-min))
        (condition-case nil
            (json-read)
          (error nil))))))

(defun mise-env--get-env (dir)
  "Get mise environment for DIR, using cache when available."
  (or (gethash dir mise-env--project-cache)
      (when-let ((result (mise-env--fetch-env dir)))
        (puthash dir result mise-env--project-cache)
        result)))

(defun mise-env--prioritize-path (path)
  "Reorder PATH so mise install directories come first."
  (let (mise-entries other-entries)
    (dolist (entry (parse-colon-path path))
      (if (string-prefix-p mise-env--installs-prefix entry)
          (push entry mise-entries)
        (push entry other-entries)))
    (string-join (append (nreverse mise-entries) (nreverse other-entries)) ":")))

(defun mise-env--apply-env (env)
  "Apply mise ENV as buffer-local `process-environment' and `exec-path'."
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
      (setq-local exec-path (parse-colon-path path)))))

;;;###autoload
(defun mise-env-update ()
  "Update current buffer's environment from mise."
  (interactive)
  (when-let ((root (mise-env--project-root))
             (env (mise-env--get-env root)))
    (mise-env--apply-env env)))

(defun mise-env-clear-cache ()
  "Clear mise environment cache."
  (interactive)
  (clrhash mise-env--project-cache)
  (message "mise-env: cache cleared"))

(provide 'mise-env)
;;; mise-env.el ends here
