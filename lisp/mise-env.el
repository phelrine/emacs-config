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

(defconst mise-env--config-files
  '("mise.toml" "mise.local.toml" ".mise.toml" ".mise.local.toml"
    ".config/mise.toml" ".tool-versions")
  "File names that mark a directory as mise-configured.
Mirrors the set mise itself reads.  The `.local' variants are the
ones to use for machine-local settings in a shared repository, since
they are conventionally gitignored.")

(defun mise-env--project-root ()
  "Find the nearest ancestor directory holding a mise config file.
When several config files match at different depths, the deepest one
wins — that is the one mise itself gives precedence to."
  (car (sort (delq nil (mapcar (lambda (name)
                                 (locate-dominating-file default-directory name))
                               mise-env--config-files))
             (lambda (a b) (> (length (expand-file-name a))
                              (length (expand-file-name b)))))))

(defun mise-env--fetch-env (dir)
  "Fetch mise environment for DIR by running `mise env --json'.
Uses a clean PATH without mise install dirs to avoid contamination
from previously loaded projects.  Stderr is discarded: mise reports
things like missing tools there, and mixing that into the parse
buffer would break `json-read' and lose the whole environment."
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
      (when (zerop (call-process mise-env-executable nil '(t nil) nil "env" "--json"))
        (goto-char (point-min))
        (condition-case nil
            (json-read)
          (error nil))))))

(defconst mise-env--global-config-files
  '("~/.config/mise/config.toml" "~/.config/mise/config.local.toml")
  "Global mise config files, which live outside any project's ancestry.")

(defun mise-env--config-signature (dir)
  "Return the config files mise would read for DIR, with their mtimes.
mise merges every config file from DIR up to the filesystem root plus
the global one, so a file added or edited anywhere along that chain
changes DIR's environment -- not just one in DIR itself."
  (let ((files (mapcar #'expand-file-name mise-env--global-config-files))
        (current (file-name-as-directory (expand-file-name dir)))
        parent)
    (while current
      (dolist (name mise-env--config-files)
        (push (expand-file-name name current) files))
      (setq parent (file-name-directory (directory-file-name current)))
      (setq current (unless (equal parent current) parent)))
    (delq nil (mapcar (lambda (file)
                        (when-let* ((attrs (file-attributes file)))
                          (cons file (file-attribute-modification-time attrs))))
                      files))))

(defun mise-env--get-env (dir)
  "Get mise environment for DIR, using cache when available.
A cached result is reused only while the config files behind it are
unchanged; otherwise a config edited or added after Emacs first looked
would go unnoticed until the cache was cleared by hand."
  (let ((signature (mise-env--config-signature dir))
        (cached (gethash dir mise-env--project-cache)))
    (if (and cached (equal (car cached) signature))
        (cdr cached)
      (when-let* ((result (mise-env--fetch-env dir)))
        (puthash dir (cons signature result) mise-env--project-cache)
        result))))

(defun mise-env--prioritize-path (path)
  "Reorder PATH so mise install directories come first."
  (let (mise-entries other-entries)
    (dolist (entry (parse-colon-path path))
      (if (string-prefix-p mise-env--installs-prefix entry)
          (push entry mise-entries)
        (push entry other-entries)))
    (string-join (append (nreverse mise-entries) (nreverse other-entries)) ":")))

(defun mise-env--apply-env (env)
  "Apply mise ENV as buffer-local `process-environment' and `exec-path'.
Builds on the global environment, not the buffer's, so re-applying
after a version change drops whatever the previous env set."
  (let* ((path (cdr (assq 'PATH env)))
         (path (when path (mise-env--prioritize-path path)))
         (new-env (copy-sequence (default-value 'process-environment))))
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
(defun mise-env-update (&rest _)
  "Update current buffer's environment from mise.
Ignores its arguments so it can serve as :before advice."
  (interactive)
  (when-let* ((root (mise-env--project-root))
             (env (mise-env--get-env root)))
    (mise-env--apply-env env)))

(defun mise-env-clear-cache ()
  "Clear mise environment cache."
  (interactive)
  (clrhash mise-env--project-cache)
  (message "mise-env: cache cleared"))

(provide 'mise-env)
;;; mise-env.el ends here
