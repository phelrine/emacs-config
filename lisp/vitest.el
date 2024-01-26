;;; vitest.el --- Run vitest from Emacs -*- lexical-binding: t; -*-

;;; Commentary:

(require 'compile)
(require 'transient)

;;; Code:

(defun vitest-find-package-json-directory ()
  "Find the directory containing package.json starting from the current directory."
  (locate-dominating-file default-directory "package.json"))

(defun vitest-get-test-relative-path ()
  "Get the relative path from root directory."
  (if-let ((file (buffer-file-name))
           (project-root-directory (locate-dominating-file (file-name-directory file) "package.json")))
      (let ((relative-path (file-relative-name file project-root-directory)))
        (message relative-path))
    (error "No package.json found in any ancestor directories or current buffer is not associated with a file")))

(transient-define-prefix vitest-transient ()
  "Transient for running vitest."
  ["Arguments"
   ("-c" "Coverage mode" "--coverage")
   ("-u" "Update snapshot" "--update-snapshot")
   ("--config" "Set configuration file" "--config=" transient-read-file-name)
   ("--environment" "Set environment name" "--environment=" transient-read-string)]
  ["Filters"
   (vitest-transient-test-name-pattern-option)]
  ["Actions"
   ("a" "vitest all" vitest-run)
   ("f" "vitest current file" vitest-run-current-file)])

(transient-define-infix vitest-transient-test-name-pattern-option ()
  :description "Test name pattern"
  :class 'transient-option
  :shortarg "-t"
  :argument "--testNamePattern=")

(defun vitest-execute-command (args)
  "Execute vitest command with ARGS."
  (let ((default-directory (vitest-find-package-json-directory)))
    (unless default-directory
      (error "No package.json found in the current directory or its parents"))
    (let ((full-command (concat "npx vitest run " (string-join args " "))))
      (message full-command)
      (compile full-command))))

(defun vitest-run (args)
  "Run vitest with the specified ARGS."
  (interactive (list (transient-args 'vitest-transient)))
  (vitest-execute-command args))

(defun vitest-run-current-file (args)
  "Run Vitest for the current file with specified ARGS."
  (interactive (list (transient-args 'vitest-transient)))
  (vitest-execute-command (cons (vitest-get-test-relative-path) args)))

(with-eval-after-load 'compile
  (defvar vitest-error-regexp "^ ❯ \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$")
  (add-to-list 'compilation-error-regexp-alist-alist `(vitest ,vitest-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'vitest))

(provide 'vitest)

;;; vitest.el ends here
