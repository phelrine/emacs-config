(when (require 'flymake nil t)
  (defun flymake-tex-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-dir   (file-name-directory buffer-file-name))
           (local-file  (file-relative-name
                         temp-file
                         local-dir)))
      (list "platex" (list "-file-line-error" "-interaction=nonstopmode" local-file))))

  (defun flymake-tex-cleanup-custom ()
    (let* ((base-file-name (file-name-sans-extension (file-name-nondirectory flymake-temp-source-file-name)))
           (regexp-base-file-name (concat "^" base-file-name "\\.")))
      (mapc '(lambda (filename)
               (when (string-match regexp-base-file-name filename)
                 (flymake-safe-delete-file filename)))
            (split-string (shell-command-to-string "ls"))))
    (setq flymake-last-change-time nil))

  (push '("\\.tex$" flymake-tex-init flymake-tex-cleanup-custom) flymake-allowed-file-name-masks))

(add-hook 'latex-mode-hook '(lambda () (reftex-mode)))
(if (require 'flyspell nil t) (add-hook 'latex-mode-hook '(lambda () (flyspell-mode))))

(setq auto-mode-alist (cons '("\\.tex\\'" . latex-mode) auto-mode-alist))
