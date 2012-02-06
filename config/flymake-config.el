(require 'flymake)
(require 'flymake-cursor)

(set-face-background 'flymake-errline "red")
(set-face-background 'flymake-warnline "yellow")

(setq flymake-err-line-patterns
      `(("\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)" 1 2 3 4) ; gcc 4.5
        ("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)          ; ruby
        ,@flymake-err-line-patterns))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes"  (list local-file))))

(push '("\\.py\\'" flymake-pyflakes-init) flymake-allowed-file-name-masks)

