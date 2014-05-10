(require 'csharp-mode nil t)
(require 'flymake nil t)
(require 'omnisharp-autoloads nil t)
(require 'nlinum-autoloads nil t)

(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(add-hook 'c++-mode-hook 'flymake-mode)

(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(add-hook 'c-mode-common-hook
          (lambda ()
            (nlinum-mode t)
            (eldoc-mode t)
            (local-set-key (kbd "C-c c") 'compile)
            (c-set-offset 'arglist-close 0)))

;; C#モードフック
(add-hook 'csharp-mode-hook
          '(lambda()
             (setq comment-column 40)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)
             (c-set-offset 'substatement-open 0)
             (c-set-offset 'case-label '+)
             (c-set-offset 'arglist-intro '+)
             (c-set-offset 'arglist-close 0)
             (omnisharp-mode t)
             ))
