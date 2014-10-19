(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

;; C
(use-package nlinum :config (add-hook 'c-mode-common-hook (nlinum-mode t)))
(use-package gtags :config (add-hook 'c-mode-common-hook (gtags-mode t)))
(use-package flycheck
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (flycheck-mode t)
              (flycheck-select-checker 'c/c++-cppcheck))))

(add-hook
 'c-mode-common-hook
 (lambda ()
   (eldoc-mode t)
   (local-set-key (kbd "C-c c") 'compile)
   (setq c-basic-offset 4
         indent-tabs-mode nil
         comment-column 40)
   (c-set-offset 'substatement-open 0)
   (c-set-offset 'case-label '+)
   (c-set-offset 'arglist-intro '+)
   (c-set-offset 'arglist-close 0)))

;; C++
(push '(".+\\.h$" . c++-mode) auto-mode-alist)

;; C#
(use-package csharp-mode
  :mode "\\.cs$"
  :config
  (progn
    (use-package omnisharp
      :config
      (add-hook 'csharp-mode-hook (omnisharp-mode t)))
    ))
