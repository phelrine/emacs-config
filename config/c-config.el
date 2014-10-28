(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(defun flycheck-setting-c/c++()
  (flycheck-mode t)
  (flycheck-select-checker 'c/c++-cppcheck))

(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
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

;; Common
(use-package gtags :config (add-hook 'c-mode-common-hook #'gtags-mode))
(use-package dtrt-indent :ensure :config (add-hook 'c-mode-common-hook #'dtrt-indent-mode))
(use-package flycheck :ensure
  :config (add-hook 'c-mode-hook #'flycheck-setting-c/c++))

;; C++
(push '(".+\\.h$" . c++-mode) auto-mode-alist)
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

(use-package flycheck :ensure
  :config (add-hook 'c++-mode-hook #'flycheck-setting-c/c++)
(use-package irony :ensure :disabled
  :config (add-hook 'c++-mode-hook #'irony-mode))
(use-package company-irony :ensure :disabled
  :config
  (progn

    (add-to-list 'company-backends 'company-irony)
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))
(use-package rtags :ensure
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (rtags-enable-standard-keybindings c-mode-base-map)
              (setq rtags-completions-enabled t)
              (rtags-diagnostics))))
(use-package company-rtags :ensure rtags
  :config
  (progn
    (add-to-list 'company-backends 'company-rtags)
    (setq company-rtags-begin-after-member-access t)))
(use-package function-args :ensure :disabled :config (fa-config-default))
;; C#
(use-package csharp-mode :ensure :mode "\\.cs$")
(use-package omnisharp :ensure
  :config (add-hook 'csharp-mode-hook #'omnisharp-mode))
