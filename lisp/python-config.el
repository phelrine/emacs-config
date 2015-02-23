(when (require 'flymake-python-pyflakes nil t)
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

(require 'ipython nil t)
(use-package jedi :ensure
  :config
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)))
;; (use-package anaconda-mode :ensure
;;   :config
;;   (progn
;;     (add-hook 'python-mode-hook 'anaconda-mode)
;;     (add-to-list 'company-backends 'company-anaconda)))
