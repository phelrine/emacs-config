(when (require 'flymake-python-pyflakes nil t)
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

(require 'ipython nil t)
(use-package anaconda-mode
  :config (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :config (add-to-list 'company-backends 'company-anaconda))
