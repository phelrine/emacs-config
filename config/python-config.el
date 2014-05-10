(when (require 'flymake-python-pyflakes nil t)
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

(require 'ipython nil t)
