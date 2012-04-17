(require 'gtags)
(require 'anything-grep)

(defun gtags-key-config()
  (mapc (lambda (c) (local-set-key (read-kbd-macro (car c)) (cdr c)))
        '(("M-t" . gtags-find-tag)
          ("M-r" . gtags-find-rtag)
          ("M-s" . gtags-find-symbol)
          ("M-p" . gtags-pop-stack)
          )))

(add-hook 'gtags-mode-hook 'gtags-key-config)
(add-hook 'c-mode-common-hook 'gtags-mode)
