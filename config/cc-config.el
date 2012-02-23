(require 'hideshow)

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-semantic)
            (local-set-key (kbd "C-c c") 'compile)
            (c-set-offset 'innamespace 0)
            (c-set-offset 'arglist-close 0)
            (hs-minor-mode t)))
