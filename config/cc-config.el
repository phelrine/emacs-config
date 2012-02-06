(require 'hideshow)

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-semantic)
            (local-set-key (kbd "C-c c") 'compile)
            (hs-minor-mode t)))
