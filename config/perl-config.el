(defalias 'perl-mode 'cperl-mode)
(use-package cperl-mode
  :config
  (progn
    (setq cperl-indent-level 4
          cperl-close-paren-offset -4
          cperl-continued-statement-offset 4
          cperl-indent-parens-as-block t
          cperl-tab-always-indent t)
    (add-hook 'cperl-mode-hook '(lambda () (setq indent-tabs-mode nil)))))
