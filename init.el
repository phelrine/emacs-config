(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setenv "LANG" "ja_JP.UTF-8")
(dolist (path '("~/.emacs.d/" "~/.emacs.d/config"))
  (add-to-list 'load-path path))

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(add-to-list 'exec-path "/home/phelrine/repos/rtags/bin")
(setenv "PATH" )
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package nil t)
(add-hook 'after-init-hook (lambda () (load "init-packages")))
