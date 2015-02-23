(custom-set-variables
 '(global-auto-revert-mode t)
 '(show-paren-mode 1)
 '(make-backup-files nil)
 '(tab-width 4)
 '(indent-tabs-mode nil))

(custom-set-faces
 '(whitespace-space ((t (:foreground "DarkGoldenrod1"))))
 '(whitespace-tab ((t (:foreground "blue"))))
 '(flymake-errline ((t (:foreground "orange" :background "blue"))))
 '(flymake-warnline ((t (:background "yellow"))))
 )

(menu-bar-mode 1)
(tool-bar-mode -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'message-box 'message)
(when (eq (window-system) 'ns)
  (set-fontset-font "fontset-default" 'japanese-jisx0208
                    '("Hiragino_Kaku_Gothic_ProN" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'katakana-jisx0201
                    '("Hiragino_Kaku_Gothic_ProN" . "iso10646-1"))
  (nconc default-frame-alist '((width . 120)(height . 40))))
(when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))
(global-font-lock-mode)

(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

;; builtin
(use-package whitespace
  :init
  (progn
    (setq whitespace-style '(face tabs tab-mark spaces space-mark)
          whitespace-space-regexp "\\(\x3000+\\)"
          whitespace-display-mappings '((space-mark ?\x3000 [?\□])
                                        (tab-mark   ?\t   [?\xBB ?\t]))))
  :config (global-whitespace-mode 1))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package saveplace :config (setq-default save-place t))
(use-package savehist
  :config
  (progn
    (setq avehist-mode 1)
    (setq history-length 5000)))
(use-package recentf
  :config
  (progn (setq recentf-max-saved-items 1000)))
(use-package hl-line
  :config
  (progn
    (set-face-background 'hl-line "violet")
    (set-face-underline-p 'hl-line "blue")
    (global-hl-line-mode t)))

;; packages
(use-package helm-config :ensure helm
  :init
  (custom-set-variables
   '(helm-ff-skip-boring-files t)
   '(helm-boring-file-regexp-list '("~$" "\\.meta$")))
  :config
  (helm-mode t)
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-x C-i" . helm-imenu)
   ("C-x C-b" . helm-buffers-list)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)))
(use-package company :ensure
  :init (global-company-mode)
  :bind (("C-;" . company-complete))
  )
(use-package magit :ensure
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (setq vcs-ediff-p nil)
    (defadvice magit-ediff (around flymake-off activate)
      (setq vcs-ediff-p t)
      ad-do-it
      (setq vcs-ediff-p nil))))
(use-package git-gutter-fringe+ :config (global-git-gutter+-mode t) :ensure)
(use-package yasnippet
  :ensure
  :config
  (progn
    (yas/global-mode 1)
    (setq yas/prompt-functions '(yas/completing-prompt))
    ;; (eval-after-load 'company
    ;; '(add-to-list 'company-backends 'company-yasnippet))
    ))

(use-package popwin :ensure
  :config
  (progn
    (setq display-buffer-function 'popwin:display-buffer)
    (dolist (window '((" *auto-async-byte-compile*")
                      (":home" :position left)))
      (add-to-list 'popwin:special-display-config window))))
(use-package exec-path-from-shell :config (exec-path-from-shell-initialize) :ensure)
(use-package open-junk-file :ensure :commands open-junk-file)
(use-package yaml-mode :mode "\\.yml$" :ensure)
(use-package powerline :config (powerline-default-theme) :ensure)
(use-package skk)
(use-package color-theme :ensure)
(use-package solarized :config (load-theme 'solarized-light t) :ensure solarized-theme)
(use-package expand-region :bind ("C-M-SPC" . er/expand-region) :ensure)
(use-package ace-isearch :config (global-ace-isearch-mode t) :ensure)
(use-package undo-tree :config (global-undo-tree-mode t) :ensure :disabled)
(use-package escreen)
(use-package auto-async-byte-compile :ensure
  :config (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))
(use-package flymake)
(use-package flymake-cursor)
;; (use-package nlinum :ensure nlinum :config (global-nlinum-mode))

;; load language settings
;; (load "c-config")
(load "python-config")
(load "ruby-config")
(load "coffee-config")
(load "scheme-config")
(load "latex-mode-config")
