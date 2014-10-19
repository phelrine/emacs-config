(require 'cl)
(require 'package)
(package-initialize)

(let ((path (substring (shell-command-to-string "echo $PATH") 0 -1)))
  (setq exec-path (split-string path ":"))
  (setenv "PATH" path))
(setenv "LANG" "ja_JP.UTF-8")

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar installed-packages
  '(
    ace-isearch
    auto-async-byte-compile
    auto-complete
    coffee-mode
    color-theme
    company
    csharp-mode
    escreen
    expand-region
    flymake-cursor
    flymake-python-pyflakes
    fuzzy
    haml-mode
    helm
    key-chord
    magit
    markdown-mode
    open-junk-file
    popwin
    powerline
    smartparens
    smartrep
    solarized-theme
    switch-window
    undo-tree
    yaml-mode
    yasnippet
    exec-path-from-shell
    use-package
    helm-company
    git-gutter-fringe+
    ))

(let ((not-installed
       (loop for x in installed-packages if (not (package-installed-p x)) collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

(require 'use-package nil t)
(use-package open-junk-file :commands open-junk-file)
(use-package auto-async-byte-compile
  :config (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

(use-package helm-config
  :config
  (progn
    (helm-mode t)
    (custom-set-variables
     '(helm-ff-skip-boring-files t)
     '(helm-boring-file-regexp-list '("~$" "\\.meta$"))))
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-x C-i" . helm-imenu)
   ("C-x C-b" . helm-buffers-list)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ))

(use-package company :init (add-hook 'after-init-hook 'global-company-mode))

(use-package flymake
  :config
  (progn
    (use-package flymake-cursor)
    (set-face-foreground 'flymake-errline "orange")
    (set-face-background 'flymake-errline "blue")
    (set-face-background 'flymake-warnline "yellow")
    (defvar flymake-err-line-patterns
      `(("\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)" 1 2 3 4) ; gcc 4.5
        ,@flymake-err-line-patterns))
    ))

(use-package magit :bind (("C-x g" . magit-status)))

(use-package popwin
  :config
  (progn
    (setq display-buffer-function 'popwin:display-buffer)
    (setq popwin:special-display-config
          (append popwin:special-display-config
                  '((" *auto-async-byte-compile*")
                    (":home" :position left))))
    ))

(use-package undo-tree :config (global-undo-tree-mode))

(use-package saveplace :config (setq-default save-place t))

(use-package savehist
  :config
  (progn
    (setq avehist-mode 1)
    (setq history-length 5000)))

(use-package yaml-mode :mode "\\.yml$")

(use-package yasnippet
  :config
  (progn
    (yas/global-mode 1)
    (setq yas/prompt-functions '(yas/completing-prompt))))

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

(use-package hl-line
  :config
  (progn
    (set-face-background 'hl-line "violet")
    (set-face-underline-p 'hl-line "blue")
    (global-hl-line-mode t)))

(custom-set-variables
 '(global-auto-revert-mode t)
 '(show-paren-mode 1)
 '(make-backup-files nil)
 '(tab-width 4)
 '(indent-tabs-mode nil))

(menu-bar-mode -1)
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

(load "c-config")
(load "python-config")
(load "ruby-config")
(load "coffee-config")
(load "latex-mode-config")

;;; Scheme
(defconst scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(add-hook 'scheme-mode-hook
          (lambda () (local-set-key (kbd "C-c s") 'scheme-other-window)))

;;; Perl
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

(when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))
(defface my-face-b-1 '((t (:background "red"))) nil)
(defface my-face-b-2 '((t (:background "blue"))) nil)
(defface my-face-u-1 '((t (:underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks
          '(lambda ()
             (if font-lock-mode
                 nil
               (font-lock-mode t))) t)

(use-package powerline :config (powerline-default-theme))
(use-package escreen)
(use-package skk)
(use-package color-theme)
(use-package solarized :config (load-theme 'solarized-light t))
(use-package exec-path-from-shell :config (exec-path-from-shell-initialize))
(use-package expand-region :bind ("C-M-SPC" . er/expand-region))
(use-package git-gutter-fringe+ :config (global-git-gutter+-mode t))
(use-package helm-company :bind ("C-;" . helm-company))
(use-package ace-isearch :config (global-ace-isearch-mode t))
