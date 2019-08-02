;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; ENV
(setenv "LANG" "ja_JP.UTF-8")
(defun flutter-exec-path ()
  (let ((flutter-path (getenv "FLUTTER_PATH")))
    (if flutter-path
        (mapcar (lambda (path) (concat flutter-path path))
                '("/bin" "/.pub-cache/bin" "/bin/cache/dart-sdk/bin"))
      nil)))

(defun go-exec-path ()
  (mapcar (lambda (path)
            (concat path "/bin"))
          (remove "" (split-string (or (getenv "GOPATH") "") ":"))))

(nconc exec-path
       (remove nil `(,(concat (getenv "HOME") "/bin")
                     ,@(flutter-exec-path)
                     ,@(go-exec-path))))

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-z" nil)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'message-box 'message)
(when (eq (window-system) 'ns)
  (set-face-attribute 'default nil :family "Menlo" :height 120)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "Hiragino Kaku Gothic ProN"))
  (add-to-list 'face-font-rescale-alist
               '(".*Hiragino Kaku Gothic ProN.*" . 1.1))
  (nconc default-frame-alist '((width . 120)(height . 40))))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(global-font-lock-mode 1)
(use-package eldoc
  :diminish eldoc-mode
  :commands global-eldoc-mode
  :config (global-eldoc-mode t))

(require 'tramp)
;; /sudo:hostname でリモートログインしてファイルを開ける
;; リモートのファイルをroot権限で編集する
;; https://qiita.com/miyakou1982/items/d05e1ce07ad632c94720
(nconc tramp-default-proxies-alist
       '((nil "\\`root\\'" "/ssh:%h:")
         ("localhost" nil nil)
         ((regexp-quote (system-name)) nil nil)))

;; builtin
(require 'hl-line)
(require 'savehist)
(use-package recentf :config (use-package recentf-ext))
(use-package saveplace :commands save-place-mode :config (save-place-mode))
(use-package whitespace :hook (before-save . delete-trailing-whitespace))

;;; packages
(use-package projectile
  :diminish
  :custom (projectile-completion-system 'ivy)
  :defines projectile-project-root-files-bottom-up
  :config
  ;; https://github.com/bradyt/dart-mode/wiki/LSP#lsp-mode
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package ivy
  :commands ivy-mode
  :config
  (use-package ivy-rich :commands ivy-rich-mode :config (ivy-rich-mode 1))
  (ivy-mode t))

(use-package counsel
  :bind (("C-x C-r" . counsel-recentf)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x b"   . ivy-switch-buffer)
         ("C-c g" . counsel-git-grep)
         ("C-x C-i" . counsel-imenu)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)))

(use-package swiper :bind ("C-s" . swiper))

(use-package counsel-projectile
  :after (projectile counsel)
  :bind (("C-x p" . projectile-command-map))
  :custom (counsel-projectile-mode t))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package hydra)

(use-package bm
  :bind (("M-[" . bm-previous)
         ("M-]" . bm-next)
         ("M-SPC" . bm-toggle))
  :config
  (setq bm-restore-repository-on-load t)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda ()
                                (bm-buffer-save-all)
                                (bm-repository-save))))

;;; Dired
(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map (("." . dired-hide-dotfiles-mode)))
  :hook (dired-mode . dired-hide-dotfiles-mode))
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(defun find-file-default-directory ()
  (interactive)
  (find-file default-directory))
(global-set-key (kbd "C-x d") 'find-file-default-directory)

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package company
  :bind ("C-;" . company-complete)
  :defines (company-backends)
  :custom
  (company-backends '(company-lsp company-capf company-dabbrev-code company-files))
  :commands global-company-mode
  :config
  (require 'company-capf)
  (global-company-mode)
  (use-package company-statistics :custom (company-statistics-mode t)))

(use-package yasnippet)

(use-package lsp-mode
  :custom
  (lsp-auto-guess-root t)
  :hook ((go-mode . lsp) (ruby-mode . lsp) (dart-mode . lsp))
  :commands lsp
  :config
  (use-package lsp-ui
    :custom (scroll-margin 0)
    :bind
    (:map lsp-mode-map
          ("C-c C-r" . lsp-ui-peek-find-references)
          ("M-." . lsp-ui-peek-find-definitions)
          ("C-M-." . lsp-ui-peek-find-implementation))
    :hook (lsp-mode . lsp-ui-mode)))

(use-package dap-mode
  :hook ((prog-mode . dap-mode) (prog-mode . dap-ui-mode))
  :bind (:map dap-mode-map (("C-c d" . dap-debug)))
  :config
  (require 'dap-go)
  (require 'dap-ruby))

(use-package company-lsp :after (lsp-mode company))

;; git
(use-package magit :bind (("C-x g" . magit-status)))
(use-package gitattributes-mode :defer t)
(use-package gitconfig-mode :defer t)
(use-package gitignore-mode :defer t)
(use-package git-gutter-fringe+ :diminish git-gutter+-mode)

(use-package yasnippet :hook (prog-mode . yas-minor-mode) :commands yas-reload-all :config (yas-reload-all))
(use-package popwin
  :custom (popwin-mode 1)
  :config
  (nconc popwin:special-display-config
         '((" *auto-async-byte-compile*" :noselect t)
           ("*Rubocopfmt Errors*" :noselect t))))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom (exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "FLUTTER_PATH"))
  :config
  (exec-path-from-shell-initialize)
  (nconc exec-path (remove nil `(,@(flutter-exec-path) ,@(go-exec-path)))))

(use-package open-junk-file :commands open-junk-file)
(use-package color-theme-modern)
(use-package solarized-theme :config (load-theme 'solarized-light t))
(use-package doom-modeline :hook (after-init . doom-modeline-mode))
(use-package expand-region :bind ("C-M-SPC" . er/expand-region))
(use-package undo-tree :commands (global-undo-tree-mode undo-tree-visualize) :bind ("C-x u" . undo-tree-visualize) :config (global-undo-tree-mode t))
(use-package auto-async-byte-compile :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode))
(use-package ddskk :bind ("C-x j" . skk-mode))

(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (setq migemo-dictionary
        (cond
         ((eq system-type 'darwin) "/usr/local/share/migemo/utf-8/migemo-dict")
         ((eq system-type 'gnu-linux) "/usr/share/cmigemo/utf-8/migemo-dict")))
  (migemo-init))

;; flycheck
(use-package flycheck
  :hook ((csharp-mode . flycheck-mode)
         (ruby-mode . flycheck-mode)
         (go-mode . flycheck-mode)
         (objc-mode . flycheck-mode)))
(use-package flycheck-color-mode-line :after flycheck :hook (flychech-mode . flycheck-color-mode-line-mode))

(if (fboundp 'display-line-numbers-mode) (add-hook 'prog-mode-hook 'display-line-numbers-mode))
(use-package nlinum :if (version< emacs-version "26.0.0") :hook (prog-mode . nlinum-mode))
(use-package volatile-highlights
  :diminish
  :hook
  (after-init . volatile-highlights-mode))

(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;;; Intent
(use-package highlight-indent-guides :hook (prog-mode . highlight-indent-guides-mode))
(use-package indent-tools :bind ("C-c >" . indent-tools-hydra/body))

;;; C#
(use-package csharp-mode :mode "\\.cs$")
(use-package omnisharp)

;;; Obj-C
(use-package objc-font-lock :config (add-hook 'objc-mode-hook 'objc-font-lock-mode))
(defadvice ff-get-file-name (around ff-get-file-name-framework (search-dirs fname-stub &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
              (header (match-string 2 fname-stub))
              (fname-stub (concat framework ".framework/Headers/" header)))
         ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)
(if (eq window-system 'ns)
    (setq-default cc-search-directories
                  '("." "../include" "/usr/include" "/usr/local/include/*"
                    "/System/Library/Frameworks" "/Library/Frameworks")))

;;; Swift
(use-package swift-mode :mode "\\.swift$")

;;; Python
(require 'ipython nil t)
(use-package ein)

;;; Ruby
(use-package inf-ruby :hook (ruby-mode . inf-ruby-minor-mode))
(use-package robe :hook (ruby-mode . robe-mode)
  :config
  (push 'company-robe company-backends))
(use-package rubocopfmt
  :hook (ruby-mode . rubocopfmt-mode))
(setq-default ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))
(use-package projectile-rails
  :bind (:map projectile-rails-mode-map (("C-c r" . hydra-projectile-rails/body)))
  :custom (projectile-rails-global-mode t))
(use-package bundler)
(use-package haml-mode)

;;; Go
(use-package go-mode
  :hook ((before-save . gofmt-before-save) (go-mode . subword-mode))
  :config
  (use-package go-projectile
    :hook (go-mode . go-projectile-set-gopath)
    :commands (go-projectile-set-gopath))
  (use-package govet)
  (use-package go-tag
    :after go-mode
    :bind (:map go-mode-map
                (("C-c `" . go-tag-add)
                 ("C-u C-c `" . go-tag-remove))))
  (use-package go-impl)
  (use-package go-gen-test)
  (use-package go-eldoc :after go-mode :hook (go-mode . go-eldoc-setup)))

;;; Dart & Flutter
(use-package dart-mode)
(use-package flutter :requires dart-mode)

;;; Web
(use-package web-mode :mode ".+\\.(erb|html)$")
(use-package vue-mode)
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

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

(defun scheme-mode-setup ()
  (local-set-key (kbd "C-c s") 'scheme-other-window))
(add-hook 'scheme-mode-hook 'scheme-mode-setup)

;;; config files
(use-package nginx-mode :mode "/nginx/sites-\\(?:available\\|enabled\\)/" :defer t)
(use-package dockerfile-mode :defer t)
(use-package json-mode :defer t)
(use-package yaml-mode :mode "\\.yml$" :defer t)
(use-package apib-mode :mode "\\.apib$" :defer t)

(use-package wakatime-mode)
(use-package json-reformat)
(use-package restart-emacs)

;; ライブコーディング用設定
;; (set-face-attribute 'default nil :height 300)

;;; Load custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
