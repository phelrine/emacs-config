;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)
(use-package hydra)

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :commands (auto-package-update-maybe)
  :config
  (auto-package-update-maybe))

;;; ENV
(setenv "LANG" "ja_JP.UTF-8")
(use-package exec-path-from-shell
  :functions exec-path-from-shell-initialize
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH"))
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-z" nil)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(use-package paren :custom (show-paren-mode 1))
(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'message-box 'message)
(when (eq (window-system) 'ns)
  (set-face-attribute 'default nil :family "Menlo" :height 120)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "Hiragino Kaku Gothic ProN"))
  (add-to-list 'face-font-rescale-alist
               '(".*Hiragino Kaku Gothic ProN.*" . 1.1)))

(global-font-lock-mode 1)
(use-package eldoc
  :diminish eldoc-mode
  :custom (global-eldoc-mode 1))

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
(use-package savehist :custom (savehist-mode 1))
(use-package recentf-ext)
(use-package saveplace :custom (save-place-mode 1))

;;; packages
(use-package avy :bind (("C-'" . avy-goto-char-timer)))
(use-package ace-window :bind (("C-x o" . ace-window)))

(use-package all-the-icons)
(use-package projectile
  :diminish
  :custom (projectile-completion-system 'ivy)
  :defines projectile-project-root-files-bottom-up
  :bind (("C-x p" . projectile-command-map))
  :config
  ;; https://github.com/bradyt/dart-mode/wiki/LSP#lsp-mode
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package ivy
  :diminish ivy-mode
  :custom
  (ivy-mode 1)
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))
(use-package counsel
  :bind (([remap find-file] . counsel-find-file)
         ([remap switch-to-buffer] . ivy-switch-buffer)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x C-r" . counsel-recentf)
         ("C-c g"   . counsel-git-grep)
         ("C-x C-i" . counsel-imenu)
         ("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))
(use-package all-the-icons-ivy :after (all-the-icons) :functions all-the-icons-ivy-setup :config (all-the-icons-ivy-setup))
(use-package ivy-rich :after (ivy) :custom (ivy-rich-mode 1))
(use-package ivy-hydra :after (ivy hydra))
(use-package swiper :bind ("C-s" . swiper))

(use-package counsel-projectile
  :after (projectile counsel)
  :custom (counsel-projectile-mode t))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

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

(use-package autorevert :diminish :hook (after-init . global-auto-revert-mode))

(use-package company
  :bind ("C-;" . company-complete)
  :defines company-backends
  :custom
  (global-company-mode 1)
  (company-backends '(company-capf company-dabbrev-code company-files company-elisp company-yasnippet))
  :config
  (require 'company-capf)
  (use-package company-statistics :custom (company-statistics-mode 1))
  (use-package company-quickhelp :hook (company-mode . company-quickhelp-mode)))

(use-package lsp-mode
  :diminish
  :init (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-auto-guess-root t)
  (lsp-solargraph-use-bundler t)
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp-mode)
  :config
  (require 'lsp-solargraph))
(use-package lsp-ui :hook (lsp-mode . lsp-ui-mode))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :hook ((prog-mode . dap-mode) (prog-mode . dap-ui-mode) (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))
  :bind (:map dap-mode-map (("C-c d" . dap-debug)))
  :commands (dap-hydra dap-register-debug-template)
  :config
  (require 'dap-go)
  (require 'dap-ruby))

;; git
(use-package magit :bind (("C-x g" . magit-status)))
(use-package gitattributes-mode :defer t)
(use-package gitconfig-mode :defer t)
(use-package gitignore-mode :defer t)
(use-package git-gutter-fringe+ :diminish git-gutter+-mode)
(use-package forge :after magit)
(use-package github-review)
(use-package gist)


;;; Terminal
(use-package shell-pop
  :custom
  (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (shell-pop-term-shell "/bin/zsh")
  (shell-pop-window-size 30)
  (shell-pop-window-position "bottom")
  :bind (("C-M-p" . shell-pop)))

(use-package yasnippet :diminish yas-minor-mode :hook (prog-mode . yas-minor-mode))
(use-package popwin
  :custom (popwin-mode 1)
  :config
  (nconc popwin:special-display-config
         '((" *auto-async-byte-compile*" :noselect t)
           ("*Warnings*" :noselect t)
           ("*Rubocopfmt Errors*" :noselect t))))

(use-package open-junk-file :commands open-junk-file)
(use-package color-theme-modern)
(use-package solarized-theme :config (load-theme 'solarized-light t))
(use-package doom-modeline
  :custom (doom-modeline-minor-modes t)
  :hook (after-init . doom-modeline-mode))
(if (fboundp 'display-line-numbers-mode) (add-hook 'prog-mode-hook 'display-line-numbers-mode))
(use-package nlinum :if (version< emacs-version "26.0.0") :hook (prog-mode . nlinum-mode))

(use-package expand-region :bind ("C-M-SPC" . er/expand-region))
(use-package undo-tree :diminish :custom (global-undo-tree-mode 1) :bind ("C-x u" . undo-tree-visualize))
(use-package volatile-highlights :diminish :hook (after-init . volatile-highlights-mode))
(use-package beacon :custom (beacon-mode 1))

(use-package ddskk :if (memq window-system '(mac ns x)) :bind ("C-x j" . skk-mode))
(use-package migemo
  :if (executable-find "cmigemo")
  :commands migemo-init
  :config
  (setq migemo-dictionary
        (cond
         ((eq system-type 'darwin) "/usr/local/share/migemo/utf-8/migemo-dict")
         ((eq system-type 'gnu-linux) "/usr/share/cmigemo/utf-8/migemo-dict")))
  (migemo-init))

;; flycheck
(use-package flycheck :hook (prog-mode . flycheck-mode))
(use-package flycheck-color-mode-line :after flycheck :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package smartparens
  :diminish
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;;; Indent
(use-package whitespace :diminish global-whitespace-mode :custom (global-whitespace-mode 1) :hook (before-save . delete-trailing-whitespace))
(setq show-trailing-whitespace t)
(add-hook 'change-major-mode-after-body-hook
          '(lambda ()
             (when (derived-mode-p 'term-mode)
               (setq show-trailing-whitespace nil))))
(add-hook 'minibuffer-setup-hook (lambda () (setq-local show-trailing-whitespace nil)))

(use-package highlight-indent-guides :diminish :hook (prog-mode . highlight-indent-guides-mode))
(use-package indent-tools :bind ("C-c >" . indent-tools-hydra/body))

(use-package restclient)

;;; Emacs Lisp
(use-package auto-async-byte-compile :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode))
(add-hook 'emacs-lisp-mode-hook '(lambda () (local-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)))

;;; C#
(use-package csharp-mode :mode "\\.cs$")
(use-package omnisharp)

;;; Obj-C
(use-package objc-font-lock :hook (objc-mode . objc-font-lock-mode))
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
(use-package ein :defer t)

;;; Ruby
(use-package inf-ruby
  :hook
  (ruby-mode . inf-ruby-minor-mode)
  (after-init . inf-ruby-switch-setup))
(use-package robe :hook (ruby-mode . robe-mode) :commands company-robe)
(add-hook 'ruby-mode-hook
          (lambda ()
            (lsp-mode)
            (make-local-variable 'company-backends)
            (push 'company-robe company-backends)))

(use-package rubocopfmt :hook (ruby-mode . rubocopfmt-mode))
(use-package projectile-rails
  :commands (projectile-rails-root)
  :bind (:map projectile-rails-mode-map
              ("C-c r" . hydra-projectile-rails/body)
              ("C-c f" . hydra-projectile-rails-find/body))
  :custom (projectile-rails-global-mode t))

(defun rails-project-find-file-hook ()
  (when (projectile-rails-root)
    (dap-register-debug-template
     (concat "Debug Rails Server (" (file-name-nondirectory (directory-file-name (projectile-rails-root))) ")")
     (list :type "Ruby"
           :request "launch"
           :cwd (projectile-rails-root)
           :program (concat (projectile-rails-root) "bin/rails")
           :pathToBundler (concat (getenv "HOME") "/.rbenv/shims/bundler")
           :pathToRDebugIDE: (concat (getenv "HOME") "/.rbenv/shims/rdebug-ide")
           :args '("server" "-p" "3000")
           ))))
(add-hook 'find-file-hook #'rails-project-find-file-hook)
(use-package rspec-mode
  :custom (rspec-key-command-prefix (kbd "C-c s")))
(use-package bundler)
(use-package coverage)

;;; HAML
(use-package haml-mode)
(use-package flymake-haml :hook (haml-mode . flymake-haml-load))

;;; Go
(use-package go-mode
  :hook ((before-save . gofmt-before-save) (go-mode . subword-mode) (go-mode . lsp))
  :config
  (use-package go-projectile :hook (go-mode . go-projectile-set-gopath))
  (use-package govet)
  (use-package go-tag
    :bind (:map go-mode-map
                ("C-c `" . go-tag-add)
                ("C-u C-c `" . go-tag-remove)))
  (use-package go-impl)
  (use-package go-gen-test)
  (use-package go-eldoc :hook (go-mode . go-eldoc-setup)))

;;; Dart & Flutter
(use-package dart-mode :hook (dart-mode . lsp) (dart-mode . subword-mode))
(use-package flutter :requires dart-mode)

;;; Gradle
(use-package groovy-mode)

;;; Web
(use-package web-mode :mode ".+\\.(erb|html)$")
(defun set-js-indent-level()
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))
(use-package vue-mode :hook (vue-mode . set-js-indent-level) (vue-mode . lsp))
;; https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-539711083
(setq-default mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq-default mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (use-package eslintd-fix :hook (vue-mode . eslintd-fix-mode))
;; (use-package yarn-mode)
(add-hook 'js-mode-hook #'set-js-indent-level)
(use-package typescript-mode :hook (typescript-mode . (lambda() (setq-default typescript-indent-level 2))))
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
;;; Scheme
(defconst scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(defun scheme-other-window ()
  "Run scheme on other window."
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(defun scheme-mode-setup ()
  "Set up scheme mode."
  (local-set-key (kbd "C-c s") 'scheme-other-window))
(add-hook 'scheme-mode-hook 'scheme-mode-setup)

;;; Docker
(use-package docker
  :bind ("C-c C-d" . docker))
(use-package docker-tramp)
(use-package dockerfile-mode :defer t)
(use-package docker-compose-mode :defer t)

;;; config files
(use-package nginx-mode :mode "/nginx/sites-\\(?:available\\|enabled\\)/" :defer t)
(use-package json-mode :defer t)
(use-package yaml-mode :mode "\\.yml$" :defer t)
(use-package apib-mode :mode "\\.apib$" :defer t)

;; (use-package wakatime-mode :diminish)
(use-package json-reformat)
(use-package restart-emacs)
;; ライブコーディング用設定
;; (set-face-attribute 'default nil :height 300)

;;; Load custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
