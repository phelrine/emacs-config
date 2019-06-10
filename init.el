;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setenv "LANG" "ja_JP.UTF-8")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-z" nil)
(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))
(when (getenv "GOPATH")
  (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin")))

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
  (nconc default-frame-alist '((width . 120)(height . 40)))
  (defvar xcode:sdk "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk")
  (defvar xcode:frameworks (concat xcode:sdk "/System/Library/Frameworks/"))
  (defvar xcode:headers (concat xcode:sdk "/usr/include/"))
  (defvar company-clang-argument
    '("-Wall" "-Wextra" "-fsyntax-only" "-ObjC" "-std=c99" "-fblocks" "-fobjc-arc"
      "-isysroot" xcode:sdk "-I" xcode:headers "-I." "-D__IPHONE_OS_VERSION_MIN_REQUIRED=70000")))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(global-font-lock-mode 1)
(use-package eldoc
  :diminish eldoc-mod
  :commands global-eldoc-mode
  :config (global-eldoc-mode t))

(require 'tramp)
(nconc tramp-default-proxies-alist
       '((nil "\\`root\\'" "/ssh:%h:")
         ("localhost" nil nil)
         ((regexp-quote (system-name)) nil nil)))

;; builtin
(require 'hl-line)
(require 'savehist)
(require 'recentf)
(use-package saveplace :commands save-place-mode :config (save-place-mode))
(use-package whitespace :hook (before-save . delete-trailing-whitespace))

;;; packages
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
         ("C-x C-i" . counsel-imenu)))
(use-package swiper :bind ("C-s" . swiper))
(use-package counsel-projectile
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
(use-package dired-k
  :hook ((dired-initial-position . dired-k)
         (dired-after-readin . dired-k-no-revert)))
(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map (("." . dired-hide-dotfiles-mode)))
  :hook (dired-mode . dired-hide-dotfiles-mode))

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
  :hook ((go-mode . lsp) (ruby-mode . lsp) (dart-mode . lsp))
  :commands lsp
  :config
  (use-package lsp-ui
    :custom (scroll-margin 0)
    :bind
    (:map lsp-mode-map
          ("C-c C-r" . lsp-ui-peek-find-references)
          ("M-." . lsp-ui-peek-find-definitions)
          ("C-M-." . lsp-ui-peek-find-implementation)
          ("C-x C-i"   . lsp-ui-imenu))
    :hook (lsp-mode . lsp-ui-mode)))

(use-package dap-mode
  :hook ((prog-mode . dap-mode) (prog-mode . dap-ui-mode))
  :bind (:map dap-mode-map (("C-c d" . dap-debug)))
  :config
  (require 'dap-go))

(use-package company-lsp :after (lsp-mode company))

;; git
(use-package magit :bind (("C-x g" . magit-status)))
(use-package git-gutter-fringe+ :diminish git-gutter+-mode)

(use-package yasnippet :hook (prog-mode . yas-minor-mode) :commands yas-reload-all :config (yas-reload-all))
(use-package popwin
  :config
  (dolist (window '(("*auto-async-byte-compile*")
                    (":home" :position left)
                    ("*compilation*")))
    (add-to-list 'popwin:special-display-config window)))


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (let ((gopath (getenv "GOPATH")))
    (if gopath (add-to-list 'exec-path (concat (car (split-string  ":")) "/bin"))))
  (let ((flutter-path (exec-path-from-shell-copy-env "FLUTTER_PATH")))
    (when flutter-path
      (add-to-list 'exec-path (concat flutter-path "/.pub-cache/bin"))
      (add-to-list 'exec-path (concat flutter-path "/bin/cache/dart-sdk/bin")))))

(use-package open-junk-file :commands open-junk-file)
(use-package yaml-mode :mode "\\.yml$")
(use-package color-theme-modern)
(use-package solarized-theme :config (load-theme 'solarized-light t))
(use-package doom-modeline :hook (after-init . doom-modeline-mode))
(use-package expand-region :bind ("C-M-SPC" . er/expand-region))
(use-package undo-tree :commands (global-undo-tree-mode undo-tree-visualize) :config (global-undo-tree-mode t))
(use-package auto-async-byte-compile :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode))
(use-package ddskk :bind ("C-x j" . skk-mode))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package migemo
  :if (executable-find "cmigemo")
  :commands (migemo-init)
  :config
  (setq migemo-dictionary
        (cond
         ((eq system-type 'darwin) "/usr/local/share/migemo/utf-8/migemo-dict")
         ((eq system-type 'gnu-linux) "/usr/share/cmigemo/utf-8/migemo-dict")))
  (migemo-init))

(if (require 'cursor-in-brackets nil t)
    (global-cursor-in-brackets-mode 1)
  (message "cannot load cursor-in-brackets"))

;; flycheck
(use-package flycheck
  :hook ((c-mode . flycheck-setting-c/c++)
         (c++-mode . flycheck-setting-c/c++)
         (csharp-mode . flycheck-mode)
         (ruby-mode . flycheck-mode)
         (go-mode . flycheck-mode)
         (objc-mode . flycheck-mode)))
(use-package flycheck-color-mode-line :after flycheck :hook (flychech-mode . flycheck-color-mode-line-mode))

(if (fboundp 'display-line-numbers-mode) (add-hook 'prog-mode-hook 'display-line-numbers-mode))
(use-package nlinum :if (version< emacs-version "26.0.0") :hook (prog-mode . nlinum-mode))
(use-package highlight-indent-guides :hook (prog-mode . highlight-indent-guides-mode))
(use-package volatile-highlights
  :diminish
  :hook
  (after-init . volatile-highlights-mode))

;;; CC-Mode
(defun cc-mode-setup()
  (setq-default c-basic-offset 4
                indent-tabs-mode nil
                comment-column 40)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-c c") 'compile))
(add-hook 'c-mode-common-hook 'cc-mode-setup)

;;; C++
(add-to-list 'auto-mode-alist '(".+\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (or (re-search-forward "#\\<import\\>" magic-mode-regexp-match-limit t)
                           (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)
                           (re-search-forward "@\\<protocol\\>" magic-mode-regexp-match-limit t))))
               . objc-mode))

(require 'cc-mode)
(require 'semantic)

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

;;; Python
(require 'ipython nil t)
(use-package ein)

;;; Ruby
(use-package inf-ruby :hook (ruby-mode . inf-ruby-minor-mode))
(use-package robe :hook (ruby-mode . robe-mode)
  :config
  (push 'company-robe company-backends))
(use-package projectile-rails)
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

;;; Dart
(use-package dart-mode)

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

(use-package web-mode :mode ".+\\.(erb|html)$")
(use-package apib-mode :mode "\\.apib$")
(use-package wakatime-mode)
(use-package json-reformat)

;; (load "latex-mode-config")
;; ライブコーディング用設定
;; (set-face-attribute 'default nil :height 300)

;;; Load custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
