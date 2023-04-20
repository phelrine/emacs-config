;;; init --- Summary
;;; Commentary:

(require 'package)
(require 'generic-x)

;;; Code:
;;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cc-other-file-alist
   '(("\\.cpp$"
      (".hpp" ".h"))
     ("\\.h$"
      (".c" ".cpp" ".m" ".mm"))
     ("\\.hpp$"
      (".cpp" ".c"))
     ("\\.m$"
      (".h"))
     ("\\.mm$"
      (".h"))))
 '(create-lockfiles nil)
 '(enable-recursive-minibuffers t)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(history-length 1000)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(recentf-max-saved-items 1000)
 '(ring-bell-function 'ignore)
 '(scroll-margin 0)
 '(sort-fold-case t t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:background "black" :foreground "LightYellow" :inverse-video t)))))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(use-package diminish)
(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))
(use-package bind-key)

;;; ENV
(setenv "LANG" "ja_JP.UTF-8")
(setq temporary-file-directory (concat (getenv "HOME") "/.tmp"))
(if (memq window-system '(mac ns))
    (setenv "TMPDIR" (concat (getenv "HOME") "/.tmp")))
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(winner-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(global-font-lock-mode 1)
(when (memq window-system '(mac ns))
  (add-to-list 'auth-sources 'macos-keychain-generic)
  (add-to-list 'auth-sources 'macos-keychain-internet))
(global-set-key (kbd "C-z") 'winner-undo)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'message-box 'message)
(when (eq (window-system) 'ns)
  (set-face-attribute 'default nil :family "Menlo" :height 180)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "Hiragino Kaku Gothic ProN"))
  (add-to-list 'face-font-rescale-alist
               '(".*Hiragino Kaku Gothic ProN.*" . 1.1)))
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)))
(global-eldoc-mode 1)
(use-package eldoc-box
  :after (eldoc)
  :bind ("C-c h" . eldoc-box-help-at-point)
  :config (eldoc-box-hover-at-point-mode 1))

(use-package recentf-ext)

(use-package ace-window :bind (("C-x o" . ace-window)))

(use-package all-the-icons :defer t)
(use-package projectile
  :diminish
  :defines projectile-project-root-files-bottom-up
  :bind (("C-x p" . projectile-command-map))
  :autoload projectile-project-root
  :config
  (projectile-mode +1)
  ;; FLUTTER
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package vertico :init (vertico-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
(use-package all-the-icons-completion :init (all-the-icons-completion-mode))
(use-package consult
  :bind (("C-x C-r" . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap isearch-forward] . consult-line)
         ("C-x C-b" . consult-buffer)
         ("C-x C-i" . consult-imenu)
         ("C-c g" . consult-git-grep)
         ("M-y" . consult-yank-pop)))
(use-package consult-flycheck
  :after (consult flycheck)
  :bind (("C-c C-e" . consult-flycheck)
         ("C-c e" . consult-flycheck)))

(use-package which-key :diminish which-key-mode :hook after-init)

;;; Dired
(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map (("." . dired-hide-dotfiles-mode)))
  :hook dired-mode)
(use-package all-the-icons-dired :diminish :hook dired-mode)
(defun find-file-default-directory ()
  "Open current directory in Dired."
  (interactive)
  (find-file default-directory))
(global-set-key (kbd "C-x d") 'find-file-default-directory)
(use-package autorevert :diminish :hook (after-init . global-auto-revert-mode))

(use-package company
  :bind ("C-;" . company-complete)
  :defines company-backends
  :custom
  (company-backends '(company-capf company-dabbrev-code company-files company-elisp company-yasnippet))
  (company-dabbrev-downcase nil)
  (company-idle-delay nil)
  (company-lsp-enable-recompletion nil)
  :config
  (global-company-mode 1))
(use-package company-box :after company :diminish :hook company-mode)
(use-package company-quickhelp :after company :hook company-mode)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook prog-mode
  :bind (("C-M-;" . copilot-complete)
         ("TAB" . my/copilot-accept-completion)
         ("<backtab>" . copilot-next-completion))
  :custom
  (copilot-disable-predicates '((lambda () t)))
  (copilot-node-executable (concat (getenv "HOME") "/.asdf/installs/nodejs/lts-gallium/bin/node"))
  :autoload copilot-accept-completion
  :init
  (defun my/copilot-accept-completion ()
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command))))

(use-package lsp-mode
  :diminish
  :defer t
  :custom
  (lsp-auto-guess-root t)
  (lsp-solargraph-use-bundler t)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp-rename
  :config
  (setq lsp-keymap-prefix "C-c l")
  (defun lsp-rename-snake-to-camel ()
    "Rename symbol from snake_case to camelCase."
    (interactive)
    (lsp-rename (string-inflection-camelcase-function (thing-at-point 'symbol)))))
(use-package string-inflection :autoload string-inflection-camelcase-function)
(use-package lsp-ui :after lsp-mode :hook lsp-mode)
(use-package dap-mode
  :hook ((prog-mode . dap-mode) (prog-mode . dap-ui-mode) (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))
  :bind (:map dap-mode-map (("C-c d" . dap-debug)))
  :commands dap-register-debug-template
  :config
  (require 'dap-dlv-go)
  (require 'dap-ruby)
  (require 'dap-chrome)
  (require 'dap-node))

;; git
(use-package magit
  :custom (magit-repository-directories (list (cons (concat (getenv "HOME") "/repos/") 1)))
  :bind (("C-x g" . magit-status)
         ([remap vc-dir] . magit-status)))
(use-package forge :after magit :custom (forge-topic-list-limit '(50 . 0)))
;; (use-package emacsql-sqlite-module :defer t)
(use-package git-gutter :init (global-git-gutter-mode +1))
(use-package git-gutter-fringe :diminish git-gutter-mode)
(use-package github-review :defer t)
(use-package code-review :defer t)
(use-package gist :defer t)
(use-package browse-at-remote :defer t)
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
  :config
  (popwin-mode 1)
  (nconc popwin:special-display-config
         '((" *auto-async-byte-compile*" :noselect t)
           ("*Warnings*" :noselect t)
           ("*Rubocopfmt Errors*" :noselect t))))

(use-package open-junk-file :defer t)
(use-package color-theme-modern)
(use-package solarized-theme :config (load-theme 'solarized-light t))
(use-package doom-modeline :custom (doom-modeline-minor-modes t) :hook after-init)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package expand-region :bind ("C-M-SPC" . er/expand-region))
(use-package vundo :bind ("C-x u" . vundo))

(use-package volatile-highlights :diminish :hook after-init)
(use-package beacon :diminish beacon-mode :config (beacon-mode t) :custom (beacon-color "light green"))

(use-package ddskk
  :if (memq window-system '(mac ns x))
  :custom (skk-use-jisx0201-input-method t)
  :bind ("C-x j" . skk-mode))

;; flycheck
(use-package flycheck :hook prog-mode :diminish flycheck-mode :commands flycheck-add-mode flycheck-add-next-checker)
(use-package flycheck-color-mode-line :after flycheck :hook flycheck-mode)
(use-package flycheck-deno
  :after (flycheck lsp-mode)
  :config
  (lsp-diagnostics-mode 1)
  (flycheck-deno-setup)
  (flycheck-add-mode 'deno-lint 'web-mode)
  (flycheck-add-next-checker 'lsp 'deno-lint 'append))
(use-package flycheck-cfn :after (flycheck cfn-mode) :hook (cfn-mode . flycheck-cfn-setup))
(use-package flycheck-eglot
  :after (flycheck eglot)
  :config (global-flycheck-eglot-mode 1))

(use-package cov :custom (cov-coverage-mode t) :defer t)
(autoload 'ansi-color-apply-on-region "ansi-color" "Translates SGR control sequences into overlays or extents." t)
(use-package compile
  :config
  (add-hook 'compilation-filter-hook #'(lambda () (ansi-color-apply-on-region compilation-filter-start (point))))
  (defvar node-error-regexp "^[ ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$")
  (defvar vitest-error-regexp "^ ❯ \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$")
  (add-to-list 'compilation-error-regexp-alist-alist `(nodejs ,node-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'nodejs)
  (add-to-list 'compilation-error-regexp-alist-alist `(vitest ,vitest-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'vitest))

(use-package smartparens
  :diminish
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config))
(use-package rainbow-delimiters :hook prog-mode)

;;; Indent
(use-package whitespace
  :diminish global-whitespace-mode
  :custom
  (whitespace-display-mappings '((space-mark 12288 [9633]) (tab-mark 9 [187 9])))
  (whitespace-space-regexp "\\(　+\\)")
  (whitespace-style '(face tabs tab-mark spaces space-mark))
  :hook (before-save . delete-trailing-whitespace)
  :config (global-whitespace-mode 1))
(setq show-trailing-whitespace t)
(add-hook 'change-major-mode-after-body-hook
          #'(lambda ()
              (when (derived-mode-p 'term-mode)
                (setq show-trailing-whitespace nil))))
(add-hook 'minibuffer-setup-hook (lambda () (setq-local show-trailing-whitespace nil)))
(use-package highlight-indent-guides :diminish :hook prog-mode)
(use-package indent-tools :bind ("C-c >" . indent-tools-hydra/body))

;;; Emacs Lisp
(use-package auto-async-byte-compile :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode) :disabled)
(add-hook 'emacs-lisp-mode-hook #'(lambda () (local-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)))

;;; C#
(use-package csharp-mode :mode "\\.cs\\'")

;;; Obj-C
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
(use-package swift-mode :mode "\\.swift\\'")

;;; Python
(use-package ein :defer t)

;;; Ruby
(use-package ruby-mode
  :defer t
  :custom (ruby-insert-encoding-magic-comment nil)
  :config
  (add-hook 'ruby-mode-hook #'(lambda () (eglot-ensure))))
(use-package inf-ruby
  :after ruby-mode
  :hook
  ((ruby-mode . inf-ruby-minor-mode)
   (compilation-filter . inf-ruby-auto-enter-and-focus)))
(use-package robe
  :after ruby-mode
  :hook ruby-mode
  :config
  (add-hook 'ruby-mode-hook
            #'(lambda ()
                (make-local-variable 'company-backends)
                (push 'company-robe company-backends))))
(use-package rubocopfmt :after ruby-mode :hook ruby-mode)
(use-package projectile-rails
  :after (ruby-mode projectile)
  :bind (:map projectile-rails-mode-map
              ("C-c r" . hydra-projectile-rails/body)
              ("C-c f" . hydra-projectile-rails-find/body))
  :hook (find-file . rails-project-find-file-hook)
  :commands projectile-rails-root
  :config
  (projectile-rails-global-mode t)
  (defun rails-project-find-file-hook ()
    (interactive)
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
             )))))

(use-package rake :custom (rake-completion-system 'default) :defer t)
(use-package rspec-mode :custom (rspec-key-command-prefix (kbd "C-c s")) :defer t)
(use-package bundler :defer t)
(use-package coverage :defer t)

;;; HAML
(use-package haml-mode :defer t)
(use-package flymake-haml :after haml-mode :hook (haml-mode . flymake-haml-load))

;;; Go
(use-package go-mode
  :hook ((before-save . gofmt-before-save))
  :config
  (add-hook 'go-mode-hook
            #'(lambda () (subword-mode) (eglot-ensure))))
(use-package govet :defer t)
(use-package gotest :defer t)
(use-package go-impl :defer t)
(use-package go-gen-test :defer t)
(use-package go-tag
  :bind (:map go-mode-map
              ("C-c `" . go-tag-add)
              ("C-u C-c `" . go-tag-remove)))
(use-package go-eldoc :after (go-mode eldoc) :hook (go-mode . go-eldoc-setup))
(use-package go-projectile :after (go-mode projectile))

;;; Dart & Flutter
(use-package dart-mode
  :defer t
  :config
  (add-hook 'dart-mode-hook
            #'(lambda ()
                (subword-mode)
                (lsp))))
(use-package lsp-dart :after (lsp-mode dart-mode))
(use-package flutter :after (dart-mode))

;;; Gradle
(use-package groovy-mode :defer t)

;;; Web
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  (web-mode-auto-quote-style 3)
  :mode "\\.html?\\'" "\\.erb\\'" "\\.tsx\\'"
  :config
  (add-hook 'web-mode-hook
            #'(lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (tide-setup)
                  (lsp)
                  (flycheck-mode +1)
                  (setq flycheck-check-syntax-automatically '(save mode-enabled))
                  (tide-hl-identifier-mode +1)))))
(defun set-js-indent-level ()
  "Confirue indent level for js files."
  (setq-local js-indent-level 2))
(use-package vue-mode
  :defer t
  :config
  (add-hook 'vue-mode-hook
            #'(lambda ()
                (set-js-indent-level)
                (lsp))))
;; https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-539711083
(setq-default mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq-default mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(add-hook 'js-mode-hook #'set-js-indent-level)
(defun kill-jest-process-and-buffer ()
  "Kill jest process and buffer."
  (interactive)
  (delete-process (buffer-name (current-buffer)))
  (kill-buffer))
(use-package jest :bind (:map jest-mode-map ("q" . #'kill-jest-process-and-buffer)))
(use-package typescript-mode
  :defer t
  :config
  (add-hook 'typescript-mode-hook
            #'(lambda()
                (setq-default typescript-indent-level 2)
                (setq-local cov-lcov-file-name (concat (projectile-project-root) "lcov.info"))
                (cov-mode)
                (jest-minor-mode 1)
                (eglot-ensure))))
(use-package tide :defer t :custom (tide-sync-request-timeout 5))

(use-package npm)
(use-package deno-fmt :commands deno-fmt)
(use-package prisma-mode
  :straight (:host github :repo "pimeys/emacs-prisma-mode" :files ("*.el"))
  :hook (prisma-mode . lsp))

(use-package restclient :defer t)
(use-package graphql-mode :defer t)

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
(use-package docker :bind ("C-c C-d" . docker))
(if (featurep 'tramp-container)
    (require 'tramp-container)
  (use-package docker-tramp :defer t))
(use-package dockerfile-mode
  :defer t
  :config (add-hook 'dockerfile-mode-hook #'(lambda () (eglot-ensure))))

;;; asdf
(use-package asdf
  :straight (:host github :repo "tabfugnic/asdf.el" :files ("asdf.el"))
  :config (asdf-enable))

;;; config files
(use-package nginx-mode :mode "/nginx/sites-\\(?:available\\|enabled\\)/")
(use-package yaml-mode :mode "\\.yml\\'")
(use-package json-mode :defer t)
(use-package json-reformat :defer t)
(use-package cfn-mode :defer t)

(use-package restart-emacs :defer t)
;; ライブコーディング用設定
;; (set-face-attribute 'default nil :height 300)

(use-package lua-mode :defer t)

(provide 'init)
;;; init.el ends here
