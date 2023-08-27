;;; init --- Summary
;;; Commentary:

(require 'generic-x)
(require 'package)

;;; Code:
;;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(create-lockfiles nil)
 '(dabbrev-case-fold-search 'case-fold-search)
 '(dabbrev-case-replace nil)
 '(enable-recursive-minibuffers t)
 '(history-length 1000)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(gc-cons-threshold 16000000)
 '(recentf-auto-cleanup 'never)
 '(recentf-max-menu-items 1000)
 '(recentf-max-saved-items 1000)
 '(ring-bell-function 'ignore)
 '(scroll-margin 0)
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-package-always-ensure t)
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package)))
 `(temporary-file-directory ,(concat (getenv "HOME") "/.tmp")))
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
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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
(use-package which-key :diminish :hook (after-init . which-key-mode))

;;; ENV
(setenv "LANG" "ja_JP.UTF-8")
(if (eq system-type 'darwin)
    (setenv "TMPDIR" (concat (getenv "HOME") "/.tmp")))
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (when (eq system-type 'darwin)
    (exec-path-from-shell-initialize)))

(winner-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(global-font-lock-mode 1)
(when (eq system-type 'darwin)
  (add-to-list 'auth-sources 'macos-keychain-generic)
  (add-to-list 'auth-sources 'macos-keychain-internet))
(bind-key "RET" 'newline-and-indent)
(autoload 'winner-undo "winner" "Load winner-undo" t nil)
(bind-keys*
 ("C-z" . winner-undo)
 ("C-o" . other-window)
 ("C-h" . delete-backward-char)
 ("C-;" . completion-at-point))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'message-box 'message)
(when (eq (window-system) 'ns)
  (set-face-attribute 'default nil :family "Menlo" :height 180)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))
  (add-to-list 'face-font-rescale-alist '(".*Hiragino Kaku Gothic ProN.*" . 1.1)))
(when (eq (window-system) 'x)
  (set-face-attribute 'default nil :family "Inconsolata" :height 140)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Noto Sans CJK JP" :size 28)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))
(global-eldoc-mode 1)
(use-package eldoc-box
  :after (eldoc)
  :bind ("C-c h" . eldoc-box-help-at-point))
(use-package ace-window :bind (("C-x o" . ace-window)))

;; https://github.com/rainstormstudio/nerd-icons.el#installing-fonts
;; M-x nerd-icons-install-fonts
(use-package nerd-icons-completion
  :hook
  (after-init . nerd-icons-completion-mode)
  (marginalia-mode . nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-dired :diminish :hook (dired-mode . nerd-icons-dired-mode))

(use-package projectile
  :diminish
  :bind (("C-x p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :autoload projectile-project-root
  :config
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
  (marginalia-mode))
(use-package corfu :hook (after-init . global-corfu-mode))
(use-package cape
  :defer t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (with-eval-after-load 'minibuffer
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-to-list 'completion-at-point-functions #'cape-keyword)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
(use-package consult
  :bind (("C-x C-r" . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap isearch-forward] . consult-line)
         ("C-x C-b" . consult-buffer)
         ("C-x C-i" . consult-imenu)
         ("C-c g" . consult-git-grep)
         ("M-y" . consult-yank-pop))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))
(use-package consult-flycheck
  :after (consult flycheck)
  :bind (("C-c C-e" . consult-flycheck)
         ("C-c e" . consult-flycheck)))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ([remap describe-bindings] . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  (embark-prompter 'embark-completing-read-prompter)
  :init
  (add-hook 'which-key-mode-hook #'(lambda () (setq prefix-help-command #'embark-prefix-help-command)))
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Dired
(use-package dired-hide-dotfiles
  :diminish
  :bind (:map dired-mode-map (("." . dired-hide-dotfiles-mode)))
  :hook (dired-mode . dired-hide-dotfiles-mode))
(bind-key "C-x d" #'(lambda () (interactive) (find-file default-directory)))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (("C-M-;" . copilot-complete)
         ("TAB" . my/copilot-accept-completion)
         ("<backtab>" . copilot-next-completion))
  :custom
  (copilot-disable-predicates '((lambda () t)))
  (copilot-node-executable (concat (getenv "HOME") "/.asdf/installs/nodejs/lts-gallium/bin/node"))
  :commands copilot-accept-completion
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
  (lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-completion-mode)
  :autoload lsp-rename
  :init
  (defun lsp-rename-snake-to-camel ()
    "Rename symbol from snake_case to camelCase."
    (interactive)
    (lsp-rename (string-inflection-camelcase-function (thing-at-point 'symbol)))))
(use-package string-inflection :autoload string-inflection-camelcase-function :defer t)
(use-package lsp-ui :after lsp-mode :hook (lsp-mode . lsp-ui-mode))
(use-package dap-mode
  :hook
  (((go-mode ruby-mode typescript-mode) . dap-mode)
   ((go-mode ruby-mode typescript-mode) . dap-ui-mode))
  :bind (:map dap-mode-map (("C-c d" . dap-debug)))
  :autoload dap-register-debug-template
  :config
  (with-eval-after-load 'dap-hydra (add-hook 'dap-stopped-hook #'dap-hydra))
  (require 'dap-hydra)
  (with-eval-after-load 'go-mode (require 'dap-dlv-go))
  (with-eval-after-load 'ruby-mode (require 'dap-ruby))
  (with-eval-after-load 'typescript-mode
    (require 'dap-chrome)
    (require 'dap-node)))

;; git
(use-package magit
  :custom (magit-repository-directories (list (cons (concat (getenv "HOME") "/repos/") 1)))
  :bind (("C-x g" . magit-status)
         ([remap vc-dir] . magit-status)))
(use-package magit-delta :hook (magit-mode . magit-delta-mode))
(use-package forge :after magit :custom (forge-topic-list-limit '(50 . 0)))
(use-package emacsql-sqlite-module :if (version< emacs-version "29.0") :ensure (version< emacs-version "29.0") :defer t)
(use-package git-gutter :diminish
  :config
  (with-eval-after-load 'git-gutter-fringe
    (global-git-gutter-mode t)))
(use-package git-gutter-fringe :after git-gutter)
(use-package github-review :defer t)
(use-package code-review :defer t)
(use-package gist :defer t)
(use-package browse-at-remote :defer t)

;;; Terminal
(use-package shell-pop
  :custom
  (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" #'(lambda nil (ansi-term shell-pop-term-shell)))))
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
(use-package solarized-theme :config (load-theme 'solarized-light t))
(use-package doom-modeline :custom (doom-modeline-minor-modes t) :hook (after-init . doom-modeline-mode))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package expand-region :bind ("C-M-SPC" . er/expand-region))
(use-package vundo :bind ("C-x u" . vundo))

(use-package volatile-highlights :diminish :hook (after-init . volatile-highlights-mode))
(use-package beacon :diminish beacon-mode :hook (after-init . beacon-mode) :custom (beacon-color "light green"))

(if (locate-library "skk-autoloads")
    (bind-key "C-x j" 'skk-mode)
  (use-package ddskk
    :if (memq window-system '(mac ns x))
    :custom (skk-use-jisx0201-input-method t)
    :bind ("C-x j" . skk-mode)))
(use-package ddskk-posframe :diminish :config (ddskk-posframe-mode t))

;; flycheck
(use-package flycheck :hook (prog-mode . flycheck-mode) :diminish flycheck-mode :autoload flycheck-add-mode flycheck-add-next-checker)
(use-package flycheck-color-mode-line :after flycheck :hook (flycheck-mode . flycheck-color-mode-line-mode))
(use-package flycheck-deno
  :after (flycheck)
  :config
  (flycheck-deno-setup)
  (flycheck-add-mode 'deno-lint 'web-mode)
  (flycheck-add-next-checker 'eglot-check 'deno-lint 'append))
(use-package flycheck-cfn :after (flycheck cfn-mode) :hook (cfn-mode . flycheck-cfn-setup))
(use-package flycheck-eglot :after (flycheck eglot) :config (global-flycheck-eglot-mode 1))

(use-package cov :custom (cov-coverage-mode t) :defer t)
(autoload 'ansi-color-apply-on-region "ansi-color" "Translates SGR control sequences into overlays or extents." t)
(add-hook 'compilation-filter-hook #'(lambda () (ansi-color-apply-on-region compilation-filter-start (point))))
(with-eval-after-load 'compile
  (defvar node-error-regexp "^[ ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$")
  (defvar vitest-error-regexp "^ ❯ \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$")
  (add-to-list 'compilation-error-regexp-alist-alist `(nodejs ,node-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'nodejs)
  (add-to-list 'compilation-error-regexp-alist-alist `(vitest ,vitest-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'vitest))
(use-package fancy-compilation
  :after compile
  :custom (fancy-compilation-override-colors nil)
  :config (fancy-compilation-mode))

(use-package smartparens
  :diminish
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;;; Indent
(use-package whitespace
  :diminish global-whitespace-mode
  :custom
  (whitespace-display-mappings '((space-mark 12288 [9633]) (tab-mark 9 [187 9])))
  (whitespace-space-regexp "\\(　+\\)")
  (whitespace-style '(face tabs tab-mark spaces space-mark))
  (whitespace-global-modes '(not dired-mode))
  :config
  (global-whitespace-mode 1))
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'change-major-mode-after-body-hook
          #'(lambda ()
              (when (or (mapcar #'(lambda (mode) (derived-mode-p mode)) '(term-mode magit-popup-mode)))
                (setq-local show-trailing-whitespace nil))))
(add-hook 'minibuffer-setup-hook #'(lambda () (setq-local show-trailing-whitespace nil)))
(use-package highlight-indent-guides :diminish :if window-system :hook (prog-mode . highlight-indent-guides-mode))
(use-package indent-tools :bind ("C-c >" . indent-tools-hydra/body))

;;; Emacs Lisp
(use-package auto-async-byte-compile :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode) :disabled)
(use-package eros :hook (emacs-lisp-mode . eros-mode))

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
(if (eq system-type 'darwin)
    (with-eval-after-load 'find-file
      (defvar cc-search-directories)
      (nconc cc-search-directories '("/System/Library/Frameworks" "/Library/Frameworks"))))

;;; Swift
(use-package swift-mode :mode "\\.swift\\'")

;;; Python
(use-package ein :defer t)

;;; Ruby
(use-package ruby-mode :defer t :custom (ruby-insert-encoding-magic-comment nil))
(add-hook 'ruby-mode-hook #'eglot-ensure)
(use-package inf-ruby
  :after ruby-mode
  :hook
  ((ruby-mode . inf-ruby-minor-mode)
   (compilation-filter . inf-ruby-auto-enter-and-focus)))
(use-package robe :after ruby-mode :hook (ruby-mode . robe-mode))
(use-package rubocopfmt :after ruby-mode :hook (ruby-mode . rubocopfmt-mode))
(use-package projectile-rails
  :after (ruby-mode projectile)
  :bind (:map projectile-rails-mode-map
              ("C-c r" . hydra-projectile-rails/body)
              ("C-c f" . hydra-projectile-rails-find/body))
  :hook (find-file . rails-project-find-file-hook)
  :autoload projectile-rails-root
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
(use-package go-mode :hook ((before-save . gofmt-before-save)))
(add-hook 'go-mode-hook #'(lambda () (subword-mode) (eglot-ensure)))
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
(use-package go-playground
  :defer t
  :custom (go-playground-init-command "go mod init snippet")
  :init
  (add-hook 'go-playground-mode-hook #'(lambda () (subword-mode) (eglot-ensure))))
(use-package flycheck-golangci-lint :hook (go-mode . flycheck-golangci-lint-setup))
(use-package go-fill-struct :defer t)

;;; Dart & Flutter
(use-package dart-mode
  :defer t
  :init
  (add-hook 'dart-mode-hook
            #'(lambda ()
                (subword-mode)
                (eglot-ensure))))
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
  :mode "\\.html?\\'" "\\.erb\\'")

(defun kill-jest-process-and-buffer ()
  "Kill jest process and buffer."
  (interactive)
  (delete-process (buffer-name (current-buffer)))
  (kill-buffer))
(use-package jest :bind (:map jest-mode-map ("q" . kill-jest-process-and-buffer)))
(use-package typescript-mode
  :custom (typescript-indent-level 2)
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))
(add-hook 'tsx-ts-mode-hook #'(lambda () (eglot-ensure)))
(add-hook 'typescript-mode-hook
          #'(lambda()
              (cov-mode)
              (setq-local cov-lcov-file-name (concat (projectile-project-root) "lcov.info"))
              (jest-minor-mode 1)
              (eglot-ensure)))
(use-package npm)
(use-package deno-fmt :commands deno-fmt :defer t)
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

(use-package docker
  :custom
  (docker-compose-command (or (and (eq system-type 'gnu/linux) "docker compose")  "docker-compose"))
  :bind ("C-c C-d" . docker))
(use-package dockerfile-mode :defer t)
(add-hook 'dockerfile-mode-hook #'eglot-ensure)

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
