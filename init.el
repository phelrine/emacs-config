;;; init --- Summary
;;; Commentary:

;;; Code:

(require 'generic-x)

;;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(create-lockfiles nil)
 '(enable-recursive-minibuffers t)
 '(gc-cons-threshold 16000000)
 '(history-length 1000)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(ring-bell-function 'ignore)
 '(scroll-margin 0)
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-package-always-ensure t)
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:background "black" :foreground "LightYellow" :inverse-video t)))))

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
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
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-envs '("UID" "GID")))

(winner-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
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
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN")))
(when (eq (window-system) 'x)
  (set-face-attribute 'default nil :family "Inconsolata" :height 140)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Noto Sans CJK JP" :size 28)))

(defvar local-lisp-load-path "~/.emacs.d/lisp")

(use-package recentf
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-menu-items 1000)
  (recentf-max-saved-items 1000)
  :config
  (recentf-mode 1))

(use-package auth-source-kwallet
  :straight (:host github :repo "phelrine/auth-source-kwallet" :files ("auth-source-kwallet.el"))
  :config
  (if (executable-find auth-source-kwallet-executable)
      (auth-source-kwallet-enable)))
(use-package auth-source-ghcli
  :load-path local-lisp-load-path
  :autoload auth-source-ghcli-enable
  :init
  (auth-source-ghcli-enable))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))
(global-eldoc-mode 1)
(use-package eldoc-box :bind ("C-c h" . eldoc-box-help-at-point))
(use-package ace-window :bind (("C-x o" . ace-window)))

;; M-x unicode-fonts-setup
(use-package unicode-fonts :config (unicode-fonts-setup))
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
(use-package corfu
  :init (global-corfu-mode)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode))))
(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p :" . cape-emoji))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package pcmpl-args)
(use-package pcmpl-git)

(use-package kind-icon
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
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (dabbrev-case-replace nil))
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
  (add-hook 'which-key-mode-hook (lambda () (setq prefix-help-command #'embark-prefix-help-command)))
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult :after (consult embark) :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Dired
(use-package dired-hide-dotfiles
  :diminish
  :bind (:map dired-mode-map (("." . dired-hide-dotfiles-mode)))
  :hook (dired-mode . dired-hide-dotfiles-mode))
(bind-key "C-x d" (lambda () (interactive) (find-file default-directory)))

(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-M-;" . copilot-complete)
         ("TAB" . my/copilot-accept-completion)
         ("<backtab>" . copilot-next-completion))
  :custom
  (copilot-disable-predicates '((lambda () t)))
  (copilot-node-executable (concat (getenv "HOME") "/.asdf/installs/nodejs/20.5.1/bin/node"))
  :commands copilot-accept-completion
  :init
  (defun my/copilot-accept-completion ()
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command))))

(use-package lsp-mode
  :diminish
  :custom
  (lsp-auto-guess-root t)
  (lsp-solargraph-use-bundler t)
  (lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-completion-mode)
  :autoload lsp-rename lsp-format-buffer)
(use-package lsp-ui :hook (lsp-mode . lsp-ui-mode))
(use-package string-inflection
  :after lsp-mode
  :autoload string-inflection-camelcase-function
  :init
  (defun lsp-rename-snake-to-camel ()
    "Rename symbol from snake_case to camelCase."
    (interactive)
    (lsp-rename (string-inflection-camelcase-function (thing-at-point 'symbol)))))

;;; DAP
(use-package dape :defer t)

;;; git
(use-package magit
  :custom (magit-repository-directories (list (cons (concat (getenv "HOME") "/repos/") 1)))
  :bind (("C-x g" . magit-status)
         ([remap vc-dir] . magit-status)))
(use-package difftastic
  :after magit
  :config
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
     ("S" "Difftastic show" difftastic-magit-show)]))
(use-package forge :after magit :custom (forge-topic-list-limit '(50 . 0)))
(use-package git-gutter :diminish)
(use-package git-gutter-fringe
  :after git-gutter
  :config (global-git-gutter-mode t))
(use-package code-review
  :straight (:host github :repo "phelrine/code-review" :branch "fix/closql-update")
  :defer t)
(use-package igist :custom (igist-current-user-name "phelrine") :commands igist-dispatch)
(use-package browse-at-remote :commands browse-at-remote)

;;; SQL
(use-package sql-ts-mode
  :mode ("\\.sql\\'")
  :load-path local-lisp-load-path
  :hook ((sql-ts-mode . (lambda ()
                          (lsp)
                          (indent-tabs-mode t))))
  :config
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'sql-ts-mode)
                (lsp-format-buffer)))))

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

(use-package open-junk-file :commands open-junk-file)
(use-package solarized-theme :config (load-theme 'solarized-light t))
(use-package doom-modeline :custom (doom-modeline-minor-modes t) :hook (after-init . doom-modeline-mode))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
(use-package ddskk-posframe :after ddskk :diminish :config (ddskk-posframe-mode t))

;; flycheck
(use-package flycheck :hook (prog-mode . flycheck-mode) :diminish flycheck-mode :autoload flycheck-add-mode flycheck-add-next-checker)
(use-package flycheck-color-mode-line :hook (flycheck-mode . flycheck-color-mode-line-mode))
(use-package flycheck-deno
  :after flycheck
  :config
  (flycheck-deno-setup)
  (flycheck-add-mode 'deno-lint 'web-mode)
  (flycheck-add-next-checker 'eglot-check 'deno-lint 'append))
(use-package flycheck-cfn :hook (cfn-mode . flycheck-cfn-setup))
(use-package flycheck-eglot :after (flycheck eglot) :config (global-flycheck-eglot-mode 1))

(use-package cov :custom (cov-coverage-mode t) :commands cov-mode)
(use-package fancy-compilation
  :hook (compilation-mode . fancy-compilation-mode)
  :custom (fancy-compilation-override-colors nil))

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
          (lambda ()
            (when (or (mapcar (lambda (mode) (derived-mode-p mode)) '(term-mode magit-popup-mode)))
              (setq-local show-trailing-whitespace nil))))
(add-hook 'minibuffer-setup-hook (lambda () (setq-local show-trailing-whitespace nil)))
(use-package highlight-indent-guides :diminish :if window-system :hook (prog-mode . highlight-indent-guides-mode))
(use-package indent-tools :bind ("C-c >" . indent-tools-hydra/body))

;;; treesit
(with-eval-after-load 'treesit
  (defvar treesit-font-lock-level)
  (defvar treesit-language-source-alist)
  (setq treesit-font-lock-level 4)
  (add-to-list 'treesit-language-source-alist '(prisma "https://github.com/victorhqc/tree-sitter-prisma")))
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :commands global-treesit-auto-mode treesit-auto-add-to-auto-mode-alist
  :init
  (global-treesit-auto-mode 1)
  :config
  (delete 'yaml treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; ChatGPT
(defun pick-openai-key ()
  "Pick the OpenAI api key from auth source."
  (auth-source-pick-first-password :host "api.openai.com"))
(use-package chatgpt-shell
  :custom
  ((chatgpt-shell-openai-key (pick-openai-key))
   (dall-e-shell-openai-key (pick-openai-key))))

;;; Emacs Lisp
(add-hook 'emacs-lisp-mode-map-hook
          (lambda ()
            (add-to-list 'completion-at-point-functions #'cape-file)
            (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
            (add-to-list 'completion-at-point-functions #'cape-elisp-block)))
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
(use-package swift-mode :defer t)

;;; Python
(use-package ein :defer t)

;;; Ruby
(use-package ruby-mode
  :custom (ruby-insert-encoding-magic-comment nil)
  :hook (ruby-mode . eglot-ensure))
(use-package inf-ruby
  :hook
  ((ruby-mode . inf-ruby-minor-mode)
   (compilation-filter . inf-ruby-auto-enter-and-focus)))
(use-package robe :hook (ruby-mode . robe-mode))
(use-package rubocopfmt :hook (ruby-mode . rubocopfmt-mode))
(use-package projectile-rails
  :after (ruby-mode projectile)
  :bind (:map projectile-rails-mode-map
              ("C-c r" . hydra-projectile-rails/body)
              ("C-c f" . hydra-projectile-rails-find/body))
  ;; :hook (find-file . rails-project-find-file-hook)
  :autoload projectile-rails-root
  :config
  (projectile-rails-global-mode t))
(use-package rake :custom (rake-completion-system 'default) :defer t)
(use-package rspec-mode :custom (rspec-key-command-prefix (kbd "C-c s")) :defer t)
(use-package bundler :defer t)
(use-package coverage :defer t)

;;; HAML
(use-package haml-mode :defer t)
(use-package flymake-haml :hook (haml-mode . flymake-haml-load))

;;; Go
(use-package go-mode :hook ((before-save . gofmt-before-save)))
(add-hook 'go-mode-hook
          (lambda ()
            (subword-mode)
            (eglot-ensure)
            (setq-default go-ts-mode-indent-offset 4)))
(setq go-ts-mode-hook go-mode-hook)
(use-package govet :commands govet)
(use-package gotest :defer t)
(use-package gotest-dape :load-path local-lisp-load-path :defer t)
(use-package go-gen-test :defer t)
(use-package go-impl :commands go-impl)
(use-package go-tag
  :bind (:map go-mode-map
              ("C-c `" . go-tag-add)
              ("C-u C-c `" . go-tag-remove)))
(use-package go-eldoc :after (go-mode eldoc) :hook (go-mode . go-eldoc-setup))
(use-package go-projectile :after (go-mode projectile))
(use-package go-playground :custom (go-playground-init-command "go mod init snippet") :commands go-playground)
(use-package flycheck-golangci-lint :hook (go-mode . flycheck-golangci-lint-setup))
(use-package go-fill-struct :commands go-fill-struct)

;;; Dart & Flutter
(use-package dart-mode :hook (dart-mode . (lambda () (subword-mode) (eglot-ensure))) :commands dart-mode)
(use-package flutter :after dart-mode)

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
(use-package typescript-mode :custom (typescript-indent-level 2))
(add-hook 'tsx-ts-mode-hook (lambda () (eglot-ensure)))
(add-hook 'typescript-ts-mode-hook
          (lambda()
            (cov-mode)
            (setq-local cov-lcov-file-name (concat (projectile-project-root) "lcov.info"))
            (jest-minor-mode 1)
            (eglot-ensure)))
(with-eval-after-load 'compile
  (defvar node-error-regexp "^[ ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$")
  (add-to-list 'compilation-error-regexp-alist-alist `(nodejs ,node-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'nodejs))
(use-package npm)
(use-package deno-fmt :defer t)
(use-package prisma-ts-mode
  :mode (("\\.prisma\\'" . prisma-ts-mode))
  :hook (prisma-ts-mode . eglot-ensure)
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(prisma-ts-mode . ("prisma-language-server" "--stdio")))))
(use-package restclient :commands restclient-mode
  :config
  (use-package restclient-jq))
(use-package graphql-mode :commands graphql-mode)

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
(use-package dockerfile-mode :hook (dockerfile-mode . eglot-ensure))
(use-package docker-compose-mode :defer t)

;;; Markdown
(use-package maple-preview
  :straight (:host github :repo "honmaple/emacs-maple-preview" :files ("*.el" "index.html" "static"))
  :defer t)

;;; asdf
(use-package asdf
  :straight (:host github :repo "tabfugnic/asdf.el" :files ("asdf.el"))
  :config (asdf-enable))

;;; AWS
(use-package aws-switch-profile
  :defer t
  :straight (:host github :repo "phelrine/aws-switch-profile.el" :files ("aws-switch-profile.el")))

(use-package aws-secretsmanager
  :commands aws-secretsmanager-show-secrets-list
  :straight (:host github :repo "phelrine/aws-secretsmanager.el" :files ("aws-secretsmanager.el")))

;;; config files
(use-package nginx-mode :mode "/nginx/sites-\\(?:available\\|enabled\\)/")
(use-package json-mode :defer t)
(use-package json-reformat :commands json-reformat-region)
(use-package cfn-mode :defer t)
(use-package lua-mode :defer t)
(use-package yaml-mode :defer t)
(use-package restart-emacs :commands restart-emacs)

(provide 'init)
;;; init.el ends here
