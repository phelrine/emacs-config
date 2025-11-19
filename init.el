;;; init --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;; ========================================
;;; STARTUP PROFILING
;;; ========================================

;; Load startup profiler before anything else
;; Uncomment the following lines to enable startup profiling:
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'startup-profiler)
;; (startup-profiler-enable)
;; Then run M-x startup-profiler-report after startup to see the results

(require 'generic-x)

;;; ========================================
;;; BASIC CONFIGURATION
;;; ========================================

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
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:background "black" :foreground "LightYellow" :inverse-video t)))))

;;; Basic Emacs settings
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(winner-mode 1)
(global-hl-line-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(bind-key "RET" 'newline-and-indent)
(autoload 'winner-undo "winner" "Load winner-undo" t nil)
(bind-keys*
 ("C-z" . winner-undo)
 ("C-h" . delete-backward-char)
 ("C-;" . completion-at-point))
(bind-key "C-o" 'other-window)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'message-box 'message)

;;; Font settings
(when (eq (window-system) 'ns)
  (set-face-attribute 'default nil :family "Menlo" :height 180)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN")))
(when (eq (window-system) 'x)
  (set-face-attribute 'default nil :family "HackGen" :height 180))

;;; Local lisp path
;; Sync load-path with flycheck-emacs-lisp-load-path
(with-eval-after-load 'flycheck
  ;; Initialize flycheck-emacs-lisp-load-path with current load-path
  (when (boundp 'flycheck-emacs-lisp-load-path)
    (setq flycheck-emacs-lisp-load-path load-path))
  ;; Advise add-to-list to sync with flycheck when modifying load-path
  (advice-add 'add-to-list :after
    (lambda (list-var element &optional append compare-fn)
      "Sync flycheck-emacs-lisp-load-path when load-path is modified."
      (when (and (eq list-var 'load-path)
                 (boundp 'flycheck-emacs-lisp-load-path))
        (add-to-list 'flycheck-emacs-lisp-load-path element append compare-fn)))))

(defvar local-lisp-load-path "~/.emacs.d/lisp")
(add-to-list 'load-path local-lisp-load-path)

;; Load version manager diagnostic tools (supports asdf and mise)
;; Provides M-x check-version-manager-environment, version-manager-quick-fix, etc.
(require 'check-version-manager)

(custom-set-variables
 '(recentf-max-menu-items 1000)
 '(recentf-max-saved-items 1000))
(recentf-mode 1)

;;; ========================================
;;; PACKAGE MANAGEMENT
;;; ========================================

(defvar straight-use-package-by-default)

(eval-when-compile (require 'use-package))
(setq use-package-verbose t)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  ;; Suppress unused variable warning
  (ignore bootstrap-version)
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(use-package straight
  :custom (straight-use-package-by-default t)
  :commands straight-use-package)
(straight-use-package '(org :type built-in))
(straight-use-package '(dired :type built-in))
(straight-use-package '(project :type built-in))

(use-package diminish :commands diminish)
(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :commands auto-package-update-maybe
  :config
  (auto-package-update-maybe))
(use-package which-key :diminish :hook (after-init . which-key-mode))

;;; ========================================
;;; ENVIRONMENT & AUTHENTICATION
;;; ========================================

;;; ENV
(setenv "LANG" "ja_JP.UTF-8")
(if (eq system-type 'darwin)
    (setenv "TMPDIR" (concat (getenv "HOME") "/.tmp")))
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "ASDF_DIR" "ASDF_DATA_DIR"))
  (exec-path-from-shell-arguments '("-l"))  ; Use login shell to read .zshenv
  (exec-path-from-shell-check-startup-files nil)
  :commands
  exec-path-from-shell-initialize
  exec-path-from-shell-copy-envs
  :init
  ;; Initialize on both macOS and Linux to ensure asdf and other tools work properly
  (unless (eq system-type 'windows-nt)
    (exec-path-from-shell-initialize))
  :config
  (exec-path-from-shell-copy-envs '("UID" "GID")))

;;; auth-source
(require 'auth-source)
(when (eq system-type 'darwin)
  (add-to-list 'auth-sources 'macos-keychain-generic)
  (add-to-list 'auth-sources 'macos-keychain-internet))
(use-package auth-source-kwallet
  :straight (:host github :repo "phelrine/auth-source-kwallet" :files ("auth-source-kwallet.el"))
  :config
  (if (executable-find auth-source-kwallet-executable)
      (auth-source-kwallet-enable)))

(use-package auth-source-ghcli
  :straight nil
  :load-path local-lisp-load-path
  :config
  (auth-source-ghcli-enable))

;;; ========================================
;;; UI & APPEARANCE
;;; ========================================

(use-package unicode-fonts
  :defer 2
  :commands unicode-fonts-setup
  :config (unicode-fonts-setup))
;; https://github.com/rainstormstudio/nerd-icons.el#installing-fonts
;; M-x nerd-icons-install-fonts
(use-package nerd-icons-completion
  :hook
  (after-init . nerd-icons-completion-mode)
  (marginalia-mode . nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-dired :diminish :hook (dired-mode . nerd-icons-dired-mode))

(use-package solarized-theme :config (load-theme 'solarized-light t))
(use-package doom-modeline
  :defer 0.5
  :commands doom-modeline-mode
  :custom (doom-modeline-minor-modes t)
  :config (doom-modeline-mode 1))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package volatile-highlights :diminish :hook (after-init . volatile-highlights-mode))
(use-package beacon :diminish beacon-mode :hook (after-init . beacon-mode) :custom (beacon-color "light green"))

(use-package popwin
  :commands popwin-mode
  :config
  (popwin-mode 1)
  (nconc popwin:special-display-config
         '((" *auto-async-byte-compile*" :noselect t)
           ("*Warnings*" :noselect t)
           ("*Rubocopfmt Errors*" :noselect t))))

;;; ========================================
;;; COMPLETION, SEARCH & NAVIGATION
;;; ========================================

(use-package projectile
  :diminish
  :bind (("C-x p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :commands
  projectile-project-root
  :config
  ;; FLUTTER
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package vertico :commands vertico-mode :init (vertico-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :commands marginalia-mode
  ;; The :init configuration is always executed (Not lazy!)
  :init
  (marginalia-mode))
(use-package corfu
  :commands
  global-corfu-mode
  corfu-mode
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
  :commands cape-history cape-elisp-symbol cape-elisp-block
  :init
  (dolist (func '(cape-dabbrev cape-file cape-elisp-symbol cape-elisp-block cape-keyword))
    (add-to-list 'completion-at-point-functions func))
  (add-hook 'emacs-lisp-mode-map-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-file)
              (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
              (add-to-list 'completion-at-point-functions #'cape-elisp-block))))

(use-package kind-icon
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :commands kind-icon-margin-formatter
  :config
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

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
  :commands embark-prefix-help-command
  :init
  (add-hook 'which-key-mode-hook (lambda () (setq prefix-help-command #'embark-prefix-help-command)))
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult :after (consult embark))

;;; ========================================
;;; EDITOR FEATURES
;;; ========================================

;;; View mode - read-only by default
(use-package view
  :straight (:type built-in)
  :init
  ;; Link read-only-mode and view-mode
  (setq view-read-only t)

  ;; Open files in read-only-mode by default
  (add-hook 'find-file-hook
            (lambda ()
              ;; Skip special buffers and modes where view-mode doesn't make sense
              (unless (or (derived-mode-p 'dired-mode)
                          (derived-mode-p 'special-mode)
                          (derived-mode-p 'term-mode)
                          (derived-mode-p 'vterm-mode)
                          (derived-mode-p 'eat-mode)
                          (derived-mode-p 'magit-mode)
                          (string-match-p "\\*.*\\*" (buffer-name))
                          buffer-read-only
                          ;; Skip view-mode for open-junk-file buffers
                          (bound-and-true-p open-junk-file-buffer))
                ;; When read-only-mode is enabled, view-mode is also automatically
                ;; enabled due to view-read-only=t
                (read-only-mode 1)))))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))
(diminish 'eldoc-mode)
(global-eldoc-mode 1)
(use-package eldoc-box :bind ("C-c h" . eldoc-box-help-at-point))
(use-package ace-window :bind (("C-x o" . ace-window)))

(use-package yasnippet :diminish yas-minor-mode :hook (prog-mode . yas-minor-mode))

(use-package open-junk-file
  :commands open-junk-file
  :init
  ;; Mark buffers opened via open-junk-file to skip view-mode
  (defvar-local open-junk-file-buffer nil
    "Non-nil if this buffer was opened via open-junk-file.")

  (defun open-junk-file-with-marker (&rest args)
    "Advice to mark buffers opened via open-junk-file."
    (setq-local open-junk-file-buffer t))

  (advice-add 'open-junk-file :after #'open-junk-file-with-marker))

(use-package expreg :bind ("C-M-SPC" . expreg-expand))
(use-package vundo :bind ("C-x u" . vundo))

(use-package smartparens
  :diminish
  :defer 1
  :hook
  (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;;; Input Method
(if (locate-library "skk-autoloads")
    (bind-key "C-x j" 'skk-mode)
  (use-package ddskk
    :if (memq window-system '(mac ns x))
    :custom (skk-use-jisx0201-input-method t)
    :bind ("C-x j" . skk-mode)))
(use-package ddskk-posframe :after ddskk :diminish :commands ddskk-posframe-mode :config (ddskk-posframe-mode t))

;;; ========================================
;;; TERMINAL & SHELL
;;; ========================================

(use-package vterm
  :straight t
  :defer t
  :config
  (require 'posframe-ime-vterm)
  (posframe-ime-vterm-enable))

(use-package eat
  :defer t
  :straight (:type git :host codeberg :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (require 'posframe-ime-eat)
  (posframe-ime-eat-enable))

;;; ========================================
;;; AI ASSISTANCE
;;; ========================================

;;; API key helpers
(defun pick-openai-key ()
  "Pick the OpenAI api key from auth source."
  (auth-source-pick-first-password :host "api.openai.com"))

(defun pick-emigo-api-key ()
  "Pick the Emigo API key from auth source."
  (auth-source-pick-first-password :host "openrouter.ai"))

(defalias 'pick-anthropic-key 'pick-emigo-api-key)

;;; Copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :defer t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
         ("C-M-;" . copilot-complete)
         ("TAB" . my/copilot-accept-completion)
         ("C-<tab>" . copilot-next-completion))
  :custom
  ;; Disable automatic completion - only trigger manually with C-M-;
  (copilot-disable-predicates '((lambda () t)))
  ;; Enable logging to debug server crashes
  ;; Check *copilot stderr* and *copilot events* buffers for errors
  (copilot-log-max 1000)
  :commands copilot-accept-completion
  :config
  (defun my/copilot-accept-completion ()
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command))))

;;; Posframe IME Input - IME-friendly input dialog
;; General-purpose posframe input
(require 'posframe-ime-input)
;; Enable SKK integration
(require 'posframe-ime-skk)
(posframe-ime-skk-enable)

;;; Claude Code IDE
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :defer t
  :commands claude-code-ide-menu
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  ;; Custom configuration loaded from separate file
  (require 'claude-code-ide-config)
  (claude-code-ide-config-setup))

;;; Agent Shell - AI Agent Integration
;; Dependencies
(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker")
  :custom
  (shell-maker-prompt-before-killing-buffer nil))

(use-package acp
  :straight (:type git :host github :repo "xenodium/acp.el"))

;; Agent Shell with OpenAI Codex support
(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell")
  :defer t
  :commands (agent-shell-openai-start-codex
             agent-shell)
  :hook (agent-shell-mode . agent-shell-completion-mode)
  :bind ("C-c C-\"" . agent-shell-openai-start-codex)
  :config
  (require 'agent-shell-openai)

  ;; OpenAI API authentication
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication
         :api-key #'pick-openai-key))

  ;; Codex command configuration
  (setq agent-shell-openai-codex-command '("codex-acp")))

;;; ChatGPT & GPT Tools
(use-package chatgpt-shell
  :defer t
  :custom
  ((chatgpt-shell-openai-key (pick-openai-key))
   (dall-e-shell-openai-key (pick-openai-key))))
(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :defer t
  :commands copilot-chat-insert-commit-message
  :config
  (require 'copilot-chat-shell-maker)
  (copilot-chat-shell-maker-init))
(use-package gptel :defer t :straight (:host github :repo "karthink/gptel" :files ("*.el")))
(use-package mcp-hub
  :defer 2
  :straight (:host github :repo "lizqwerscott/mcp.el" :files ("*.el"))
  :preface
  (require 'mcp-config)
  (setq mcp-hub-servers (mcp-config-resolve-servers))
  :init
  (run-with-idle-timer 2 nil #'mcp-hub-start-all-server)
  :config
  (defun gptel-mcp-register-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (apply #'gptel-make-tool
                         tool))
              tools)))
  (defun gptel-mcp-use-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (let ((path (list (plist-get tool :category)
                                    (plist-get tool :name))))
                    (push (gptel-get-tool path)
                          gptel-tools)))
              tools)))
  (with-eval-after-load 'gptel
    (gptel-mcp-register-tool)
    (gptel-mcp-use-tool)))

(use-package emigo
  :defer 3
  :straight (:host github :repo "MatthewZMD/emigo" :files (:defaults "*.py" "*.el"))
  :config
  (emigo-enable) ;; Starts the background process automatically
  :custom
  (emigo-model "openrouter/anthropic/claude-3.7-sonnet")
  (emigo-base-url "https://openrouter.ai/api/v1")
  (emigo-api-key (pick-emigo-api-key)))

;;; ========================================
;;; LSP & DEVELOPMENT TOOLS
;;; ========================================

(use-package lsp-mode
  :diminish
  :defer t
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-ruby-lsp-use-bundler t)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-completion-mode)
  ((tsx-ts-mode typescript-ts-mode js-ts-mode ruby-ts-mode) . lsp)
  :autoload lsp-rename
  :init
  (setq read-process-output-max (* 1024 1024))
  :config
  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '("[/\\\\]vendor\\'" "[/\\\\]\\.bundle\\'")))
  (require 'lsp-ruby-lsp)
  (custom-set-variables
   '(lsp-disabled-clients '((tsx-ts-mode . graphql-lsp) (js-ts-mode . graphql-lsp) (typescript-ts-mode . graphql-lsp) (ruby-ts-mode . rubocop-ls)))))
(use-package lsp-treemacs :defer t)
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-show-with-cursor t)
  :config
  (require 'lsp-graphql)
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)))
(use-package string-inflection
  :after lsp-mode
  :autoload string-inflection-camelcase-function
  :init
  (with-eval-after-load 'lsp-mode
    (defun lsp-rename-snake-to-camel ()
      "Rename symbol from snake_case to camelCase."
      (interactive)
      (lsp-rename (string-inflection-camelcase-function (thing-at-point 'symbol))))))

;;; DAP
(use-package dape :defer t)

;;; flycheck
(use-package flycheck :hook (prog-mode . flycheck-mode) :diminish flycheck-mode :autoload flycheck-add-mode flycheck-add-next-checker)
(use-package flycheck-color-mode-line :hook (flycheck-mode . flycheck-color-mode-line-mode))
;; (use-package flycheck-deno
;;   :after flycheck
;;   :config
;;   (flycheck-deno-setup)
;;   (flycheck-add-mode 'deno-lint 'web-mode)
;;   (flycheck-add-next-checker 'eglot-check 'deno-lint 'append))
(use-package flycheck-cfn :hook (cfn-mode . flycheck-cfn-setup))
(use-package flycheck-eglot
  :after (flycheck eglot)
  :commands global-flycheck-eglot-mode
  :config (global-flycheck-eglot-mode 1))

(use-package cov :custom (cov-coverage-mode t) :commands cov-mode)
(use-package fancy-compilation
  :hook (compilation-mode . fancy-compilation-mode)
  :custom (fancy-compilation-override-colors nil))

;;; ========================================
;;; GIT & VERSION CONTROL
;;; ========================================

(use-package magit
  :custom (magit-repository-directories (list (cons (concat (getenv "HOME") "/repos/") 1)))
  :bind (("C-x g" . magit-status)
         ([remap vc-dir] . magit-status)))
(use-package difftastic
  :after magit
  :disabled t
  :config
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
     ("S" "Difftastic show" difftastic-magit-show)]))
(use-package forge :after magit :custom (forge-topic-list-limit '(50 . 0)))
(use-package git-gutter :diminish :defer 1)
(use-package git-gutter-fringe
  :defer 1
  :config
  (when (fboundp 'global-git-gutter-mode)
    (global-git-gutter-mode t)))
(use-package code-review
  :straight (:host github :repo "phelrine/code-review" :branch "fix/closql-update")
  :defer t)
(use-package igist :custom (igist-current-user-name "phelrine") :commands igist-dispatch)
(use-package browse-at-remote :commands browse-at-remote)

;;; ========================================
;;; FILE MANAGEMENT
;;; ========================================

;;; Dired
(use-package dired
  :bind
  (:map dired-mode-map ("." . dired-omit-mode))
  :custom
  (dired-omit-files (rx (seq bol ".")))
  :hook
  (dired-mode . dired-omit-mode)
  :init
  (with-eval-after-load 'dired (require 'dired-x)))

;;; treesit
(with-eval-after-load 'treesit
  (eval-when-compile (require 'treesit))
  (custom-set-variables '(treesit-font-lock-level 4))
  (add-to-list 'treesit-language-source-alist '(prisma "https://github.com/victorhqc/tree-sitter-prisma")))
(use-package treesit-auto
  :commands (global-treesit-auto-mode treesit-auto-add-to-auto-mode-alist)
  :custom (treesit-auto-install t)
  :init
  (global-treesit-auto-mode 1)
  :config
  ;; Exclude bash and yaml from treesit-auto
  ;; bash: ABI version mismatch issues with tree-sitter-bash
  ;; yaml: user preference for standard yaml-mode
  (delete 'bash treesit-auto-langs)
  (delete 'yaml treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; ========================================
;;; INDENTATION & WHITESPACE
;;; ========================================

;;; Indent
(custom-set-variables
 '(whitespace-display-mappings '((space-mark 12288 [9633]) (tab-mark 9 [187 9])))
 '(whitespace-space-regexp "\\(　+\\)")
 '(whitespace-style '(face tabs tab-mark spaces space-mark))
 '(whitespace-global-modes '(not dired-mode)))
(diminish 'global-whitespace-mode)
(global-whitespace-mode 1)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'change-major-mode-after-body-hook
          (lambda ()
            (when (or (derived-mode-p 'term-mode)
                      (derived-mode-p 'magit-popup-mode)
                      (derived-mode-p 'eat-mode)
                      (and (fboundp 'vterm-mode)
                           (derived-mode-p 'vterm-mode)))
              (setq-local show-trailing-whitespace nil))))
(add-hook 'minibuffer-setup-hook (lambda () (setq-local show-trailing-whitespace nil)))
(use-package highlight-indent-guides :diminish :if window-system :hook (prog-mode . highlight-indent-guides-mode))
(use-package indent-tools :bind ("C-c >" . indent-tools-hydra/body))

;;; ========================================
;;; ORG MODE
;;; ========================================

(use-package org
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :init
  (setq org-directory "~/Dropbox/org")
  :config
  (defvar org-todo-file (concat org-directory "/todo.org")
    "Path to the org-mode TODO file.")
  (defvar org-query-file (concat org-directory "/query.org")
    "Path to the org-mode query file.")
  (setq org-agenda-files `(,org-todo-file))
  (customize-set-variable 'org-capture-templates
        '(("t" "TODO" entry (file+headline org-todo-file "Tasks")
           "** TODO %? \n" :prepend t)
          ("s" "SQL" entry (file+headline org-query-file "Queries")
           "** %?%T\n#+name\n#+begin_src sql\n\n#+end_src\n" :prepend t)))
  (use-package ob-typescript :defer t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typescript . t)
     (sql . t))))

(defun run-local-vars-mode-hook ()
  "Run `major-mode' hook after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))
(add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)

;;; ========================================
;;; LANGUAGE SUPPORT
;;; ========================================

;;; Emacs Lisp
(use-package auto-async-byte-compile :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode) :disabled)
(use-package eros :hook (emacs-lisp-mode . eros-mode))

;;; Swift
(use-package swift-mode :defer t)

;;; Ruby
(use-package inf-ruby
  :hook
  ((ruby-ts-mode . inf-ruby-minor-mode)
   (compilation-filter . inf-ruby-auto-enter-and-focus)))
(use-package robe :hook (ruby-ts-mode . robe-mode))
(use-package rubocopfmt :hook (ruby-ts-mode . rubocopfmt-mode))
(use-package projectile-rails
  :after (ruby-ts-mode projectile)
  :definitions projectile-rails-mode-map
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
(with-eval-after-load 'go-ts-mode
  (custom-set-variables '(go-ts-mode-indent-offset tab-width))
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (subword-mode)
              (lsp))))
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#configuring-project-for-go-modules-in-emacs
(defun project-find-go-module (dir)
  "Search for go.mod file in DIR."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(with-eval-after-load 'project
  (cl-defmethod project-root ((project (head go-module)))
    "Return the root directory of a go module PROJECT."
    (cdr project))
  (add-hook 'project-find-functions #'project-find-go-module))

(use-package govet :commands govet)
(use-package gotest :defer t)
(use-package go-gen-test :defer t)
(use-package go-impl :commands go-impl)
(use-package go-tag :commands go-tag-add go-tag-remove)
;; Ensure go-ts-mode-map is available
(with-eval-after-load 'go-ts-mode
  (when (boundp 'go-ts-mode-map)
    (define-key go-ts-mode-map (kbd "C-c `") 'go-tag-add)
    (define-key go-ts-mode-map (kbd "C-u C-c `") 'go-tag-remove)))
(use-package go-eldoc :after (go-mode eldoc) :hook (go-mode . go-eldoc-setup))
(use-package go-projectile :after (go-mode projectile))
(use-package go-playground :custom (go-playground-init-command "go mod init snippet") :commands go-playground)
(use-package flycheck-golangci-lint :hook (go-mode . flycheck-golangci-lint-setup))
(use-package go-fill-struct :commands go-fill-struct)

;;; Dart & Flutter
(use-package dart-mode :hook (dart-mode . (lambda () (subword-mode) (eglot-ensure))) :commands dart-mode)
(use-package flutter :after dart-mode)

;;; Web Development
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  (web-mode-auto-quote-style 3)
  :mode "\\.html?\\'" "\\.erb\\'")
(add-hook 'js-ts-mode-hook
          (lambda ()
            (setq js-indent-level 2)
            (setq tab-width 2)))
(defun kill-jest-process-and-buffer ()
  "Kill jest process and buffer."
  (interactive)
  (delete-process (buffer-name (current-buffer)))
  (kill-buffer))
(use-package jest :bind (:map jest-mode-map ("q" . kill-jest-process-and-buffer)) :commands jest-minor-mode)
(use-package vitest :load-path local-lisp-load-path :straight nil :defer t)
(with-eval-after-load 'typescript-ts-mode
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (cov-mode)
              (subword-mode t)
              (setq-local cov-lcov-file-name (concat (projectile-project-root) "lcov.info"))
              (jest-minor-mode 1)))
  (add-hook 'tsx-ts-mode-hook
            (lambda ()
              (subword-mode t))))

(use-package lsp-biome
  :after lsp-mode
  :straight (:host github :repo "cxa/lsp-biome" :files ("lsp-biome.el"))
  :custom
  (lsp-biome-organize-imports-on-save t)
  (lsp-biome-autofix-on-save t)
  (lsp-biome-format-on-save t))

(with-eval-after-load 'compile
  (defvar node-error-regexp "^[ ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$")
  (add-to-list 'compilation-error-regexp-alist-alist `(nodejs ,node-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'nodejs))
(use-package npm
  :defer t
  :commands (npm-mode npm npm-run npm-install-menu npm-update npm-publish-menu)
  :init
  (dolist (feature '(npm-run npm-install npm-update npm-publish))
    (with-eval-after-load feature (require 'npm))))
(use-package deno-fmt :defer t)
(use-package prisma-ts-mode
  :mode (("\\.prisma\\'" . prisma-ts-mode))
  :hook (prisma-ts-mode . eglot-ensure)
  :ensure-system-package (prisma-language-server . "npm install -g @prisma/language-server")
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(prisma-ts-mode . ("prisma-language-server" "--stdio")))))
(use-package restclient :commands restclient-mode
  :config
  (use-package restclient-jq))
(use-package graphql-ts-mode
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :ensure-system-package (graphql-language-server . "npm i -g graphql-language-service-cli")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql"))))

;;; SQL
(use-package sql-ts-mode
  :straight nil
  :mode ("\\.sql\\'")
  :load-path local-lisp-load-path
  :hook (sql-ts-mode . (lambda ()
                         (setq lsp-enable-indentation nil)
                         (lsp)
                         (indent-tabs-mode t)))
  :init
  (put 'lsp-sqls-workspace-config-path 'safe-local-variable 'stringp))

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

;;; ========================================
;;; INFRASTRUCTURE & TOOLS
;;; ========================================

;;; Docker
(use-package docker
  :custom
  (docker-compose-command (or (and (eq system-type 'gnu/linux) "docker compose")  "docker-compose"))
  :ensure-system-package (docker-langserver . "npm install -g dockerfile-language-server-nodejs")
  :bind
  ("C-c d" . docker)
  ("C-c C-d" . docker-compose))
(add-hook 'dockerfile-ts-mode-hook #'eglot-ensure)
(use-package docker-compose-mode
  :mode ("docker-compose.yaml\\'" "compose.yaml\\'")
  :hook
  (docker-compose-mode . eglot-ensure)
  (docker-compose-mode . display-line-numbers-mode)
  :ensure-system-package (docker-compose-langserver . "npm i -g @microsoft/compose-language-service")
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(docker-compose-mode . ("docker-compose-langserver" "--stdio")))))

;;; AWS
(use-package aws-switch-profile
  :defer t
  :commands aws-switch-profile
  :straight (:host github :repo "phelrine/aws-switch-profile.el" :files ("aws-switch-profile.el")))

(use-package aws-secretsmanager
  :commands aws-secretsmanager-show-secrets-list
  :straight (:host github :repo "phelrine/aws-secretsmanager.el" :files ("aws-secretsmanager.el")))

;;; ========================================
;;; CONFIG FILES & FORMATS
;;; ========================================

(use-package nginx-mode :mode "/nginx/sites-\\(?:available\\|enabled\\)/")
(use-package json-mode :defer t)
(use-package json-reformat :commands json-reformat-region)
(use-package cfn-mode :defer t)
(use-package lua-mode :defer t)
(use-package yaml-mode :defer t)

;;; Gradle
(use-package groovy-mode :defer t)

;;; Markdown
(use-package maple-preview
  :straight (:host github :repo "honmaple/emacs-maple-preview" :files ("*.el" "index.html" "static"))
  :defer t)

;;; ========================================
;;; UTILITIES
;;; ========================================

(use-package restart-emacs :commands restart-emacs)

;;; ========================================
;;; STARTUP OPTIMIZATION - POST INIT
;;; ========================================

;; Restore GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16000000
                  gc-cons-percentage 0.1)
            (message "GC threshold restored to normal")))

;; Run GC when idle for 5 seconds
(run-with-idle-timer 5 t #'garbage-collect)

(provide 'init)
;;; init.el ends here
