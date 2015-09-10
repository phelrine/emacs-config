(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setenv "LANG" "ja_JP.UTF-8")
(add-to-list 'load-path "~/.emacs.d/lisp")

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(add-to-list 'exec-path (concat (getenv "HOME") "/repos/rtags/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))

(require 'tramp)
(add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
(add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-cmake company-capf
                  (company-clang :with company-dabbrev-code)
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(global-auto-revert-mode t)
 '(helm-boring-file-regexp-list (quote ("~$" "\\.meta$")))
 '(helm-ff-skip-boring-files t)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(show-paren-mode 1)
 '(tab-width 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:foreground "orange" :background "blue"))))
 '(flymake-warnline ((t (:background "yellow"))))
 '(whitespace-space ((t (:foreground "DarkGoldenrod1"))))
 '(whitespace-tab ((t (:foreground "blue")))))

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
  (nconc default-frame-alist '((width . 120)(height . 40)))
  (setq xcode:sdk "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk")
  (setq xcode:foundation (concat xcode:sdk "/System/Library/Frameworks/Foundation.framework/Headers/"))
  (setq xcode:uikit (concat xcode:sdk "/System/Library/Frameworks/UIKit.framework/Headers/"))
  (setq company-clang-argument
        '("-Wall" "-Wextra" "-fsyntax-only" "-ObjC" "-std=c99"
          "-isysroot" xcode:sdk "-I."
          "-D__IPHONE_OS_VERSION_MIN_REQUIRED=30200")))

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
    (savehist-mode 1)
    (setq history-length 5000)))
(use-package recentf
  :config
  (progn (setq recentf-max-saved-items 1000)))
(use-package hl-line
  :config
  (progn
    (set-face-background 'hl-line "violet")
    (set-face-underline 'hl-line "blue")
    (global-hl-line-mode t)))

;; packages
(use-package helm-config :ensure helm
  :config (helm-mode t)
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x C-i" . helm-imenu)
         ("C-x C-b" . helm-buffers-list)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)))

(use-package company :ensure
  :init (progn
          (setq company-dabbrev-downcase nil)
          (add-hook 'after-init-hook 'global-company-mode))
  :bind (("C-;" . company-complete)))
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
(use-package yasnippet :ensure
  :config (progn
            (yas/global-mode 1)
            (setq yas/prompt-functions '(yas/completing-prompt))))

(use-package popwin :ensure
  :config (progn
            (setq display-buffer-function 'popwin:display-buffer)
            (dolist (window '((" *auto-async-byte-compile*")
                              (":home" :position left)))
              (add-to-list 'popwin:special-display-config window))))

(use-package exec-path-from-shell :config (exec-path-from-shell-initialize) :ensure)
(use-package open-junk-file :ensure :commands open-junk-file)
(use-package yaml-mode :mode "\\.yml$" :ensure)
(use-package telephone-line :config (telephone-line-mode t) :ensure)
(use-package color-theme :ensure)
(use-package solarized :config (load-theme 'solarized-light t) :ensure solarized-theme)
(use-package expand-region :bind ("C-M-SPC" . er/expand-region) :ensure)
(use-package undo-tree :config (global-undo-tree-mode t) :ensure)
(use-package escreen)
(use-package auto-async-byte-compile :ensure
  :config (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))
(use-package skk)
(use-package ddskk :bind ("C-x j" . skk-mode) :ensure)

;;; CC-Mode
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(defun flymake-ruby-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(use-package flymake
  :config (nconc flymake-allowed-file-name-masks
                 '(("\\.cpp$" flymake-cc-init)
                   (".+\\.rb$" flymake-ruby-init)
                   ("Rakefile$" flymake-ruby-init))))
(use-package flymake-cursor)

(defun flycheck-setting-c/c++()
  (flycheck-mode t)
  (flycheck-select-checker 'c/c++-cppcheck))
(use-package flycheck :ensure
  :config
  (progn
    (add-hook 'c-mode-hook #'flycheck-setting-c/c++)
    (add-hook 'c++-mode-hook #'flycheck-setting-c/c++)
    (add-hook 'csharp-mode-hook #'flycheck-mode)
    (add-hook 'ruby-mode-hook #'flycheck-mode)))

(use-package nlinum :ensure
  :config (progn
            (add-hook 'c-mode-common-hook #'nlinum-mode)
            (add-hook 'ruby-mode-hook #'nlinum-mode)))

(setq cc-other-file-alist
      `(("\\.cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".m" ".mm"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.m$" (".h"))
        ("\\.mm$" (".h"))))

(add-hook
 'c-mode-common-hook
 '(lambda ()
    (eldoc-mode t)
    (local-set-key (kbd "C-c c") 'compile)
    (setq c-basic-offset 4
          indent-tabs-mode nil
          comment-column 40)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; (use-package gtags :config (add-hook 'c-mode-common-hook #'gtags-mode))
;;; C++
(add-to-list 'auto-mode-alist '(".+\\.h$" . c++-mode))
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (or (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)
                           (re-search-forward "@\\<protocol\\>" magic-mode-regexp-match-limit t))))
               . objc-mode))

(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

(use-package irony :ensure :disabled :config (add-hook 'c++-mode-hook #'irony-mode))
(use-package company-irony :ensure :disabled
  :config (progn
            (set (make-local-variable 'company-backends) '(company-irony))
            (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(use-package rtags :ensure :disabled
  :config (add-hook 'c++-mode-hook
            (lambda ()
              (rtags-enable-standard-keybindings c-mode-base-map)
              (setq rtags-completions-enabled t)
              (rtags-diagnostics))))

(use-package company-rtags :ensure rtags
  :config (progn (add-hook
                  'c++-mode-hook
                  (lambda()
                    (set (make-local-variable 'company-backends) '(company-rtags))
                    (setq company-rtags-begin-after-member-access t)))))

(use-package function-args :ensure :disabled :config (fa-config-default))

;;; C#
(use-package csharp-mode :ensure :mode "\\.cs$")
(use-package omnisharp :ensure
  :config (progn
            (add-hook 'csharp-mode-hook 'omnisharp-mode)
            (let ((global-backends company-backends))
              (set (make-local-variable 'company-backends) (cons 'company-omnisharp global-backends)))))

;; 設定ファイルを別にする
;; indent
(setq ruby-deep-indent-paren-style nil)
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

(use-package robe :config (progn (push 'company-robe company-backends)))
(use-package web-mode :mode ".+\\.erb$")

(defun coffee-custom ()
  "coffee-mode-hook"
  ;; CoffeeScript uses two spaces.
  (set (make-local-variable 'tab-width) 2)
  ;; If you don't have js2-mode
  (setq coffee-js-mode 'javascript-mode)
  ;; If you don't want your compiled files to be wrapped
  (setq coffee-args-compile '("-c" "--bare"))
  ;; *Messages* spam
  (setq coffee-debug-mode t)
  ;; Emacs key binding
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
  ;; Compile '.coffee' files on every save
  (and (file-exists-p (buffer-file-name))
       (file-exists-p (coffee-compiled-file-name))
       (coffee-cos-mode t)))

(use-package coffee-mode
  :config (add-hook 'coffee-mode-hook 'coffee-custom)
  :mode "\\.coffee$")

;; (Load "python-config")
;; (load "scheme-config")
;; (load "latex-mode-config")
