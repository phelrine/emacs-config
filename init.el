;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (eq window-system 'ns)
    (require 'cask "/usr/local/opt/cask/cask.el")
  (require 'cask "~/.cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-expand-on-auto-complete t)
 '(ac-show-menu-immediately-on-auto-complete nil)
 '(beacon-color "light green")
 '(beacon-mode t)
 '(bm-buffer-persistence t)
 '(bm-cycle-all-buffers t)
 '(bm-fringe-face (quote bm-face))
 '(bm-fringe-persistent-face (quote bm-face))
 '(bm-highlight-style (quote bm-highlight-line-and-fringe))
 '(bm-persistent-face (quote bm-face))
 '(cc-other-file-alist
   (quote
    (("\\.cpp$"
      (".hpp" ".h"))
     ("\\.h$"
      (".c" ".cpp" ".m" ".mm"))
     ("\\.hpp$"
      (".cpp" ".c"))
     ("\\.m$"
      (".h"))
     ("\\.mm$"
      (".h")))))
 '(clang-format-style
   "{BasedOnStyle: google, IndentWidth: 4, BreakBeforeBraces: Linux, ObjCBlockIndentWidth: 4, ColumnLimit: 0, ObjCSpaceAfterProperty: true}")
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-cmake
                  (company-dabbrev-code :with company-clang)
                  (company-clang company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-sync-timeout 0.5 t)
 '(global-auto-revert-mode t)
 '(global-git-gutter+-mode t)
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(helm-boring-file-regexp-list (quote ("~$" "\\.meta$")))
 '(helm-ff-skip-boring-files t)
 '(history-length 5000)
 '(indent-tabs-mode nil)
 '(lsp-go-executable-path "go-langserver")
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(migemo-coding-system (quote utf-8))
 '(migemo-command "cmigemo")
 '(migemo-options (quote ("-q" "--emacs")))
 '(migemo-regex-dictionary nil)
 '(migemo-user-dictionary nil)
 '(package-selected-packages
   (quote
    (anzu yasnippet company-lsp lsp-go go-tag go-gen-test github-issues go-imports w3m ein go-impl lua-mode govet gotest json-reformat http fabric jinja2-mode ssh-tunnels sql-indent edbi golint escreen apib-mode gitconfig-mode go-projectile go-errcheck go-gopath go-direx go-complete flycheck-swift yaml-mode xcode-mode websocket web-mode volatile-highlights visible-mark use-package undo-tree tumblesocks telephone-line swift-mode solarized-theme smex smeargle smartwin smartparens slim-mode show-marks ruby-hash-syntax ruby-block rtags robe restart-emacs request rainbow-delimiters prodigy popwin point-undo phpunit php-mode pallet open-junk-file omnisharp objc-font-lock oauth2 nyan-mode nlinum nginx-mode magit key-leap jedi ivy idle-highlight-mode highlight-indent-guides helm-projectile helm-migemo helm-ls-git helm-git-grep helm-codesearch helm-bm helm-ag go-rename go-eldoc go-autocomplete git-messenger git-gutter-fringe+ ggtags flymake-cursor flycheck-color-mode-line flycheck-cask expand-region exec-path-from-shell emojify elogcat drag-stuff direx ddskk cursor-in-brackets company-sourcekit company-jedi color-theme coffee-mode codic clang-format circe beacon autofit-frame auto-compile auto-async-byte-compile apache-mode alert ac-clang)))
 '(recentf-max-saved-items 1000)
 '(save-place t nil (saveplace))
 '(savehist-mode t)
 '(show-paren-mode 1)
 '(show-trailing-whitespace t)
 '(skk-use-jisx0201-input-method t)
 '(sort-fold-case t t)
 '(sourcekit-sourcekittendaemon-executable "/usr/local/bin/sourcekittendaemon")
 '(sourcekit-verbose t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(volatile-highlights-mode t)
 '(whitespace-display-mappings (quote ((space-mark 12288 [9633]) (tab-mark 9 [187 9]))))
 '(whitespace-space-regexp "\\(　+\\)")
 '(whitespace-style (quote (face tabs tab-mark spaces space-mark)))
 '(xcode-completing-read-function (quote ivy-completing-read))
 '(yas-prompt-functions (quote (yas-popup-isearch-prompt yas/completing-prompt))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "spring green" :overline nil :underline t))))
 '(flymake-errline ((t (:foreground "orange" :background "blue"))))
 '(flymake-warnline ((t (:background "yellow"))))
 '(highlight-indent-guides-even-face ((t (:background "wheat1"))))
 '(highlight-indent-guides-odd-face ((t (:background "wheat2"))))
 '(objc-font-lock-background ((t (:inherit nil))))
 '(whitespace-space ((t (:foreground "DarkGoldenrod1"))))
 '(whitespace-tab ((t (:foreground "dark blue" :inverse-video nil)))))

(setenv "LANG" "ja_JP.UTF-8")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
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


(global-font-lock-mode 1)
(require 'eldoc)
(global-eldoc-mode t)

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(require 'tramp)
(nconc tramp-default-proxies-alist
       '((nil "\\`root\\'" "/ssh:%h:")
         ("localhost" nil nil)
         ((regexp-quote (system-name)) nil nil)))

;; builtin
(require 'hl-line)
(require 'saveplace)
(require 'savehist)
(require 'recentf)
(require 'whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; packages
(require 'use-package)
(use-package helm-config
  :commands (helm-mode)
  :config (helm-mode t)
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x C-i" . helm-imenu)
         ("C-x C-b" . helm-buffers-list)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)))

(use-package helm-git-grep
  :commands (helm-git-grep-from-isearch)
  :config (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
  :bind (("C-c g" . helm-git-grep)))

(use-package bm
  :commands (bm-buffer-restore bm-buffer-save bm-buffer-save-all bm-repository-save)
  :config
  (setq bm-restore-repository-on-load t)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda ()
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  :bind (("M-[" . bm-previous)
         ("M-]" . bm-next)
         ("M-SPC" . bm-toggle)))

(use-package helm-bm :bind (("C-c m" . helm-bm)))

(use-package company
  :bind ("C-;" . company-complete)
  :hook (prog-mode . company-mode))
(use-package company-lsp)
(use-package company-statistics
  :commands (company-statistics-mode)
  :config (company-statistics-mode t))

(use-package auto-complete)

;;; git
(use-package magit :bind (("C-x g" . magit-status)))
(use-package git-gutter-fringe+ :diminish git-gutter+-mode)
(use-package popup :commands (popup-menu* popup-make-item))
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))
(use-package yasnippet :hook (prog-mode . yas-minor-mode) :config (yas-reload-all))
(use-package popwin
  :config
  (dolist (window '((" *auto-async-byte-compile*")
                    (":home" :position left)
                    ("*compilation*")))
    (add-to-list 'popwin:special-display-config window)))

(use-package exec-path-from-shell :commands (exec-path-from-shell-initialize) :config (exec-path-from-shell-initialize))
(use-package open-junk-file :commands open-junk-file)
(use-package yaml-mode :mode "\\.yml$")
(use-package telephone-line :commands (telephone-line-mode) :config (telephone-line-mode t))
(use-package color-theme)
(use-package solarized :config (load-theme 'solarized-light t))
(use-package expand-region :bind ("C-M-SPC" . er/expand-region))
(use-package undo-tree :commands (global-undo-tree-mode) :config (global-undo-tree-mode t))
(use-package auto-async-byte-compile :config (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))
(use-package ddskk :bind ("C-x j" . skk-mode))
(use-package rainbow-delimiters :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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

;; Flymake
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
         (local-file  (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (list "g++" (list "std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(use-package flymake :commands (flymake-init-create-temp-buffer-copy) :config (add-to-list 'flymake-allowed-file-name-masks '("\\.cpp$" flymake-cc-init)))
(use-package flymake-cursor)
(defun flycheck-setting-c/c++()
  (flycheck-mode t)
  ;; (flycheck-select-checker 'c/c++-cppcheck)
  )

(use-package flycheck
  :hook ((c-mode . flycheck-setting-c/c++)
         (c++-mode . flycheck-setting-c/c++)
         (csharp-mode . flycheck-mode)
         (ruby-mode . flycheck-mode)
         (objc-mode . flycheck-mode)
         (go-mode . flycheck-mode)))
(use-package flycheck-color-mode-line
  :config (eval-after-load "flycheck" '(add-hook 'flychech-mode-hook 'flycheck-color-mode-line-mode)))

(use-package nlinum :hook (prog-mode . nlinum-mode))
(use-package highlight-indent-guides :hook (prog-mode . highlight-indent-guides-mode))
(use-package projectile
  :commands (projectile-mode helm-projectile-on)
  :config
  (projectile-mode t)
  (helm-projectile-on))

(use-package anzu :commands (global-anzu-mode) :config (global-anzu-mode +1))

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
  (local-set-key (kbd "C-c c") 'compile)
  )
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
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)

;;; C#
(use-package csharp-mode :mode "\\.cs$")
(defun setup-csharp-mode ()
  (let ((global-backends company-backends))
    (set (make-local-variable 'company-backends) (cons 'company-omnisharp global-backends))))
(use-package omnisharp
  :hook omnisharp-mode
  :config (add-hook 'csharp-mode-hook 'setup-csharp-mode))

;;; Obj-C
(use-package objc-font-lock :config (add-hook 'objc-mode-hook 'objc-font-lock-mode))
(defadvice ff-get-file-name (around ff-get-file-name-framework
                                    (search-dirs
                                     fname-stub
                                     &optional suffix-list))
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

(use-package clang-format)

;;; Python
(require 'ipython nil t)
(use-package jedi :commands (jedi:setup jedi-mode))
(use-package company-jedi)
(defun python-mode-setup()
  (when (featurep 'jedi)
    (jedi:setup)
    (jedi-mode t)
    (when (featurep 'company-jedi)
      (add-to-list 'company-backends 'company-jedi)
      )))
(add-hook 'python-mode-hook 'python-mode-setup)

;; indent
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

(use-package robe :config (add-to-list 'company-backends 'company-robe))
(use-package web-mode :mode ".+\\.erb$")

(defun coffee-custom ()
  "coffee-mode-hook"
  ;; CoffeeScript uses two spaces.
  (set (make-local-variable 'tab-width) 2)
  ;; If you don't have js2-mode
  (setq-default
   coffee-js-mode 'javascript-mode
   coffee-args-compile '("-c" "--bare")
   coffee-debug-mode t)
  ;; Compile '.coffee' files on every save
  (and (file-exists-p (buffer-file-name))
       (file-exists-p (coffee-compiled-file-name))
       (coffee-cos-mode t)))

(use-package coffee-mode
  :commands (coffee-compiled-file-name coffee-cos-mode)
  :bind (("M-r" . coffee-compile-buffer))
  :config (add-hook 'coffee-mode-hook 'coffee-custom)
  :mode "\\.coffee$")

(defun go-mode-setup()
  (subword-mode 1)
  (set (make-local-variable 'company-backends)
       '((company-lsp company-keywords company-dabbrev-code))))
(use-package lsp-go :commands (lsp-go-enable) :hook (go-mode . lsp-go-enable))
(use-package go-projectile)
(use-package govet)
(use-package go-tag
  :bind (:map go-mode-map
              (("C-c `" . go-tag-add)
               ("C-u C-c `" . go-tag-remove))))
(use-package go-impl)
(use-package go-gen-test)
(use-package go-eldoc :commands (go-eldoc-setup) :hook (go-mode . go-eldoc-setup))
(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :bind (:map go-mode-map (("C-c ." . godef-jump) ("C-c ," . pop-tag-mark)))
  :config (add-hook 'go-mode-hook 'go-mode-setup))

(use-package company-sourcekit)
(defun swift-mode-setup()
  (when (featurep 'flycheck)
    (flycheck-mode t)
    (add-to-list 'flycheck-checkers 'swift)
    (setq-default flycheck-swift-sdk-path
          (replace-regexp-in-string
           "\n+$" "" (shell-command-to-string
                      "xcrun --show-sdk-path --sdk macosx")))
    ))

(use-package swift-mode
  :config (add-hook 'swift-mode-hook 'swift-mode-setup))

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

(global-set-key "\C-z" nil)
(use-package escreen
  :bind (("C-z c" . escreen-create-screen)
         ("C-z n" . escreen-goto-next-screen)
         ("C-z p" . escreen-goto-prev-screen)
         ("C-z k" . escreen-kill-screen)
         ("C-z d" . escreen-get-current-screen-number)))

(use-package web-mode :mode "\\.html\\'")
(use-package json-reformat)
(use-package apib-mode :mode "\\.apib$")

(use-package ein)
;; (load "latex-mode-config")
;; ライブコーディング用設定
;; (set-face-attribute 'default nil :height 300)
