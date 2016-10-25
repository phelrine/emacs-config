
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (eq window-system 'ns)
    (require 'cask)
  (require 'cask (concat (getenv "HOME") "/.cask/cask.el")))
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
 '(company-async-timeout 0.5 t)
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-cmake company-capf
                  (company-dabbrev-code :with company-clang)
                  (company-clang company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(global-auto-revert-mode t)
 '(global-git-gutter+-mode t)
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(helm-boring-file-regexp-list (quote ("~$" "\\.meta$")))
 '(helm-ff-skip-boring-files t)
 '(history-length 5000)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(migemo-coding-system (quote utf-8))
 '(migemo-command "cmigemo")
 '(migemo-options (quote ("-q" "--emacs")))
 '(migemo-regex-dictionary nil)
 '(migemo-user-dictionary nil)
 '(package-selected-packages
   (quote
    (yaml-mode xcode-mode websocket web-mode volatile-highlights visible-mark use-package undo-tree tumblesocks telephone-line swift-mode solarized-theme smex smeargle smartwin smartparens slim-mode show-marks ruby-hash-syntax ruby-block rtags robe restart-emacs request rainbow-delimiters prodigy popwin point-undo phpunit php-mode pallet open-junk-file omnisharp objc-font-lock oauth2 nyan-mode nlinum nginx-mode magit key-leap jedi ivy idle-highlight-mode hyperbole highlight-indent-guides helm-projectile helm-migemo helm-ls-git helm-git-grep helm-codesearch helm-bm helm-ag go-rename go-eldoc go-autocomplete git-messenger git-gutter-fringe+ ggtags flymake-cursor flycheck-color-mode-line flycheck-cask expand-region exec-path-from-shell es-mode emojify elogcat drag-stuff direx ddskk cursor-in-brackets company-sourcekit company-jedi company-go company-anaconda color-theme coffee-mode codic clang-format circe beacon autofit-frame auto-compile auto-async-byte-compile apache-mode alert ac-clang)))
 '(recentf-max-saved-items 1000)
 '(save-place t nil (saveplace))
 '(savehist-mode t)
 '(show-paren-mode 1)
 '(show-trailing-whitespace t)
 '(sourcekit-sourcekittendaemon-executable "/usr/local/bin/sourcekittendaemon")
 '(sourcekit-verbose t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(volatile-highlights-mode t)
 '(whitespace-display-mappings (quote ((space-mark 12288 [9633]) (tab-mark 9 [187 9]))))
 '(whitespace-space-regexp "\\(　+\\)")
 '(whitespace-style (quote (face tabs tab-mark spaces space-mark)))
 '(xcode-completing-read-function (quote ivy-completing-read)))

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
 '(whitespace-tab ((t (:foreground "blue")))))

(setenv "LANG" "ja_JP.UTF-8")
(add-to-list 'load-path "~/.emacs.d/lisp")

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(add-to-list 'exec-path (concat (getenv "HOME") "/repos/rtags/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))
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

(require 'tramp)
(add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
(add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil))

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

(setq bm-restore-repository-on-load t)
(use-package bm
  :commands (bm-buffer-restore bm-buffer-save bm-buffer-save-all bm-repository-save)
  :config
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

(use-package helm-bm :commands (helm-bm) :bind (("C-c m" . helm-bm)))

(use-package company)
(use-package auto-complete)

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq vcs-ediff-p nil)
  (defadvice magit-ediff (around flymake-off activate)
    (setq vcs-ediff-p t)
    ad-do-it
    (setq vcs-ediff-p nil)))

(use-package git-gutter-fringe+ :diminish git-gutter+-mode)
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
(use-package yasnippet
  :config
  (setq yas-prompt-functions '(yas-popup-isearch-prompt yas/completing-prompt))
  (yas-global-mode 1))

(use-package popwin
  :config
  (dolist (window '((" *auto-async-byte-compile*")
                    (":home" :position left)
                    ("*compilation*")))
    (add-to-list 'popwin:special-display-config window)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))
(use-package open-junk-file :commands open-junk-file)
(use-package yaml-mode :mode "\\.yml$")
(use-package telephone-line :config (telephone-line-mode t))
(use-package color-theme)
(use-package solarized :config (load-theme 'solarized-light t))
(use-package expand-region :bind ("C-M-SPC" . er/expand-region))
(use-package undo-tree :config (global-undo-tree-mode t))
(use-package escreen)
(use-package auto-async-byte-compile
  :config (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))
(use-package ddskk :bind ("C-x j" . skk-mode))
(use-package rainbow-delimiters :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (setq migemo-dictionary
        (cond
         ((eq system-type 'darwin) "/usr/local/share/migemo/utf-8/migemo-dict")
         ((eq system-type 'gnu-linux) "/usr/share/cmigemo/utf-8/migemo-dict")))
  (migemo-init))

(use-package point-undo
  :bind (("<f5>" . point-undo)
         ("<f6>" . point-redo)))

(use-package cursor-in-brackets :config (global-cursor-in-brackets-mode t))

;; Flymake
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
         (local-file  (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (list "g++" (list "std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(use-package flymake :config (add-to-list 'flymake-allowed-file-name-masks '("\\.cpp$" flymake-cc-init)))
(use-package flymake-cursor)
(defun flycheck-setting-c/c++()
  (flycheck-mode t)
  ;; (flycheck-select-checker 'c/c++-cppcheck)
  )

(use-package flycheck
  :config
  (add-hook 'c-mode-hook 'flycheck-setting-c/c++)
  (add-hook 'c++-mode-hook 'flycheck-setting-c/c++)
  (add-hook 'csharp-mode-hook 'flycheck-mode)
  (add-hook 'ruby-mode-hook 'flycheck-mode)
  (add-hook 'objc-mode-hook 'flycheck-mode)
  )

(use-package flycheck-color-mode-line
  :config (eval-after-load "flycheck" '(add-hook 'flychech-mode-hook 'flycheck-color-mode-line-mode)))

(use-package nlinum :config (add-hook 'prog-mode-hook 'nlinum-mode))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package projectile :disabled
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;;; Lisp Mode
(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook hook
            (lambda ()
              (turn-on-eldoc-mode)
              (company-mode t)
              (local-set-key (kbd "C-;") 'company-complete))))

;;; CC-Mode
(defun cc-mode-setup()
  (eldoc-mode t)
  (local-set-key (kbd "C-c c") 'compile)
  (setq c-basic-offset 4
        indent-tabs-mode nil
        comment-column 40)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (local-set-key (kbd "C-c o") 'ff-find-other-file))
(add-hook 'c-mode-common-hook 'cc-mode-setup)

;; (use-package gtags :config (add-hook 'c-mode-common-hook 'gtags-mode))
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
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

(use-package auto-complete-clang-async
  :load-path "~/repos/emacs-clang-complete-async"
  :if (eq window-system 'ns)
  :config
  (setq ac-clang-complete-executable (executable-find "clang-complete"))
  (defun objc-mode-setup()
    (setq ac-clang-cflags
          (split-string
           (concat (shell-command-to-string (concat (executable-find "clang-complete-helper") " cflags "
                                                    (and buffer-file-name
                                                         (file-relative-name buffer-file-name))))
                   " -ObjC")))
    (add-to-list 'ac-sources 'ac-source-clang-async)
    (ac-clang-launch-completion-process)
    (auto-complete-mode t)
    (local-set-key (kbd "C-;") 'auto-complete))
  (add-hook 'objc-mode-hook 'objc-mode-setup)
  (add-hook 'c++-mode-hook 'objc-mode-setup))
(use-package objc-font-lock :config (add-hook 'objc-mode-hook 'objc-font-lock-mode))
(use-package irony :disabled :config (add-hook 'c++-mode-hook 'irony-mode))
(use-package company-irony :disabled
  :config (progn
            (set (make-local-variable 'company-backends) '(company-irony))
            (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(use-package rtags :disabled
  :config (add-hook 'c++-mode-hook
                    (lambda ()
                      (rtags-enable-standard-keybindings c-mode-base-map)
                      (setq rtags-completions-enabled t)
                      (rtags-diagnostics))))

(use-package company-rtags
  :config (progn (add-hook
                  'c++-mode-hook
                  (lambda()
                    ;; (set (make-local-variable 'company-backends) '(company-rtags))
                    (setq company-rtags-begin-after-member-access t)))))

(use-package function-args :disabled :config (fa-config-default))

;;; C#
(use-package csharp-mode :mode "\\.cs$")
(use-package omnisharp
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (let ((global-backends company-backends))
    (set (make-local-variable 'company-backends) (cons 'company-omnisharp global-backends))))

;;; Obj-C
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
    (setq cc-search-directories
          '("." "../include" "/usr/include" "/usr/local/include/*"
            "/System/Library/Frameworks" "/Library/Frameworks")))

(use-package clang-format)

(require 'ipython nil t)
(use-package anaconda-mode :ensure :config (add-to-list 'company-backends 'company-anaconda))
(defun python-mode-setup()
  (when (featurep 'anaconda-mode)
    (anaconda-mode t))
  (company-mode t)
  (local-set-key (kbd "C-;") 'company-complete))

(add-hook 'python-mode-hook 'python-mode-setup)

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

(use-package robe :config (add-to-list 'company-backends 'company-robe))
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

(defun go-mode-setup()
  (go-eldoc-setup)
  (auto-complete-mode t)
  (flycheck-mode t)
  (local-set-key (kbd "C-;") 'auto-complete)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark))

(use-package go-autocomplete)
(use-package go-mode :config (add-hook 'go-mode-hook 'go-mode-setup))

(use-package company-sourcekit)
(defun swift-mode-setup()
  (when (and (featurep 'company) (featurep 'company-sourcekit))
    (company-mode t)
    (add-to-list 'company-backends '(company-dabbrev-code :with company-sourcekit))
    (local-set-key (kbd "C-;") 'company-complete))
  (when (featurep 'flycheck)
    (flycheck-mode t)
    (setq flycheck-swift-sdk-path
          (replace-regexp-in-string
           "\n+$" "" (shell-command-to-string
                      "xcrun --show-sdk-path --sdk macosx")))
    (add-to-list 'flycheck-checkers 'swift))
  )

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
  (when (featurep 'company)
    (company-mode t)
    (local-set-key (kbd "C-;") 'company-complete))
  (local-set-key (kbd "C-c s") 'scheme-other-window))
(add-hook 'scheme-mode-hook 'scheme-mode-setup)

;; (load "latex-mode-config")

;; ライブコーディング用設定
;; (set-face-attribute 'default nil :height 300)
