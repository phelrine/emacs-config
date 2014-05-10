(require 'cl)
(require 'package)
(package-initialize)

(let ((path (substring (shell-command-to-string "echo $PATH") 0 -1)))
  (setq exec-path (split-string path ":"))
  (setenv "PATH" path))
(setenv "LANG" "ja_JP.UTF-8")

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar installed-packages
  '(
    auto-async-byte-compile
    auto-complete
    coffee-mode
    color-theme
    csharp-mode
    escreen
    flymake-cursor
    flymake-python-pyflakes
    fuzzy
    haml-mode
    helm
    key-chord
    magit
    markdown-mode
    open-junk-file
    popwin
    powerline
    smartparens
    smartrep
    solarized-theme
    switch-window
    undo-tree
    yaml-mode
    yasnippet
    exec-path-from-shell
    ))

(let ((not-installed
       (loop for x in installed-packages if (not (package-installed-p x)) collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

(require 'open-junk-file nil t)

(when (require 'auto-async-byte-compile nil t)
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
  )

(when (require 'helm-config nil t)
  (helm-mode 1)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "C-x C-i") 'helm-imenu)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (custom-set-variables
   '(helm-ff-skip-boring-files t)
   '(helm-boring-file-regexp-list '("~$" "\\.meta$")))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  )

(when (require 'company nil t)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-;") 'company-complete)
  )

(when (require 'flymake nil t)
  (require 'flymake-cursor nil t)
  (set-face-foreground 'flymake-errline "orange")
  (set-face-background 'flymake-errline "blue")
  (set-face-background 'flymake-warnline "yellow")

  (defvar flymake-err-line-patterns
    `(("\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)" 1 2 3 4) ; gcc 4.5
      ,@flymake-err-line-patterns))
  )

(when (require 'magit nil t)
  (global-set-key (kbd "C-x g") 'magit-status))

(when (require 'popwin nil t)
  (setq display-buffer-function 'popwin:display-buffer)
  (global-set-key (kbd "C-x C-p") popwin:keymap)
  (setq popwin:special-display-config
        (append popwin:special-display-config
                '(("magit" :regexp t :height 0.4 :stick t)
                  (" *auto-async-byte-compile*")
                  (":home" :position left)))))

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

(when (require 'saveplace nil t)
  (setq-default save-place t))

(when (require 'savehist nil t)
  (savehist-mode 1)
  (setq history-length 5000))

(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(when (require 'yasnippet)
  ;; (defun yas/minor-mode-off()) ;; dummy
  (yas/global-mode 1)
  (setq yas/prompt-functions '(yas/completing-prompt)))

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'delete-backward-char)

;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

(require 'hl-line)
(set-face-background 'hl-line "violet")
(set-face-underline-p 'hl-line "blue")
(custom-set-variables
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(show-paren-mode 1)
 '(make-backup-files nil)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 )

(menu-bar-mode -1)
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
  (nconc default-frame-alist '((width . 120)(height . 40))))

(load "c-config")
(load "python-config")
(load "ruby-config")
(load "coffee-config")
(load "latex-mode-config")

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

(add-hook 'scheme-mode-hook
          (lambda () (local-set-key (kbd "C-c s") 'scheme-other-window)))

;;; Perl
(defalias 'perl-mode 'cperl-mode)
(require 'cperl-mode)
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)
(add-hook 'cperl-mode-hook '(lambda () (setq indent-tabs-mode nil)))

(when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))
(defface my-face-b-1 '((t (:background "red"))) nil)
(defface my-face-b-2 '((t (:background "blue"))) nil)
(defface my-face-u-1 '((t (:underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks
          '(lambda ()
             (if font-lock-mode
                 nil
               (font-lock-mode t))) t)

(when (require 'powerline nil t)
  (powerline-default-theme)
  )

(when (require 'key-chord nil t)
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.05)
  ;; (key-chord-define-global "op" 'popwin:popup-buffer)
  (key-chord-define-global "kw" 'delete-other-windows))

(push '(".+\\.h$" . c++-mode) auto-mode-alist)

(require 'escreen nil t)
(require 'skk nil t)

(when (and (require 'color-theme nil t) (require 'solarized))
  (load-theme 'solarized-light t))

(when (require 'exec-path-from-shell nil t)
  (exec-path-from-shell-initialize))
