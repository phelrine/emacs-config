(let ((path (substring (shell-command-to-string "echo $PATH") 0 -1)))
  (setq exec-path (split-string path ":"))
  (setenv "PATH" path))
(setenv "LANG" "ja_JP.UTF-8")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/config")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(defvar my-el-packages
  '(anything anything-startup apel auto-async-byte-compile auto-complete coffee-mode el-get flymake-cursor fuzzy haml-mode magit open-junk-file popup popwin switch-window tempbuf undo-tree yaml-mode yasnippet yasnippet-config powerline escreen key-chord))

(when (require 'el-get nil t)
  (el-get 'sync my-el-packages))

(require 'open-junk-file nil t)
(require 'haml-mode nil t)

(when (require 'auto-async-byte-compile nil t)
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

(when (require 'anything-startup nil t)
  (global-set-key (kbd "M-y") 'anything-show-kill-ring)
  (global-set-key (kbd "C-x C-b") 'anything-buffers+))

(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (global-auto-complete-mode 1))

(when (require 'flymake nil t)
  (require 'flymake-cursor nil t)
  (set-face-background 'flymake-errline "red")
  (set-face-background 'flymake-warnline "yellow")
  (defvar flymake-err-line-patterns
    `(("\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)" 1 2 3 4) ; gcc 4.5
      ("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)          ; ruby
      ,@flymake-err-line-patterns))

  (add-hook 'find-file-hook 'flymake-find-file-hook)

  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes"  (list local-file))))

  (push '("\\.py\\'" flymake-pyflakes-init) flymake-allowed-file-name-masks))

(when (require 'hideshow nil t)
  (add-hook 'ruby-mode-hook (lambda () (hs-minor-mode t)))
  (add-hook 'c-mode-common-hook (lambda () (hs-minor-mode t)))
  (global-set-key (kbd "C-t") 'hs-toggle-hiding))

(when (require 'magit nil t)
  (global-set-key (kbd "C-x g") 'magit-status))

(when (require 'popwin nil t)
  (setq display-buffer-function 'popwin:display-buffer)
  (global-set-key (kbd "C-x C-p") popwin:keymap)
  (add-to-list 'popwin:special-display-config '("magit" :regexp t :height 0.4 :stick t)))

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

(when (require 'saveplace nil t)
  (setq-default save-place t))

(when (require 'savehist nil t)
  (savehist-mode 1)
  (setq history-length 5000))

(when (require 'tempbuf nil t)
  (dolist (hook '(view-mode-hook
                  apropos-mode-hook
                  magit-mode-hook
                  dired-mode-hook
                  find-file-hook))
    (add-hook hook 'turn-on-tempbuf-mode))
  (global-set-key (kbd "C-x p") 'tempbuf-mode))

(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(when (require 'yasnippet-config nil t)
  (require 'dropdown-list)
  (defun yas/minor-mode-off()) ;; dummy
  (yas/global-mode 1)
  (setq yas/prompt-functions '(yas/dropdown-prompt yas/completing-prompt)))

(when (require 'switch-window nil t)
  (global-set-key (kbd "C-o") 'switch-window))

(when (require 'ido nil t)
  (ido-mode 1))

(global-set-key (kbd "C-h") 'delete-backward-char)

;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c c") 'compile)
            (c-set-offset 'arglist-close 0)))

(column-number-mode 1)
(display-time)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(line-number-mode 1)
(menu-bar-mode -1)
(set-background-color "gray90")
(set-face-background 'hl-line "violet")
(set-face-underline-p 'hl-line "black")
(setq make-backup-files nil)
(setq-default tab-width 4 indent-tabs-mode nil)
(show-paren-mode 1)
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

(load "coffee-config")
(load "gtags-config")
(load "latex-mode-config")
(load "ruby-config")

;;; Python
(require 'ipython nil t)

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


(when (require 'escreen nil t)
  (setq escreen-prefix-char "\C-z")
  (escreen-install))

(require 'powerline nil t)
(defpowerline escreen (propertize (format-mode-line
                                   (if (boundp 'escreen-current-screen-string)
                                       (concat "S" escreen-current-screen-string) ""))))

(setq-default  mode-line-format
               (list "%e"
                     '(:eval (concat
                              (powerline-rmw 'left nil)
                              (powerline-buffer-id 'left nil powerline-color1)
                              (powerline-escreen 'left powerline-color1 nil)
                              (powerline-major-mode 'left powerline-color1)
                              (powerline-minor-modes 'left powerline-color1)
                              (powerline-narrow  'left powerline-color1  powerline-color2)
                              (powerline-vc 'center powerline-color2)
                              (powerline-make-fill  powerline-color2)
                              (powerline-row 'right powerline-color1 powerline-color2)
                              (powerline-make-text ":" powerline-color1  )
                              (powerline-column 'right powerline-color1  )
                              (powerline-percent 'right nil powerline-color1)
                              (powerline-make-text "  " nil)))))

(when (require 'key-chord nil t)
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.05)
  (key-chord-define-global "op" 'popwin:popup-buffer)
  (key-chord-define-global "kw" 'delete-other-windows))
