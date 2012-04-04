(let ((path (substring (shell-command-to-string "echo $PATH") 0 -1)))
  (setq exec-path (split-string path ":"))
  (setenv "PATH" path))
(setenv "LANG" "ja_JP.UTF-8")

(nconc load-path
       (mapcar (lambda (d)
                 (expand-file-name (concat "~/.emacs.d/" d)))
               '("site-lisp/" "yasnippet/")))
(normal-top-level-add-to-load-path (cddr (directory-files "~/.emacs.d/modules/" t)))

(require 'yasnippet-config)
(require 'auto-complete-config)
(require 'cedet)
(require 'open-junk-file)
(require 'switch-window)
(require 'undo-tree)
(global-undo-tree-mode)

(require 'dropdown-list)
(yas/setup "~/.emacs.d/yasnippet")
(setq yas/prompt-functions '(yas/dropdown-prompt yas/completing-prompt))

(if (fboundp 'semantic-load-enable-code-helpers)
    (semantic-load-enable-code-helpers))

(ac-config-default)
(add-hook 'find-file-hook 'auto-complete-mode)

(let ((config-dir "~/.emacs.d/config/"))
  (mapc (lambda (config)
          (load (concat config-dir (replace-regexp-in-string "\\.el$" "" config))))
        (directory-files config-dir nil "\\.el$")))

(progn
  (show-paren-mode 1)
  (global-hl-line-mode 1)
  (set-background-color "gray90")
  (set-face-background 'hl-line "violet")
  (set-face-underline-p 'hl-line "black")
  (display-time)
  (line-number-mode 1)
  (column-number-mode 1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  nil)

(require 'jaunte)
(require 'hideshow)
(require 'anything-startup)
(require 'magit)
(require 'auto-async-byte-compile)
(require 'multi-command)
(require 'haml-mode)

(mapc (lambda (c) (global-set-key (read-kbd-macro (car c)) (cdr c)))
      '(("C-h" . delete-backward-char)
        ("C-o" . other-window)
        ("C-c C-j" . jaunte)
        ("C-t" . hs-toggle-hiding)
        ("C-x C-b" . anything-buffers+)
        ("M-y" . anything-show-kill-ring)
        ("C-x g" . magit-status)
        ("C-q" . mcmd-set-next-mode)))

(windmove-default-keybindings)
(define-multi-command global-map "C-f" '(forward-char forward-word) :reset t)
(define-multi-command global-map "C-b" '(backward-char backward-word) :reset t)

(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

(require 'saveplace)
(setq-default save-place t)

(require 'savehist)
(savehist-mode 1)
(setq history-length 5000)

(setq make-backup-files nil)
(setq-default tab-width 4
              indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-auto-revert-mode 1)

(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'message-box 'message)

(when (eq (window-system) 'ns)
  (set-fontset-font "fontset-default" 'japanese-jisx0208
                    '("Hiragino_Kaku_Gothic_ProN" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'katakana-jisx0201
                    '("Hiragino_Kaku_Gothic_ProN" . "iso10646-1"))
  (nconc default-frame-alist '((width . 120)(height . 40))))

