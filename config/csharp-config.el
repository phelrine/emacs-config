(require 'csharp-mode)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; C#モードフック
(add-hook 'csharp-mode-hook
          '(lambda()
             (setq comment-column 40)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)
             (c-set-offset 'substatement-open 0)
             (c-set-offset 'case-label '+)
             (c-set-offset 'arglist-intro '+)
             (c-set-offset 'arglist-close 0)

             (omnisharp-mode t)
             )
          )

;; Gtags設定
(require 'gtags)
(global-set-key (kbd "C-'") 'gtags-find-tag-other-window)  ; (別バッファで)関数の定義元(関数の実体)へジャンプ
(global-set-key (kbd "C-M-'") 'gtags-find-tag)             ; 変数等のジャンプ
