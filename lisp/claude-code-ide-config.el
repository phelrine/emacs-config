;;; claude-code-ide-config.el --- Configuration for claude-code-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Shogo Nagasaka
;; Keywords: tools

;;; Commentary:

;; Custom configuration for claude-code-ide including:
;; - C-o keybinding to send directly to terminal
;; - Posframe-based input dialog with SKK support
;; - Visual cursor and mode indicators

;;; Code:

(require 'claude-code-ide)
(require 'posframe)

;;; C-o Terminal Keybinding

(defun claude-code-ide-send-c-o ()
  "Send C-o directly to the terminal in Claude Code IDE buffer."
  (interactive)
  (cond
   ((eq claude-code-ide-terminal-backend 'vterm)
    (when (fboundp 'vterm-send-string)
      (vterm-send-string "\C-o")))
   ((eq claude-code-ide-terminal-backend 'eat)
    (when (and (boundp 'eat-terminal)
               eat-terminal
               (fboundp 'eat-term-send-string))
      (eat-term-send-string eat-terminal "\C-o")))))

(defun claude-code-ide-setup-c-o-binding ()
  "Setup C-o keybinding for Claude Code IDE buffers only."
  (when (and (fboundp 'claude-code-ide--session-buffer-p)
             (claude-code-ide--session-buffer-p (current-buffer)))
    (local-set-key (kbd "C-o") #'claude-code-ide-send-c-o)))

;;; Posframe Input Dialog

(defvar claude-code-ide--cursor-overlay nil
  "Overlay for displaying cursor position in posframe.")

(defvar claude-code-ide--mode-indicator-overlay nil
  "Overlay for displaying SKK mode indicator in posframe.")

(defun claude-code-ide--get-skk-mode-string ()
  "Get current SKK mode as a string."
  (cond
   ((not (and (boundp 'skk-mode) skk-mode)) "[A]")
   ((and (boundp 'skk-j-mode) skk-j-mode)
    (cond
     ((and (boundp 'skk-katakana) skk-katakana) "[カナ]")
     ((and (boundp 'skk-jisx0201-mode) skk-jisx0201-mode) "[ｶﾅ]")
     ((and (boundp 'skk-abbrev-mode) skk-abbrev-mode) "[aA]")
     (t "[あ]")))
   (t "[A]")))

(defun claude-code-ide--update-mode-indicator ()
  "Update SKK mode indicator overlay."
  (when claude-code-ide--mode-indicator-overlay
    (delete-overlay claude-code-ide--mode-indicator-overlay))
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (setq claude-code-ide--mode-indicator-overlay
          (make-overlay (point) (point)))
    (overlay-put claude-code-ide--mode-indicator-overlay 'after-string
                 (propertize (concat " " (claude-code-ide--get-skk-mode-string))
                            'face '(:foreground "#4CAF50" :weight bold)))))

(defun claude-code-ide--update-cursor-overlay ()
  "Update cursor position overlay and mode indicator."
  (when claude-code-ide--cursor-overlay
    (delete-overlay claude-code-ide--cursor-overlay))
  (setq claude-code-ide--cursor-overlay (make-overlay (point) (point)))
  (overlay-put claude-code-ide--cursor-overlay 'after-string
               (propertize "█" 'face '(:foreground "#4CAF50" :background "#4CAF50")))
  ;; モードインジケーターも更新
  (claude-code-ide--update-mode-indicator))

(defun claude-code-ide-read-string-in-posframe (prompt &optional initial-input)
  "Read string from user in a posframe with direct editing.
Supports SKK input. C-c C-c to submit and send, C-g to submit without sending."
  (require 'posframe)
  (let* ((buffer-name " *claude-code-input*")
         (buffer (get-buffer-create buffer-name))
         (result 'not-set)
         (should-send nil)
         (keymap (make-sparse-keymap))
         (skk-mode-was-on (and (boundp 'skk-mode) skk-mode)))

    ;; キーマップ設定
    (define-key keymap (kbd "C-c C-c")
      (lambda ()
        (interactive)
        (setq result (buffer-substring-no-properties
                     (save-excursion
                       (goto-char (point-min))
                       (search-forward "\n" nil t)
                       (point))
                     (point-max)))
        (setq should-send t)
        (exit-recursive-edit)))

    (define-key keymap (kbd "C-g")
      (lambda ()
        (interactive)
        (setq result (buffer-substring-no-properties
                     (save-excursion
                       (goto-char (point-min))
                       (search-forward "\n" nil t)
                       (point))
                     (point-max)))
        (setq should-send nil)
        (exit-recursive-edit)))

    ;; 通常の編集コマンドを有効化
    (set-keymap-parent keymap (current-global-map))

    (with-current-buffer buffer
      (erase-buffer)
      ;; プロンプト行とヘルプ（読み取り専用）
      (insert (propertize (concat prompt " ")
                         'face '(:foreground "#808080" :weight bold)
                         'read-only t
                         'rear-nonsticky t))
      (insert (propertize "(C-c C-c: Submit & Send | C-g: Submit)\n"
                         'face '(:foreground "#808080" :slant italic)
                         'read-only t
                         'rear-nonsticky t))
      ;; 入力エリア
      (when initial-input
        (insert initial-input))
      (use-local-map keymap)
      ;; SKKモードが有効だった場合は再度有効化
      (when skk-mode-was-on
        (skk-mode 1))
      ;; カーソル位置を示すオーバーレイを追加
      (add-hook 'post-command-hook #'claude-code-ide--update-cursor-overlay nil t))

    (unwind-protect
        (condition-case err
            (progn
              ;; posframe を表示（ボーダー付き、accept-focusを有効化）
              (posframe-show buffer
                            :position (point)
                            :poshandler #'posframe-poshandler-frame-center
                            :width 70
                            :height 10
                            :border-width 2
                            :border-color "#4CAF50"
                            :left-fringe 8
                            :right-fringe 8
                            :internal-border-width 2
                            :internal-border-color "#4CAF50"
                            :background-color (face-background 'default nil t)
                            :foreground-color (face-foreground 'default nil t)
                            :accept-focus t
                            :override-parameters '((alpha . 95)))

              ;; posframeにフォーカスを移動
              (let ((posframe (posframe--find-existing-posframe buffer)))
                (when posframe
                  (select-frame-set-input-focus posframe)
                  (with-selected-frame posframe
                    (select-window (frame-first-window posframe))
                    (with-current-buffer buffer
                      (goto-char (point-max))
                      ;; SKKモードを有効化して、自動的にひらがなモードに
                      (when (fboundp 'skk-mode)
                        (unless skk-mode
                          (skk-mode 1))
                        ;; ひらがなモードに切り替え
                        (when (and (boundp 'skk-j-mode) (not skk-j-mode))
                          (skk-j-mode-on)))
                      (recursive-edit))))))
          (quit
           (setq result nil)))

      ;; クリーンアップ
      (with-current-buffer buffer
        (remove-hook 'post-command-hook #'claude-code-ide--update-cursor-overlay t)
        (when claude-code-ide--cursor-overlay
          (delete-overlay claude-code-ide--cursor-overlay)
          (setq claude-code-ide--cursor-overlay nil))
        (when claude-code-ide--mode-indicator-overlay
          (delete-overlay claude-code-ide--mode-indicator-overlay)
          (setq claude-code-ide--mode-indicator-overlay nil)))
      (posframe-delete buffer-name))

    (if (eq result 'not-set)
        nil
      (cons result should-send))))

(defun claude-code-ide-send-prompt-with-posframe (orig-fun &optional prompt)
  "Override to use posframe for input. C-c C-c sends, C-g doesn't send."
  (if prompt
      ;; プログラム的に呼ばれた場合は元の動作
      (funcall orig-fun prompt)
    ;; インタラクティブに呼ばれた場合はposframeで入力
    (let* ((result (claude-code-ide-read-string-in-posframe "Claude prompt: "))
           (prompt-to-send (car-safe result))
           (should-send (cdr-safe result)))
      (when (and prompt-to-send (not (string-empty-p prompt-to-send)))
        (let ((buffer-name (claude-code-ide--get-buffer-name)))
          (when-let ((buffer (get-buffer buffer-name)))
            (with-current-buffer buffer
              (claude-code-ide--terminal-send-string prompt-to-send)
              ;; should-sendがtの場合のみEnterを送信
              (when should-send
                (sit-for 0.1)
                (claude-code-ide--terminal-send-return)))))))))

(defun claude-code-ide-setup-c-x-j-binding ()
  "Setup C-x j keybinding for Claude Code IDE buffers."
  (when (and (fboundp 'claude-code-ide--session-buffer-p)
             (claude-code-ide--session-buffer-p (current-buffer)))
    (local-set-key (kbd "C-x j") #'claude-code-ide-send-prompt)))

;;; Setup

(defun claude-code-ide-config-setup ()
  "Setup claude-code-ide custom configuration."
  ;; C-o binding for vterm and eat
  (dolist (hook '(vterm-mode-hook eat-mode-hook))
    (add-hook hook #'claude-code-ide-setup-c-o-binding))

  ;; C-x j binding for posframe input
  (dolist (hook '(vterm-mode-hook eat-mode-hook))
    (add-hook hook #'claude-code-ide-setup-c-x-j-binding))

  ;; Advice for posframe input dialog
  (advice-add 'claude-code-ide-send-prompt :around #'claude-code-ide-send-prompt-with-posframe))

(provide 'claude-code-ide-config)
;;; claude-code-ide-config.el ends here
