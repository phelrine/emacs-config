;;; claude-code-ide-config.el --- Configuration for claude-code-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Shogo Nagasaka
;; Keywords: tools

;;; Commentary:

;; Custom configuration for claude-code-ide including:
;; - C-o keybinding to other-window (consistent with global binding)
;; - C-c o keybinding to send C-o to terminal (for verbose toggle)
;; - Posframe-based input dialog with SKK support using posframe-ime-input

;;; Code:

(require 'claude-code-ide)
(require 'posframe-ime-input)

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
  "Setup C-o keybinding for Claude Code IDE buffers only.
C-o is bound to `other-window' for consistency with global binding.
C-c o sends C-o to terminal (e.g., for Claude Code verbose toggle)."
  (when (and (fboundp 'claude-code-ide--session-buffer-p)
             (claude-code-ide--session-buffer-p (current-buffer)))
    (local-set-key (kbd "C-o") #'other-window)
    (local-set-key (kbd "C-c o") #'claude-code-ide-send-c-o)))

;;; Posframe Input Dialog

(defvar claude-code-ide--last-dismissed-prompt nil
  "Prompt text saved when posframe was dismissed by ediff guard.")

;; Define a minor mode to ensure our keybindings take precedence
(defvar claude-code-ide-posframe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x j") #'claude-code-ide-send-prompt)
    map)
  "Keymap for `claude-code-ide-posframe-mode'.")

(define-minor-mode claude-code-ide-posframe-mode
  "Minor mode for Claude Code IDE posframe input.
This mode's keymap takes precedence over local bindings."
  :lighter nil
  :keymap claude-code-ide-posframe-mode-map)

(defun claude-code-ide-send-prompt-with-posframe (orig-fun &optional prompt)
  "Override to use posframe for input. RET sends, S-RET for new line, C-g doesn't send."
  (if prompt
      ;; Called programmatically - use original behavior
      (funcall orig-fun prompt)
    ;; Called interactively - use posframe with callbacks
    (let* ((initial (prog1 claude-code-ide--last-dismissed-prompt
                      (setq claude-code-ide--last-dismissed-prompt nil)))
           (submitted nil)
           (text (posframe-ime-input-read-string
                  "Claude prompt: " initial
                  :help-text "(RET: Submit & Send | S-RET: New line | C-g: Submit)"
                  :on-submit (lambda (text) (setq submitted t) text)
                  :on-cancel #'identity
                  :on-dismiss (lambda (text)
                                (when (not (string-empty-p text))
                                  (setq claude-code-ide--last-dismissed-prompt text)
                                  (kill-new text)
                                  (message "プロンプトを保存しました (C-y で貼り付け可能)"))
                                nil)))
           (buffer-name (claude-code-ide--get-buffer-name)))
      (when (and text (not (string-empty-p text)))
        (when-let ((buffer (get-buffer buffer-name)))
          (with-current-buffer buffer
            (claude-code-ide--terminal-send-string text)
            (when submitted
              (sit-for 0.1)
              (claude-code-ide--terminal-send-return))))))))

(defun claude-code-ide-setup-posframe-mode ()
  "Enable posframe mode for Claude Code IDE buffers."
  (when (and (fboundp 'claude-code-ide--session-buffer-p)
             (claude-code-ide--session-buffer-p (current-buffer)))
    (claude-code-ide-posframe-mode 1)))

;;; Posframe Guard for Ediff

(defun claude-code-ide-config--dismiss-posframe-for-diff (orig-fn arguments)
  "Advice around `claude-code-ide-mcp-handle-open-diff'.
When posframe-ime-input is active, cancel it and switch to main frame
before starting ediff."
  (when (posframe-ime-input-active-p)
    (posframe-ime-input-cancel)
    ;; Switch to the main (non-child) frame
    (when-let ((main-frame (seq-find
                            (lambda (f)
                              (not (frame-parameter f 'parent-frame)))
                            (frame-list))))
      (select-frame-set-input-focus main-frame))
    ;; Wait for recursive-edit to finish via the timer-based abort
    (sit-for 0.2))
  (funcall orig-fn arguments))

;;; Setup

(defun claude-code-ide-config-setup ()
  "Setup claude-code-ide custom configuration."
  ;; C-o binding for vterm and eat
  (dolist (hook '(vterm-mode-hook eat-mode-hook))
    (add-hook hook #'claude-code-ide-setup-c-o-binding))

  ;; Enable posframe minor mode for claude-code buffers
  ;; Minor mode keymap takes precedence over local-set-key
  (dolist (hook '(vterm-mode-hook eat-mode-hook))
    (add-hook hook #'claude-code-ide-setup-posframe-mode))

  ;; Advice for posframe input dialog
  (advice-add 'claude-code-ide-send-prompt :around #'claude-code-ide-send-prompt-with-posframe)

  ;; Dismiss posframe before opening ediff
  (advice-add 'claude-code-ide-mcp-handle-open-diff
              :around #'claude-code-ide-config--dismiss-posframe-for-diff))

(provide 'claude-code-ide-config)
;;; claude-code-ide-config.el ends here
