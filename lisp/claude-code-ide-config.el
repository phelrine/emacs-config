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
(require 'claude-code-ide-session)
(require 'claude-code-ide-transient)
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

;;; Session Tiling

(defvar claude-code-ide-config--saved-window-configuration nil
  "Saved window configuration before tiling.")

(defvar claude-code-ide-config--tiled-p nil
  "Non-nil when sessions are currently tiled.")

(defun claude-code-ide-config--filtered-sessions (filter)
  "Return list of sessions matching FILTER with live buffers.
FILTER is one of: nil or `idle' (idle only), `working' (active only),
`all' (all sessions)."
  (let (result)
    (maphash
     (lambda (_id session)
       (when-let ((buf (claude-code-ide-session-buffer session)))
         (when (buffer-live-p buf)
           (let ((status (claude-code-ide-session-status session)))
             (when (pcase filter
                     ('all t)
                     ('working (eq status 'active))
                     (_ (eq status 'idle)))
               (push session result))))))
     claude-code-ide--sessions)
    (nreverse result)))

(defun claude-code-ide-tile-sessions (&optional filter)
  "Tile Claude Code session buffers in a grid layout.
FILTER is one of: nil or `idle' (idle only), `working' (active only),
`all' (all sessions)."
  (interactive)
  (let* ((sessions (claude-code-ide-config--filtered-sessions (or filter 'all)))
         (n (length sessions)))
    (cond
     ((= n 0)
      (message "No matching Claude Code sessions found."))
     (t
      (setq claude-code-ide-config--saved-window-configuration
            (current-window-configuration))
      (setq claude-code-ide-config--tiled-p t)
      ;; Delete claude-code-ide side windows first, since they have
      ;; no-delete-other-windows parameter and survive delete-other-windows
      (dolist (win (window-list))
        (when (and (window-parameter win 'window-side)
                   (claude-code-ide--session-buffer-p (window-buffer win)))
          (delete-window win)))
      (delete-other-windows)
      (let* ((cols (ceiling (sqrt n)))
             (rows (ceiling (/ (float n) cols)))
             (windows '()))
        ;; Split into rows
        (dotimes (r (1- rows))
          (let ((size (/ (window-total-height) (- rows r))))
            (split-window-below size)))
        ;; Split each row into columns
        (let ((row-windows '()))
          ;; Collect the first window of each row
          (let ((w (frame-first-window)))
            (dotimes (_ rows)
              (push w row-windows)
              (setq w (ignore-errors (window-in-direction 'below w))))
            (setq row-windows (nreverse row-windows)))
          ;; Split each row window into columns
          (let ((session-idx 0))
            (dotimes (r rows)
              (let* ((row-win (nth r row-windows))
                     (cols-in-row (min cols (- n session-idx))))
                (select-window row-win)
                (dotimes (c (1- cols-in-row))
                  (let ((size (/ (window-total-width row-win) (- cols-in-row c))))
                    (split-window-right size)))
                ;; Assign buffers to windows in this row
                (let ((w row-win))
                  (dotimes (_ cols-in-row)
                    (push w windows)
                    (setq w (ignore-errors (window-in-direction 'right w)))))
                (cl-incf session-idx cols-in-row)))))
        ;; Assign session buffers to windows
        (setq windows (nreverse windows))
        (cl-loop for session in sessions
                 for win in windows
                 do (set-window-buffer win (claude-code-ide-session-buffer session))))
      (message "Tiled %d Claude Code session(s)." n)))))

(defun claude-code-ide-untile-sessions ()
  "Restore window configuration from before tiling."
  (interactive)
  (if (not claude-code-ide-config--tiled-p)
      (message "Sessions are not currently tiled.")
    (set-window-configuration claude-code-ide-config--saved-window-configuration)
    (setq claude-code-ide-config--saved-window-configuration nil)
    (setq claude-code-ide-config--tiled-p nil)
    (message "Restored previous window layout.")))

(defun claude-code-ide-tile-idle-sessions ()
  "Tile idle Claude Code sessions."
  (interactive)
  (claude-code-ide-tile-sessions 'idle))

(defun claude-code-ide-tile-working-sessions ()
  "Tile working (active) Claude Code sessions."
  (interactive)
  (claude-code-ide-tile-sessions 'working))

(defun claude-code-ide-tile-all-sessions ()
  "Tile all Claude Code sessions."
  (interactive)
  (claude-code-ide-tile-sessions 'all))

(transient-define-prefix claude-code-ide-tile-menu ()
  "Tile Claude Code session buffers."
  ["Tile Sessions"
   ("i" "Tile idle sessions" claude-code-ide-tile-idle-sessions)
   ("w" "Tile working sessions" claude-code-ide-tile-working-sessions)
   ("a" "Tile all sessions" claude-code-ide-tile-all-sessions)
   ("u" "Untile (restore)" claude-code-ide-untile-sessions)])

;;; Side Window Fix

(defun claude-code-ide-config--delete-other-windows-advice (orig-fun &optional window interactive)
  "Handle `delete-other-windows' when the target is a side window.
When called from a side window, toggle side windows off, display the
buffer in the main area, then delete other windows normally."
  (let ((win (or window (selected-window))))
    (if (not (window-parameter win 'window-side))
        (funcall orig-fun window interactive)
      (let ((buf (window-buffer win)))
        (window-toggle-side-windows)
        (set-window-buffer (selected-window) buf)
        (funcall orig-fun nil interactive)))))

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
              :around #'claude-code-ide-config--dismiss-posframe-for-diff)

  ;; Fix "Cannot make side window the only window" error
  (advice-add 'delete-other-windows :around
              #'claude-code-ide-config--delete-other-windows-advice)

  ;; Add tile menu to Navigation group in transient menu
  (transient-append-suffix 'claude-code-ide-menu '(1 1 -1)
    '("T" "Tile sessions" claude-code-ide-tile-menu)))

(provide 'claude-code-ide-config)
;;; claude-code-ide-config.el ends here
