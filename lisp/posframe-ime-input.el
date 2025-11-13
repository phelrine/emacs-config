;;; posframe-ime-input.el --- IME-friendly input dialog using posframe -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: phelrine
;; Keywords: convenience, frames, i18n
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (posframe "1.0"))

;;; Commentary:

;; This package provides an IME-friendly input dialog using posframe.
;; It's particularly useful for read-only or terminal buffers (like vterm, eat)
;; where you can't directly edit text but want to use Japanese IME like SKK.
;;
;; Features:
;; - Posframe-based input dialog with multi-line support
;; - SKK mode integration with mode-aware cursor colors
;; - Customizable callbacks for submit/cancel actions
;; - Visual mode indicator for SKK states
;;
;; Usage:
;;   (require 'posframe-ime-input)
;;   ;; Simple usage
;;   (let ((result (posframe-ime-input-read-string "Prompt: ")))
;;     (when result
;;       (do-something-with result)))
;;
;;   ;; With callbacks
;;   (posframe-ime-input-read-string
;;     "Prompt: " nil
;;     :on-submit (lambda (text) (cons text t))
;;     :on-cancel (lambda (text) (cons text nil)))

;;; Code:

(require 'posframe)
(require 'cl-lib)

;;; Customization

(defgroup posframe-ime-input nil
  "IME-friendly input dialog using posframe."
  :group 'convenience
  :prefix "posframe-ime-input-")

(defcustom posframe-ime-input-enable-skk t
  "Enable SKK mode integration when available."
  :type 'boolean
  :group 'posframe-ime-input)

(defcustom posframe-ime-input-width 70
  "Width of the posframe input dialog."
  :type 'integer
  :group 'posframe-ime-input)

(defcustom posframe-ime-input-height 10
  "Height of the posframe input dialog."
  :type 'integer
  :group 'posframe-ime-input)

;;; Internal Variables

(defvar posframe-ime-input--cursor-overlay nil
  "Overlay for displaying cursor position in posframe.")

(defvar posframe-ime-input--mode-indicator-overlay nil
  "Overlay for displaying SKK mode indicator.")

;;; SKK Integration

(defun posframe-ime-input--ensure-skk-mode ()
  "Ensure SKK mode is loaded and activated."
  (when (and posframe-ime-input-enable-skk
             (or (featurep 'skk) (require 'skk nil t)))
    (unless (and (boundp 'skk-mode) skk-mode)
      (skk-mode 1))
    ;; Switch to hiragana mode
    (when (and (boundp 'skk-j-mode)
               (not skk-j-mode)
               (fboundp 'skk-j-mode-on))
      (skk-j-mode-on))))

(defun posframe-ime-input--get-skk-mode-string ()
  "Get SKK mode indicator string."
  (if (and (boundp 'skk-mode) skk-mode)
      (cond
       ((and (boundp 'skk-katakana) skk-katakana) "[カナ]")
       ((and (boundp 'skk-jisx0201-mode) skk-jisx0201-mode) "[半角ｶﾅ]")
       ((and (boundp 'skk-abbrev-mode) skk-abbrev-mode) "[abbrev]")
       ((and (boundp 'skk-j-mode) skk-j-mode) "[かな]")
       (t "[SKK]"))
    ""))

(defun posframe-ime-input--get-cursor-color ()
  "Get cursor color based on SKK mode."
  (cond
   ((not (and (boundp 'skk-mode) skk-mode))
    ;; SKK disabled
    (or (and (boundp 'skk-cursor-latin-color) skk-cursor-latin-color)
        "white"))
   ((and (boundp 'skk-j-mode) skk-j-mode)
    ;; SKK enabled, choose color based on mode
    (cond
     ((and (boundp 'skk-katakana) skk-katakana)
      (or (and (boundp 'skk-cursor-katakana-color) skk-cursor-katakana-color) "red"))
     ((and (boundp 'skk-jisx0201-mode) skk-jisx0201-mode)
      (or (and (boundp 'skk-cursor-jisx0201-color) skk-cursor-jisx0201-color) "pink"))
     ((and (boundp 'skk-abbrev-mode) skk-abbrev-mode)
      (or (and (boundp 'skk-cursor-abbrev-color) skk-cursor-abbrev-color) "orange"))
     (t
      (or (and (boundp 'skk-cursor-hiragana-color) skk-cursor-hiragana-color) "coral"))))
   (t
    (or (and (boundp 'skk-cursor-latin-color) skk-cursor-latin-color) "white"))))

;;; Visual Updates

(defun posframe-ime-input--update-mode-indicator ()
  "Update SKK mode indicator overlay."
  (when posframe-ime-input--mode-indicator-overlay
    (delete-overlay posframe-ime-input--mode-indicator-overlay))
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (setq posframe-ime-input--mode-indicator-overlay
          (make-overlay (point) (point)))
    (overlay-put posframe-ime-input--mode-indicator-overlay 'after-string
                 (propertize (concat " " (posframe-ime-input--get-skk-mode-string))
                            'face '(:foreground "#4CAF50" :weight bold)))))

(defun posframe-ime-input--update-cursor-overlay ()
  "Update cursor position overlay and mode indicator."
  (when posframe-ime-input--cursor-overlay
    (delete-overlay posframe-ime-input--cursor-overlay))
  ;; Thin bar cursor
  (setq posframe-ime-input--cursor-overlay (make-overlay (point) (point)))
  (let ((cursor-color (posframe-ime-input--get-cursor-color)))
    (overlay-put posframe-ime-input--cursor-overlay 'before-string
                 (propertize " " 'display `(space :width (1))
                            'face `(:background ,cursor-color))))
  ;; Update mode indicator
  (posframe-ime-input--update-mode-indicator))

;;; Main Function

(cl-defun posframe-ime-input-read-string (prompt &optional initial-input
                                                 &key on-submit on-cancel help-text
                                                 (allow-newline t))
  "Read string from user in a posframe with IME support.

PROMPT is displayed at the top of the dialog.
INITIAL-INPUT is the initial content of the input field.

Keyword arguments:
  :on-submit FUNC    - Called with input text when RET is pressed.
                       Return value becomes the result. Default: identity.
  :on-cancel FUNC    - Called with input text when C-g is pressed.
                       Return value becomes the result. Default: (lambda (_) nil).
  :help-text TEXT    - Help text shown below prompt.
                       Default: \"(RET: Submit | S-RET: New line | C-g: Cancel)\"
  :allow-newline BOOL - Allow S-RET to insert newline. Default: t.

Returns whatever the callback returns.

Default behavior (no callbacks):
  RET       - Returns the input text
  C-g       - Returns nil
  S-RET     - Insert newline (if :allow-newline is t)"
  (require 'posframe)
  (require 'cl-lib)
  (let* ((buffer-name " *posframe-ime-input*")
         (buffer (get-buffer-create buffer-name))
         (result 'not-set)
         (keymap (make-sparse-keymap))
         (on-submit-fn (or on-submit #'identity))
         (on-cancel-fn (or on-cancel (lambda (_) nil)))
         (help-text (or help-text
                        (if allow-newline
                            "(RET: Submit | S-RET: New line | C-g: Cancel)"
                          "(RET: Submit | C-g: Cancel)"))))

    ;; Keymap setup
    (define-key keymap (kbd "RET")
      (lambda ()
        (interactive)
        (let ((text (with-current-buffer buffer
                      (buffer-substring-no-properties
                       (save-excursion
                         (goto-char (point-min))
                         (search-forward "\n" nil t)
                         (point))
                       (point-max)))))
          (setq result (funcall on-submit-fn text)))
        (exit-recursive-edit)))

    (when allow-newline
      (define-key keymap (kbd "S-RET")
        (lambda () (interactive) (insert "\n")))
      (define-key keymap (kbd "<S-return>")
        (lambda () (interactive) (insert "\n")))
      (define-key keymap (kbd "S-<return>")
        (lambda () (interactive) (insert "\n"))))

    (define-key keymap (kbd "C-g")
      (lambda ()
        (interactive)
        (let ((text (with-current-buffer buffer
                      (buffer-substring-no-properties
                       (save-excursion
                         (goto-char (point-min))
                         (search-forward "\n" nil t)
                         (point))
                       (point-max)))))
          (setq result (funcall on-cancel-fn text)))
        (exit-recursive-edit)))

    ;; Enable normal editing commands
    (set-keymap-parent keymap (current-global-map))

    ;; Buffer setup
    (with-current-buffer buffer
      (erase-buffer)
      (insert (propertize (concat prompt " ")
                         'face '(:foreground "#808080" :weight bold)
                         'read-only t
                         'rear-nonsticky t))
      (insert (propertize (concat help-text "\n")
                         'face '(:foreground "#808080" :slant italic)
                         'read-only t
                         'rear-nonsticky t))
      (when initial-input
        (insert initial-input))
      (use-local-map keymap)
      ;; Enable SKK mode if available
      (posframe-ime-input--ensure-skk-mode)
      ;; Cursor overlay
      (add-hook 'post-command-hook #'posframe-ime-input--update-cursor-overlay nil t))

    ;; Display posframe
    (unwind-protect
        (condition-case nil
            (progn
              (posframe-show buffer
                            :position (point)
                            :poshandler #'posframe-poshandler-frame-center
                            :width posframe-ime-input-width
                            :height posframe-ime-input-height
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
              ;; Move focus to posframe
              (let ((posframe (posframe--find-existing-posframe buffer)))
                (when posframe
                  (select-frame-set-input-focus posframe)
                  (with-selected-frame posframe
                    (select-window (frame-first-window posframe))
                    (with-current-buffer buffer
                      (goto-char (point-max))
                      (posframe-ime-input--ensure-skk-mode)
                      (recursive-edit))))))
          (quit
           (let ((text (with-current-buffer buffer
                         (buffer-substring-no-properties
                          (save-excursion
                            (goto-char (point-min))
                            (search-forward "\n" nil t)
                            (point))
                          (point-max)))))
             (setq result (funcall on-cancel-fn text)))))
      ;; Clean up overlays and hooks
      (with-current-buffer buffer
        (remove-hook 'post-command-hook #'posframe-ime-input--update-cursor-overlay t)
        (when posframe-ime-input--cursor-overlay
          (delete-overlay posframe-ime-input--cursor-overlay)
          (setq posframe-ime-input--cursor-overlay nil))
        (when posframe-ime-input--mode-indicator-overlay
          (delete-overlay posframe-ime-input--mode-indicator-overlay)
          (setq posframe-ime-input--mode-indicator-overlay nil)))
      ;; Delete posframe completely
      (posframe-delete buffer))

    ;; Return result
    (if (eq result 'not-set)
        nil
      result)))

;;; Interactive Commands

;;;###autoload
(defun posframe-ime-input-test ()
  "Test the posframe IME input dialog."
  (interactive)
  (let ((result (posframe-ime-input-read-string "Test input: " "Initial text")))
    (if result
        (message "Input: %s" result)
      (message "Cancelled"))))

(provide 'posframe-ime-input)
;;; posframe-ime-input.el ends here
