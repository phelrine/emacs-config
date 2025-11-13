;;; posframe-ime-skk.el --- SKK integration for posframe-ime-input -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: phelrine
;; Keywords: convenience, i18n, japanese
;; Package-Requires: ((emacs "26.1") (posframe-ime-input "0.2.0") (ddskk "16.2"))

;;; Commentary:

;; SKK (Simple Kana to Kanji) integration for posframe-ime-input.
;; Provides automatic SKK activation, mode-aware cursor colors,
;; and visual mode indicators.

;;; Code:

(require 'posframe-ime-input)

;;; SKK Integration

(defun posframe-ime-skk--ensure-mode ()
  "Ensure SKK mode is loaded and activated."
  (when (or (featurep 'skk) (require 'skk nil t))
    (unless (and (boundp 'skk-mode) skk-mode)
      (skk-mode 1))
    ;; Switch to hiragana mode
    (when (and (boundp 'skk-j-mode)
               (not skk-j-mode)
               (fboundp 'skk-j-mode-on))
      (skk-j-mode-on))))

(defun posframe-ime-skk--get-mode-string ()
  "Get SKK mode indicator string."
  (if (and (boundp 'skk-mode) skk-mode)
      (cond
       ((and (boundp 'skk-katakana) skk-katakana) "[カナ]")
       ((and (boundp 'skk-jisx0201-mode) skk-jisx0201-mode) "[半角ｶﾅ]")
       ((and (boundp 'skk-abbrev-mode) skk-abbrev-mode) "[abbrev]")
       ((and (boundp 'skk-j-mode) skk-j-mode) "[かな]")
       (t "[SKK]"))
    nil))

(defun posframe-ime-skk--get-cursor-color ()
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

;;;###autoload
(defun posframe-ime-skk-enable ()
  "Enable SKK integration for posframe-ime-input."
  (add-hook 'posframe-ime-input-setup-hook #'posframe-ime-skk--ensure-mode)
  (setq posframe-ime-input-cursor-color-function #'posframe-ime-skk--get-cursor-color)
  (setq posframe-ime-input-mode-indicator-function #'posframe-ime-skk--get-mode-string))

;;;###autoload
(defun posframe-ime-skk-disable ()
  "Disable SKK integration for posframe-ime-input."
  (remove-hook 'posframe-ime-input-setup-hook #'posframe-ime-skk--ensure-mode)
  (setq posframe-ime-input-cursor-color-function nil)
  (setq posframe-ime-input-mode-indicator-function nil))

(provide 'posframe-ime-skk)
;;; posframe-ime-skk.el ends here
