;;; posframe-ime-vterm.el --- Vterm integration for posframe-ime-input -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: phelrine
;; Keywords: convenience, terminals
;; Package-Requires: ((emacs "26.1") (posframe-ime-input "0.1.0") (vterm "0.0.1"))

;;; Commentary:

;; Vterm integration for posframe-ime-input.
;; Provides C-x j keybinding to input text via posframe and send to vterm.

;;; Code:

(require 'posframe-ime-input)

(defun posframe-ime-vterm-send (text)
  "Send TEXT to vterm terminal."
  (when (and (fboundp 'vterm-send-string)
             (eq major-mode 'vterm-mode))
    (vterm-send-string text)
    t))

(defun posframe-ime-vterm-input ()
  "Read input from posframe and send to vterm."
  (interactive)
  (unless (eq major-mode 'vterm-mode)
    (user-error "Not in a vterm buffer"))
  (when-let ((result (posframe-ime-input-read-string "Vterm input: ")))
    (posframe-ime-vterm-send result)))

(defun posframe-ime-vterm-setup ()
  "Setup posframe IME input for vterm."
  (local-set-key (kbd "C-x j") 'posframe-ime-vterm-input))

;;;###autoload
(defun posframe-ime-vterm-enable ()
  "Enable posframe IME input for vterm."
  (add-hook 'vterm-mode-hook #'posframe-ime-vterm-setup))

(provide 'posframe-ime-vterm)
;;; posframe-ime-vterm.el ends here
