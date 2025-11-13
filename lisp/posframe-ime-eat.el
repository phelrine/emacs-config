;;; posframe-ime-eat.el --- Eat integration for posframe-ime-input -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: phelrine
;; Keywords: convenience, terminals
;; Package-Requires: ((emacs "26.1") (posframe-ime-input "0.1.0") (eat "0.0.1"))

;;; Commentary:

;; Eat integration for posframe-ime-input.
;; Provides C-x j keybinding to input text via posframe and send to eat.

;;; Code:

(require 'posframe-ime-input)

(defun posframe-ime-eat-send (text)
  "Send TEXT to eat terminal."
  (when (and (boundp 'eat-terminal)
             eat-terminal
             (fboundp 'eat-term-send-string)
             (eq major-mode 'eat-mode))
    (eat-term-send-string eat-terminal text)
    t))

(defun posframe-ime-eat-input ()
  "Read input from posframe and send to eat."
  (interactive)
  (unless (eq major-mode 'eat-mode)
    (user-error "Not in an eat buffer"))
  (when-let ((result (posframe-ime-input-read-string "Eat input: ")))
    (posframe-ime-eat-send result)))

(defun posframe-ime-eat-setup ()
  "Setup posframe IME input for eat."
  (local-set-key (kbd "C-x j") 'posframe-ime-eat-input))

;;;###autoload
(defun posframe-ime-eat-enable ()
  "Enable posframe IME input for eat."
  (add-hook 'eat-mode-hook #'posframe-ime-eat-setup))

(provide 'posframe-ime-eat)
;;; posframe-ime-eat.el ends here
