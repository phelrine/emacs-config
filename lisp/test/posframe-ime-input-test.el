;;; posframe-ime-input-test.el --- Tests for posframe-ime-input -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs --batch -L lisp -L straight/build/posframe \
;;     -l ert -l posframe-ime-input \
;;     -l lisp/test/posframe-ime-input-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'posframe-ime-input)

(defmacro posframe-ime-input-test--with-dialog-active (&rest body)
  "Run BODY with the dialog marked active and the input buffer created."
  `(let ((posframe-ime-input--active t))
     (unwind-protect
         (progn
           (get-buffer-create posframe-ime-input--buffer-name)
           ,@body)
       (when-let ((buf (get-buffer posframe-ime-input--buffer-name)))
         (kill-buffer buf)))))

(ert-deftest posframe-ime-input-test-no-dismiss-when-inactive ()
  "Selection changes while the dialog is not active are ignored."
  (let ((posframe-ime-input--active nil))
    (should-not (posframe-ime-input--should-dismiss-p (selected-window)))))

(ert-deftest posframe-ime-input-test-no-dismiss-when-input-buffer-selected ()
  "No dismiss while the selected window still shows the input buffer."
  (posframe-ime-input-test--with-dialog-active
   (let ((win (selected-window))
         (orig (window-buffer (selected-window))))
     (unwind-protect
         (progn
           (set-window-buffer win (get-buffer posframe-ime-input--buffer-name))
           (should-not (posframe-ime-input--should-dismiss-p win)))
       (set-window-buffer win orig)))))

(ert-deftest posframe-ime-input-test-dismiss-when-other-buffer-selected ()
  "Dismiss when the selected window shows some other buffer.
This is the MCP-handler case: an async `find-file' steals the
selection away from the posframe input buffer."
  (posframe-ime-input-test--with-dialog-active
   (let ((win (selected-window))
         (orig (window-buffer (selected-window)))
         (other (get-buffer-create "*posframe-ime-input-test-other*")))
     (unwind-protect
         (progn
           (set-window-buffer win other)
           (should (posframe-ime-input--should-dismiss-p win)))
       (set-window-buffer win orig)
       (kill-buffer other)))))

(ert-deftest posframe-ime-input-test-no-dismiss-for-minibuffer ()
  "Minibuffer selection (e.g. SKK dictionary registration) is ignored."
  (posframe-ime-input-test--with-dialog-active
   (should-not (posframe-ime-input--should-dismiss-p (minibuffer-window)))))

(provide 'posframe-ime-input-test)
;;; posframe-ime-input-test.el ends here
