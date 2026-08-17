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

;;; Cursor

(ert-deftest posframe-ime-input-test-asks-posframe-for-a-cursor ()
  "Posframe hides the cursor and pins the window to position 0 unless
told otherwise, which leaves the user typing with nothing to aim at and
the caret in front of INITIAL-INPUT."
  (let (args)
    (cl-letf (((symbol-function 'posframe-show)
               (lambda (_buffer &rest rest) (setq args rest) nil))
              ((symbol-function 'posframe--find-existing-posframe) #'ignore)
              ((symbol-function 'posframe-delete) #'ignore))
      (posframe-ime-input-read-string "Prompt: " "既存の入力"))
    (should (plist-get args :cursor))
    ;; Past the prompt line, the help line and the initial input.
    (should (> (plist-get args :window-point) (length "既存の入力")))))

(ert-deftest posframe-ime-input-test-cursor-color-paints-the-frame ()
  "The mode colour goes on the real cursor now, not on a fake overlay."
  (cl-letf (((symbol-function 'posframe--find-existing-posframe)
             (lambda (&rest _) (selected-frame)))
            (posframe-ime-input-cursor-color-function (lambda () "coral")))
    (posframe-ime-input--apply-cursor-color)
    (should (equal (frame-parameter (selected-frame) 'cursor-color) "coral"))))

(provide 'posframe-ime-input-test)
;;; posframe-ime-input-test.el ends here
