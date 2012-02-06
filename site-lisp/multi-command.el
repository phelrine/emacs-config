;; SETTING
;; (require 'multi-command)
;; (global-set-key (kbd "C-f") (define-multi-command mcmd-c-f forward-char forward-word))
;; (global-set-key (kbd "C-b") (define-multi-command mcmd-c-b backward-char backward-word))
;; (global-set-key (kbd "C-q") 'mcmd-next-mode)

(eval-when-compile (require 'cl))

(defstruct mcmd-command mode command-list)
(defvar mcmd-hash-table (make-hash-table :test 'equal))
(defvar mcmd-reset-mode nil)

(defun mcmd-get-hash-value(name)
  (gethash (symbol-name name) mcmd-hash-table))

(defun mcmd-get-mode(name)
  (if (mcmd-get-hash-value name)
      (mcmd-command-mode (mcmd-get-hash-value name))))

(defun mcmd-set-mode(name mode)
  (if (mcmd-get-hash-value name)
      (setf (mcmd-command-mode (mcmd-get-hash-value name)) mode)))

(defun mcmd-change-mode(mode)
  (interactive "p")
  (mcmd-set-mode last-command mode))

(defun mcmd-next-mode()
  (interactive)
  (mcmd-set-mode last-command (+ (mcmd-get-mode last-command) 1)))

(defmacro define-multi-command (name &rest commands)
  `(progn
     (puthash (symbol-name ',name) (make-mcmd-command :mode 0 :command-list ',commands) mcmd-hash-table)
     (defun ,name ()
       (interactive)
       (if mcmd-reset-mode
           (and (not (eq last-command this-command))
                (not (eq last-command #'mcmd-next-mode))
                (not (eq last-command #'mcmd-change-mode))
                (mcmd-set-mode this-command 0)))
       (call-interactively
        (nth (mod (mcmd-get-mode ',name) ,(length commands)) ',commands)))))

(defvar anything-c-source-multi-command
    '((name . "multi-command")
      (candidates . (lambda ()
                      (mapcar #'(lambda (e)
                                  (let ((key (car e)) (value (cdr e)))
                                    (concat key " => "
                                            (symbol-name (nth (mcmd-command-mode value)
                                                              (mcmd-command-command-list value))))))
                              (loop for k being the hash-keys in mcmd-hash-table using (hash-values v)
                                    collect (cons k v)))))
      (action . anything-mcmd-change-mode)))

(defun anything-mcmd-change-mode (mode-string)
  (let* ((key (car (split-string mode-string)))
         (cmd (gethash key mcmd-hash-table))
         (cmd-list  (mcmd-command-command-list cmd)))
    (anything-other-buffer
     `((name . "change mode")
       (candidates . ,cmd-list)
       (action . (lambda (func) (mcmd-set-mode (make-symbol ,key) (position (intern func) ',cmd-list)))))
     "*anything multi-command*")))


(defun anything-multi-command()
  (interactive)
  (if (fboundp 'anything-other-buffer)
      (anything-other-buffer '(anything-c-source-multi-command) "*anything multi-command*")))

(provide 'multi-command)
