;;; gotest-dape.el --- Debug Go tests using dape -*- lexical-binding: t; -*-
;;; Commentary:

(require 'gotest)
(require 'dape)

;;; Code:

(defun go-test-dape-get-directory-name-from-go-mod ()
  "Get the directory name where go.mod is located."
  (if-let ((file (buffer-file-name))
           (go-mod-root (locate-dominating-file (file-name-directory file) "go.mod")))
      (let ((relative-path (file-relative-name (file-name-directory file) go-mod-root)))
        (concat "./" relative-path))
    (error "No go.mod found in any ancestor directories or current buffer is not associated with a file")))

(defun go-test-dape-generate-arg-from-current-test ()
  "Generate arguments to run the Go test at the current cursor position."
  (if-let ((test-info (go-test--get-current-test-info))
           (test-name (cadr test-info)))
      (list "-test.run" (concat "^" test-name "$"))
    (error "No test selected")))

;;;###autoload
(defun dape-go-test-at-point ()
  "Debug Go test at point using dape."
  (interactive)
  (dape (dape--config-eval-1
         `(modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 fn (dape-config-autoport dape-config-tramp)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-cwd-fn
                 port :autoport
                 args go-test-dape-generate-arg-from-current-test
                 :request "launch"
                 :type "debug"
                 :mode "test"
                 :cwd "."
                 :program go-test-dape-get-directory-name-from-go-mod))))

(provide 'gotest-dape)

;;; gotest-dape.el ends here
