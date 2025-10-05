;;; startup-profiler.el --- Emacs startup time profiling -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: performance, startup
;; Version: 1.0.0

;;; Commentary:

;; This package provides startup time profiling for Emacs.
;; It measures the time taken by each package and operation during startup.
;;
;; Usage:
;;   (require 'startup-profiler)
;;   (startup-profiler-enable)
;;
;; After startup, run M-x startup-profiler-report to see detailed analysis.

;;; Code:

(require 'use-package-core nil t)

(defvar startup-profiler-start-time (current-time)
  "Time when init.el started loading.")

(defvar startup-profiler-log nil
  "List of startup time measurements (label . elapsed-seconds).")

(defvar startup-profiler-package-times (make-hash-table :test 'equal)
  "Hash table tracking package load times.")

(defvar startup-profiler-total-time nil
  "Total startup time captured at emacs-startup-hook.")

(defvar startup-profiler-enabled nil
  "Whether startup profiler is enabled.")

(defun startup-profiler-elapsed-time ()
  "Get elapsed time since init start in seconds."
  (float-time (time-subtract (current-time) startup-profiler-start-time)))

(defun startup-profiler-log (label)
  "Log current elapsed startup time for LABEL."
  (when startup-profiler-enabled
    (let ((elapsed (startup-profiler-elapsed-time)))
      (push (cons label elapsed) startup-profiler-log)
      (when (> elapsed 1.0)
        (message "[STARTUP] %s: %.3fs total" label elapsed)))))

(defmacro startup-profiler-measure (label &rest body)
  "Measure execution time of BODY and log with LABEL."
  (declare (indent 1))
  `(if startup-profiler-enabled
       (let ((start-time (current-time)))
         (prog1
             (progn ,@body)
           (let ((elapsed (float-time (time-subtract (current-time) start-time))))
             (push (cons ,label elapsed) startup-profiler-log)
             (when (> elapsed 0.1)
               (message "[STARTUP] %s: %.3fs" ,label elapsed)))))
     (progn ,@body)))

;;;###autoload
(defun startup-profiler-report ()
  "Show detailed startup time report."
  (interactive)
  (let* ((total-time (or startup-profiler-total-time
                         (startup-profiler-elapsed-time)))
         (sorted-log (sort (copy-sequence startup-profiler-log)
                          (lambda (a b) (> (cdr a) (cdr b)))))
         (measured-time (apply #'+ (mapcar #'cdr startup-profiler-log))))
    (with-current-buffer (get-buffer-create "*Startup Profiler Report*")
      (erase-buffer)
      (insert "=== Emacs Startup Profiler Report ===\n\n")
      (insert (format "Total startup time: %.3fs\n" total-time))
      (insert (format "Measured time:      %.3fs (%.1f%%)\n"
                      measured-time
                      (* 100 (/ measured-time total-time))))
      (insert (format "Emacs version:      %s\n" emacs-version))
      (insert (format "Packages measured:  %d\n\n" (length startup-profiler-log)))
      (insert "Slowest packages/operations (>0.01s):\n")
      (insert (make-string 70 ?-) "\n")
      (dolist (entry sorted-log)
        (when (> (cdr entry) 0.01)
          (insert (format "  %-55s: %6.3fs\n" (car entry) (cdr entry)))))
      (insert "\n" (make-string 70 ?-) "\n")
      (insert "\nTips for optimization:\n")
      (insert "  - Use :defer t for packages not needed at startup\n")
      (insert "  - Use :hook to load packages when needed\n")
      (insert "  - Move heavy configurations to :config section\n")
      (insert "  - Consider using :after for dependencies\n")
      (goto-char (point-min))
      (special-mode)
      (display-buffer (current-buffer)))))

(defun startup-profiler--record-package (name phase elapsed)
  "Record package NAME loading time for PHASE with ELAPSED seconds."
  (when (> elapsed 0.005)
    (let* ((key (format "%s:%s" name phase))
           (label (format "%-30s [%s]" name phase)))
      (push (cons label elapsed) startup-profiler-log)
      (puthash key elapsed startup-profiler-package-times)
      (when (> elapsed 0.1)
        (message "[STARTUP] %s: %.3fs" label elapsed)))))

(defvar startup-profiler--load-times (make-hash-table :test 'equal)
  "Hash table to track when packages start loading.")

(defun startup-profiler--advice-load (orig-fun file &optional noerror nomessage nosuffix must-suffix)
  "Advice to measure load time.
ORIG-FUN is the original function.
FILE, NOERROR, NOMESSAGE, NOSUFFIX, MUST-SUFFIX are load arguments."
  (if startup-profiler-enabled
      (let ((start-time (current-time))
            (result (funcall orig-fun file noerror nomessage nosuffix must-suffix)))
        (when result
          (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
                 (filename (if (stringp file) (file-name-nondirectory file) (format "%s" file))))
            (startup-profiler--record-package (format "load:%s" filename) "file" elapsed)))
        result)
    (funcall orig-fun file noerror nomessage nosuffix must-suffix)))

(defun startup-profiler--advice-require (orig-fun feature &optional filename noerror)
  "Advice to measure require loading time.
ORIG-FUN is the original function.
FEATURE, FILENAME, NOERROR are require arguments."
  (if (and startup-profiler-enabled (not (featurep feature)))
      (let ((start-time (current-time))
            (result (funcall orig-fun feature filename noerror)))
        (let ((elapsed (float-time (time-subtract (current-time) start-time))))
          (startup-profiler--record-package (format "require:%s" feature) "load" elapsed))
        result)
    (funcall orig-fun feature filename noerror)))

(defun startup-profiler--startup-finished ()
  "Hook function called after startup."
  (setq startup-profiler-total-time (startup-profiler-elapsed-time))
  (message "=== Emacs started in %.3fs ===" startup-profiler-total-time)
  (message "Run M-x startup-profiler-report for detailed analysis"))

;;;###autoload
(defun startup-profiler-enable ()
  "Enable startup profiling."
  (interactive)
  (setq startup-profiler-enabled t
        startup-profiler-start-time (current-time)
        startup-profiler-log nil)
  (clrhash startup-profiler-package-times)

  ;; Add advice to load and require for measuring
  (advice-add 'load :around #'startup-profiler--advice-load)
  (advice-add 'require :around #'startup-profiler--advice-require)

  ;; Add startup hook
  (add-hook 'emacs-startup-hook #'startup-profiler--startup-finished)

  ;; Log initial timestamp
  (startup-profiler-log "init-start"))

;;;###autoload
(defun startup-profiler-disable ()
  "Disable startup profiling."
  (interactive)
  (setq startup-profiler-enabled nil)
  (advice-remove 'load #'startup-profiler--advice-load)
  (advice-remove 'require #'startup-profiler--advice-require)
  (remove-hook 'emacs-startup-hook #'startup-profiler--startup-finished))

(provide 'startup-profiler)
;;; startup-profiler.el ends here
