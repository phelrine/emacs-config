;;; check-version-manager-base.el --- Base classes for version managers -*- lexical-binding: t; -*-

;;; Commentary:
;; Base structure and generic method definitions for version managers
;; This file has no dependencies on implementation files

;;; Code:

(require 'cl-lib)

;;; Base Structure

(cl-defstruct version-manager
  "Base structure for version managers."
  type)

;;; Generic Method Definitions

(cl-defgeneric vm-name (manager)
  "Return the human-readable name of MANAGER.")

(cl-defgeneric vm-env-vars (manager)
  "Return list of environment variable names for MANAGER.")

(cl-defgeneric vm-shim-pattern (manager)
  "Return regex pattern to detect shims for MANAGER.")

(cl-defgeneric vm-path-pattern (manager)
  "Return regex pattern to detect MANAGER-managed paths.")

(cl-defgeneric vm-prefers-shims (manager)
  "Return t if MANAGER prefers shims over direct paths.")

(cl-defgeneric vm-config-files (manager)
  "Return list of possible config files for MANAGER in order of preference.")

(cl-defgeneric vm-cmd (manager &rest args)
  "Build command string for MANAGER with ARGS.")

(cl-defgeneric vm-required-env-vars (manager)
  "Return list of required environment variables for MANAGER.")

(cl-defgeneric vm-install-url (manager)
  "Return installation URL for MANAGER.")

(cl-defgeneric vm-detect-p (manager-type)
  "Return t if MANAGER-TYPE is detected in the environment.")

;;; Common Helper Functions

(defun vm-find-config (manager)
  "Find the active config file for MANAGER."
  (when manager
    (seq-find #'file-exists-p (vm-config-files manager))))

(defun vm-check-executable-status (manager _cmd path)
  "Check if executable at PATH is properly managed by MANAGER.
Returns a status string with checkmark or warning."
  (if (not manager)
      "NO ⚠"
    (let* ((shim-pattern (vm-shim-pattern manager))
           (path-pattern (vm-path-pattern manager))
           (is-shim (and shim-pattern (string-match-p shim-pattern path)))
           (is-managed (string-match-p path-pattern path))
           (prefers-shims (vm-prefers-shims manager)))
      (cond
       (is-shim "YES (shim) ✓")
       (is-managed (if prefers-shims "YES (direct) ⚠" "YES (direct) ✓"))
       (t "NO ⚠")))))

(provide 'check-version-manager-base)
;;; check-version-manager-base.el ends here
