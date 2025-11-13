;;; check-version-manager-asdf.el --- asdf version manager implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; asdf-specific implementation for version manager diagnostics

;;; Code:

(require 'check-version-manager-base)

;;; asdf Manager Structure

(cl-defstruct (asdf-manager (:include version-manager (type 'asdf))))

;;; Method Implementations

(cl-defmethod vm-name ((_manager asdf-manager))
  "Return the name of asdf manager."
  "asdf")

(cl-defmethod vm-env-vars ((_manager asdf-manager))
  "Return environment variables for asdf."
  '("ASDF_DIR" "ASDF_DATA_DIR" "PATH"))

(cl-defmethod vm-shim-pattern ((_manager asdf-manager))
  "Return shim pattern for asdf."
  "/\\.asdf/shims/")

(cl-defmethod vm-path-pattern ((_manager asdf-manager))
  "Return path pattern for asdf."
  "asdf")

(cl-defmethod vm-prefers-shims ((_manager asdf-manager))
  "Return t as asdf prefers shims."
  t)

(cl-defmethod vm-config-files ((_manager asdf-manager))
  "Return config files for asdf."
  (list (expand-file-name ".tool-versions" user-emacs-directory)))

(cl-defmethod vm-cmd ((_manager asdf-manager) &rest args)
  "Build asdf command with ARGS."
  (mapconcat 'identity (cons "asdf" args) " "))

(cl-defmethod vm-required-env-vars ((_manager asdf-manager))
  "Return required environment variables for asdf."
  '("ASDF_DIR"))

(cl-defmethod vm-install-url ((_manager asdf-manager))
  "Return installation URL for asdf."
  "https://asdf-vm.com")

(cl-defmethod vm-detect-p ((_type (eql 'asdf)))
  "Detect if asdf is available in the environment."
  (or (getenv "ASDF_DIR")
      (getenv "ASDF_DATA_DIR")
      (executable-find "asdf")
      (and (getenv "PATH") (string-match-p "asdf" (getenv "PATH")))))

(provide 'check-version-manager-asdf)
;;; check-version-manager-asdf.el ends here
