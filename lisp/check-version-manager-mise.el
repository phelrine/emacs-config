;;; check-version-manager-mise.el --- mise version manager implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; mise-specific implementation for version manager diagnostics

;;; Code:

(require 'check-version-manager-base)

;;; mise Manager Structure

(cl-defstruct (mise-manager (:include version-manager (type 'mise))))

;;; Method Implementations

(cl-defmethod vm-name ((_manager mise-manager))
  "Return the name of mise manager."
  "mise")

(cl-defmethod vm-env-vars ((_manager mise-manager))
  "Return environment variables for mise."
  '("MISE_DATA_DIR" "MISE_CONFIG_DIR" "PATH"))

(cl-defmethod vm-shim-pattern ((_manager mise-manager))
  "Return shim pattern for mise."
  "/mise/shims/")

(cl-defmethod vm-path-pattern ((_manager mise-manager))
  "Return path pattern for mise."
  "mise")

(cl-defmethod vm-prefers-shims ((_manager mise-manager))
  "Return nil as mise prefers direct paths."
  nil)

(cl-defmethod vm-config-files ((_manager mise-manager))
  "Return config files for mise."
  (list (expand-file-name ".mise.toml" user-emacs-directory)
        (expand-file-name ".tool-versions" user-emacs-directory)))

(cl-defmethod vm-cmd ((_manager mise-manager) &rest args)
  "Build mise command with ARGS."
  (mapconcat 'identity (cons "mise" args) " "))

(cl-defmethod vm-required-env-vars ((_manager mise-manager))
  "Return required environment variables for mise."
  '("MISE_DATA_DIR"))

(cl-defmethod vm-install-url ((_manager mise-manager))
  "Return installation URL for mise."
  "https://mise.jdx.dev")

(cl-defmethod vm-detect-p ((_type (eql 'mise)))
  "Detect if mise is available in the environment."
  (or (getenv "MISE_DATA_DIR")
      (getenv "MISE_CONFIG_DIR")
      (executable-find "mise")
      (and (getenv "PATH") (string-match-p "mise" (getenv "PATH")))))

(provide 'check-version-manager-mise)
;;; check-version-manager-mise.el ends here
