;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Increase GC threshold during startup (will be lowered later in init.el)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation nil)

;; Package initialization optimization
(setq package-enable-at-startup nil)

;; Prefer newer .el over stale .elc (a stray outdated .elc in lisp/
;; once silently shadowed bug fixes)
(setq load-prefer-newer t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; LSP optimization
(setenv "LSP_USE_PLISTS" "true")

;; Emacs.app's launcher (Contents/MacOS/Emacs) exports LIBRARY_PATH pointing at
;; the bundled libgccjit dirs before exec'ing the arch binary. When that binary
;; is started directly the variable is absent, and native compilation dies with
;; "ld: library 'emutls_w' not found" -- libgccjit's link step cannot locate
;; libemutls_w.a. Redefining a C primitive (see the yes-or-no-p defalias in
;; init.el) needs a runtime-compiled trampoline, so this is not hypothetical.
(when (and (eq system-type 'darwin)
           (not (getenv "LIBRARY_PATH"))
           (string-prefix-p "Emacs-" invocation-name))
  (let ((dir (expand-file-name
              (format "lib-%s/libgccjit/" (substring invocation-name (length "Emacs-")))
              invocation-directory)))
    (when (file-directory-p dir)
      (setenv "LIBRARY_PATH"
              (mapconcat (lambda (sub) (expand-file-name sub dir))
                         '("." "apple-darwin" "sdk-libs") path-separator)))))

;;; early-init.el ends here
