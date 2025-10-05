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

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; LSP optimization
(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here
