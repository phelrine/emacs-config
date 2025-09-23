;;; codex-transient.el --- Transient menu for Codex IDE -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: phelrine
;; Keywords: tools, ai, codex
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (codex-ide "0.1.0"))

;;; Commentary:

;; This package provides Transient menu interface for Codex IDE integration.

;;; Code:

(require 'transient)
(require 'codex-ide)

;;; Session Management Commands

(transient-define-suffix codex-ide-transient-start ()
  "Start a new Codex session."
  :description "Start new session"
  :key "s"
  (interactive)
  (call-interactively #'codex-ide-start))

(transient-define-suffix codex-ide-transient-resume ()
  "Resume the last Codex session."
  :description "Resume last session"
  :key "r"
  (interactive)
  (call-interactively #'codex-ide-resume))

(transient-define-suffix codex-ide-transient-resume-with-selection ()
  "Resume a Codex session with conversation selection."
  :description "Resume with selection"
  :key "R"
  (interactive)
  (call-interactively #'codex-ide-resume-with-selection))

(transient-define-suffix codex-ide-transient-stop ()
  "Stop the current Codex session."
  :description "Stop current session"
  :key "k"
  (interactive)
  (call-interactively #'codex-ide-stop))

(transient-define-suffix codex-ide-transient-switch ()
  "Switch to Codex buffer."
  :description "Switch to buffer"
  :key "b"
  (interactive)
  (call-interactively #'codex-ide-switch-to-buffer))

(transient-define-suffix codex-ide-transient-toggle ()
  "Toggle Codex buffer visibility."
  :description "Toggle buffer visibility"
  :key "t"
  (interactive)
  (call-interactively #'codex-ide-toggle))

(transient-define-suffix codex-ide-transient-list ()
  "List all active sessions."
  :description "List active sessions"
  :key "l"
  (interactive)
  (call-interactively #'codex-ide-list-sessions))

;;; Configuration Commands

(transient-define-suffix codex-ide-transient-set-model ()
  "Set default model for new sessions."
  :description "Set default model"
  :key "m"
  (interactive)
  (let ((model (read-string "Model name: " codex-ide-default-model)))
    (setq codex-ide-default-model (if (string-empty-p model) nil model))
    (message "Default model set to: %s" (or codex-ide-default-model "CLI default"))))

(transient-define-suffix codex-ide-transient-toggle-sandbox ()
  "Toggle sandbox mode."
  :description "Toggle sandbox mode"
  :key "x"
  (interactive)
  (setq codex-ide-sandbox-mode (not codex-ide-sandbox-mode))
  (message "Sandbox mode: %s" (if codex-ide-sandbox-mode "enabled" "disabled")))

(transient-define-suffix codex-ide-transient-set-command ()
  "Set Codex CLI command."
  :description "Set CLI command"
  :key "c"
  (interactive)
  (let ((command (read-string "Codex command: " codex-ide-command)))
    (setq codex-ide-command command)
    (message "Codex command set to: %s" command)))

(transient-define-suffix codex-ide-transient-set-extra-args ()
  "Set extra CLI arguments."
  :description "Set extra arguments"
  :key "a"
  (interactive)
  (let ((args-string (read-string "Extra arguments (space-separated): "
                                  (string-join codex-ide-extra-args " "))))
    (setq codex-ide-extra-args (if (string-empty-p args-string)
                                   nil
                                 (split-string args-string)))
    (message "Extra arguments: %s" (or (string-join codex-ide-extra-args " ") "none"))))

(transient-define-suffix codex-ide-transient-toggle-auto-switch ()
  "Toggle auto-switch to buffer."
  :description "Toggle auto-switch"
  :key "w"
  (interactive)
  (setq codex-ide-auto-switch-to-buffer (not codex-ide-auto-switch-to-buffer))
  (message "Auto-switch to buffer: %s" (if codex-ide-auto-switch-to-buffer "enabled" "disabled")))

;;; Debug Commands

(transient-define-suffix codex-ide-transient-show-debug ()
  "Show debug buffer."
  :description "Show debug buffer"
  :key "d"
  (interactive)
  (call-interactively #'codex-ide-show-debug))

(transient-define-suffix codex-ide-transient-version ()
  "Show Codex CLI version."
  :description "Show CLI version"
  :key "v"
  (interactive)
  (call-interactively #'codex-ide-version))

(transient-define-suffix codex-ide-transient-check-cli ()
  "Check Codex CLI availability."
  :description "Check CLI availability"
  :key "?"
  (interactive)
  (if (executable-find codex-ide-command)
      (message "Codex CLI found at: %s" (executable-find codex-ide-command))
    (message "Codex CLI not found in PATH")))

;;; Settings Submenu

(transient-define-prefix codex-ide-settings-menu ()
  "Codex IDE settings menu."
  :info-manual "(codex-ide) Settings"
  ["Configuration"
   [("m" codex-ide-transient-set-model)
    ("x" codex-ide-transient-toggle-sandbox)
    ("c" codex-ide-transient-set-command)]
   [("a" codex-ide-transient-set-extra-args)
    ("w" codex-ide-transient-toggle-auto-switch)]]
  ["Current Settings"
   :class transient-row
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      'codex-ide-settings-menu
      `[("Model:" ,(or codex-ide-default-model "CLI default"))
        ("Sandbox:" ,(if codex-ide-sandbox-mode "enabled" "disabled"))
        ("Command:" ,codex-ide-command)
        ("Auto-switch:" ,(if codex-ide-auto-switch-to-buffer "enabled" "disabled"))]))])

;;; Debug Submenu

(transient-define-prefix codex-ide-debug-menu ()
  "Codex IDE debug menu."
  :info-manual "(codex-ide) Debug"
  ["Debug & Info"
   [("d" codex-ide-transient-show-debug)
    ("v" codex-ide-transient-version)
    ("?" codex-ide-transient-check-cli)]])

;;; Main Menu

;;;###autoload
(transient-define-prefix codex-ide-menu ()
  "Main Codex IDE menu."
  :info-manual "(codex-ide) Usage"
  ["Session Management"
   [("s" codex-ide-transient-start)
    ("r" codex-ide-transient-resume)
    ("R" codex-ide-transient-resume-with-selection)
    ("k" codex-ide-transient-stop)]
   [("b" codex-ide-transient-switch)
    ("t" codex-ide-transient-toggle)
    ("l" codex-ide-transient-list)]]
  ["Configuration & Debug"
   [("C" "Settings" codex-ide-settings-menu)
    ("D" "Debug" codex-ide-debug-menu)]]
  (interactive)
  (transient-setup 'codex-ide-menu))

(provide 'codex-transient)

;;; codex-transient.el ends here