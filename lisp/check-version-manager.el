;;; check-version-manager.el --- Check version manager environment in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Script to verify asdf or mise environment is correctly loaded in Emacs
;; Provides diagnostic tools and automatic fixes for common version manager issues
;; Supports both asdf and mise (previously rtx)

;;; Code:

(require 'shell)
(require 'check-version-manager-base)
(require 'check-version-manager-asdf)
(require 'check-version-manager-mise)

;;; Auto-detection

(defun version-manager-detect ()
  "Detect which version manager is being used.
Returns an instance of asdf-manager, mise-manager, or nil."
  (cond
   ;; Check for mise first (newer tool, might coexist with asdf)
   ((vm-detect-p 'mise) (make-mise-manager))
   ;; Check for asdf
   ((vm-detect-p 'asdf) (make-asdf-manager))
   ;; Neither detected
   (t nil)))

;;; Additional Helper Functions

(defun version-manager-find-copilot-server ()
  "Find copilot-language-server in PATH or in Emacs cache.
Returns (PATH . SOURCE) where SOURCE is \\='path, \\='emacs-cache, or nil."
  (let ((path-location (executable-find "copilot-language-server"))
        (cache-location (expand-file-name ".cache/copilot/bin/copilot-language-server"
                                         user-emacs-directory)))
    (cond
     (path-location (cons path-location 'path))
     ((file-exists-p cache-location) (cons cache-location 'emacs-cache))
     (t nil))))

;;; Main Diagnostic Function

;;;###autoload
(defun check-version-manager-environment ()
  "Check if version manager environment is correctly loaded in Emacs.
Supports both asdf and mise."
  (interactive)
  (let* ((manager (version-manager-detect))
         (manager-name (if manager (vm-name manager) "version manager"))
         (results '()))
    (with-current-buffer (get-buffer-create "*version-manager-check*")
      (erase-buffer)
      (insert (format "=== %s Environment Check ===\n\n"
                      (upcase manager-name)))

      (when (null manager)
        (insert "⚠ WARNING: No version manager detected!\n")
        (insert "Expected to find either asdf or mise.\n\n"))

      ;; Check environment variables
      (insert "## Environment Variables\n")
      (when manager
        (dolist (var (vm-env-vars manager))
          (let ((value (getenv var)))
            (insert (format "%-20s: %s\n" var (or value "NOT SET")))
            (push (cons var value) results))))
      (insert "\n")

      ;; Check PATH contains version manager
      (when manager
        (insert (format "## PATH contains %s?\n" manager-name))
        (let* ((path (getenv "PATH"))
               (has-manager (and path (string-match-p (vm-path-pattern manager) path))))
          (insert (format "Result: %s\n" (if has-manager "YES ✓" "NO ✗")))
          (when has-manager
            (insert (format "%s paths in PATH:\n" manager-name))
            (dolist (p (split-string path ":" t))
              (when (string-match-p (vm-path-pattern manager) p)
                (insert (format "  - %s\n" p))))))
        (insert "\n"))

      ;; Check executable-find
      (insert "## Executable Paths\n")
      (let ((executables (if manager
                            (list "node" "npm" (vm-name manager))
                          '("node" "npm"))))
        (dolist (cmd executables)
          (let ((path (executable-find cmd)))
            (insert (format "%-25s: %s\n" cmd (or path "NOT FOUND")))
            (push (cons cmd path) results))))
      ;; Special handling for copilot-language-server
      (let ((copilot-info (version-manager-find-copilot-server)))
        (if copilot-info
            (let ((path (car copilot-info))
                  (source (cdr copilot-info)))
              (insert (format "%-25s: %s\n" "copilot-language-server" path))
              (insert (format "%-25s  (source: %s)\n" ""
                             (pcase source
                               ('path "PATH")
                               ('emacs-cache "Emacs cache")
                               (_ "unknown"))))
              (push (cons "copilot-language-server" path) results))
          (insert (format "%-25s: %s\n" "copilot-language-server" "NOT FOUND"))))
      (insert "\n")

      ;; Check if executables are managed
      (when manager
        (insert (format "## Are executables %s managed?\n" manager-name))
        (dolist (cmd '("node" "npm" "copilot-language-server"))
          (let ((path (executable-find cmd)))
            (when path
              (let ((status (vm-check-executable-status manager cmd path)))
                (insert (format "%-25s: %s\n" cmd status))))))
        (insert "\n"))

      ;; Test node execution
      (insert "## Node.js Execution Test\n")
      (let* ((node-path (executable-find "node"))
             (version-output
              (when node-path
                (with-temp-buffer
                  (call-process node-path nil t nil "--version")
                  (buffer-string)))))
        (insert (format "Node path: %s\n" (or node-path "NOT FOUND")))
        (insert (format "Node version: %s" (or version-output "FAILED\n"))))
      (insert "\n")

      ;; Test shell command execution
      (insert "## Shell Command Test\n")
      (let ((shell-node-version
             (shell-command-to-string "node --version 2>&1")))
        (insert (format "Shell 'node --version': %s" shell-node-version)))
      (insert "\n")

      ;; Check copilot-language-server details
      (insert "## copilot-language-server Details\n")
      (let ((copilot-info (version-manager-find-copilot-server)))
        (if copilot-info
            (let ((copilot-path (car copilot-info))
                  (source (cdr copilot-info)))
              (insert (format "Path: %s\n" copilot-path))
              (insert (format "Source: %s\n"
                             (pcase source
                               ('path "PATH (version manager or global npm)")
                               ('emacs-cache "Emacs cache (copilot.el managed)")
                               (_ "unknown"))))
              (insert (format "Exists: %s\n" (if (file-exists-p copilot-path) "YES" "NO")))
              (insert (format "Executable: %s\n" (if (file-executable-p copilot-path) "YES" "NO")))
              (when (file-symlink-p copilot-path)
                (insert (format "Symlink target: %s\n" (file-truename copilot-path))))
              ;; Read first few lines if it's a text file
              (when (and (file-exists-p copilot-path)
                         (< (nth 7 (file-attributes copilot-path)) 10000))
                (insert "\nFirst 5 lines:\n")
                (with-temp-buffer
                  (insert-file-contents copilot-path nil 0 500)
                  (insert (buffer-substring-no-properties (point-min) (point-max))))))
          (insert "NOT FOUND (checked PATH and Emacs cache)\n")))
      (insert "\n")

      ;; Summary
      (insert "## Summary\n")
      (let* ((node-path (executable-find "node"))
             (copilot-info (version-manager-find-copilot-server))
             (copilot-path (when copilot-info (car copilot-info)))
             (has-path (and (getenv "PATH")
                           (if manager
                               (string-match-p (vm-path-pattern manager) (getenv "PATH"))
                             t)))
             (all-good (and manager node-path copilot-path has-path)))
        (if all-good
            (insert (format "✓ All checks passed! %s environment is properly loaded.\n"
                           manager-name))
          (insert "✗ Some checks failed. Issues:\n")
          (unless manager (insert "  - No version manager detected\n"))
          (when (and manager (not has-path))
            (insert (format "  - PATH does not contain %s\n" manager-name)))
          (unless node-path (insert "  - node executable not found\n"))
          (unless copilot-path (insert "  - copilot-language-server not found\n"))))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; Fix Commands

;;;###autoload
(defun version-manager-fix-copilot-server ()
  "Fix copilot-language-server installation issues.
Reinstalls copilot-language-server in the current Node.js version."
  (interactive)
  (let* ((manager (version-manager-detect))
         (manager-name (if manager (vm-name manager) "version manager"))
         (node-version (shell-command-to-string "node --version 2>&1")))
    (if (string-match "v[0-9]+" node-version)
        (when (yes-or-no-p (format "Reinstall copilot-language-server for Node.js %s? "
                                   (string-trim node-version)))
          (message "Installing copilot-language-server...")
          (let ((result (shell-command-to-string "npm install -g @github/copilot-language-server 2>&1")))
            (if (string-match-p "added\\|up to date" result)
                (progn
                  (message "✓ copilot-language-server installed successfully")
                  (when manager
                    (let ((reshim-cmd (vm-cmd manager "reshim" "nodejs")))
                      (shell-command reshim-cmd)
                      (message "✓ %s reshim completed. Please restart Emacs." manager-name))))
              (message "✗ Installation failed: %s" result))))
      (message "✗ Node.js not found. Please check %s installation." manager-name))))

;;;###autoload
(defun version-manager-reshim-all ()
  "Reshim all version manager plugins."
  (interactive)
  (let ((manager (version-manager-detect)))
    (if manager
        (let* ((manager-name (vm-name manager))
               (cmd (vm-cmd manager "reshim")))
          (message "Reshimming all %s plugins..." manager-name)
          (let ((result (shell-command-to-string (concat cmd " 2>&1"))))
            (if (or (string-empty-p result)
                    (string-match-p "success\\|done" result))
                (message "✓ All %s plugins reshimmed successfully" manager-name)
              (message "Reshim output: %s" result))))
      (message "✗ No version manager detected"))))

;;;###autoload
(defun version-manager-verify-tool-versions ()
  "Verify version config file exists and check installed versions."
  (interactive)
  (let* ((manager (version-manager-detect))
         (config-file (vm-find-config manager)))
    (if config-file
        (progn
          (message "✓ Config file found at %s" config-file)
          (with-temp-buffer
            (insert-file-contents config-file)
            (let ((content (buffer-string)))
              (message "Contents:\n%s" content)
              ;; Handle .tool-versions format
              (when (string-suffix-p ".tool-versions" config-file)
                (dolist (line (split-string content "\n" t))
                  (when (string-match "^\\([a-z-]+\\)\\s-+\\(.+\\)$" line)
                    (let* ((plugin (match-string 1 line))
                           (version (match-string 2 line))
                           (list-cmd (when manager (vm-cmd manager "list" plugin)))
                           (installed (when list-cmd
                                       (shell-command-to-string
                                        (format "%s 2>&1 | grep -F '%s'" list-cmd version)))))
                      (if (and installed (string-match-p version installed))
                          (message "  ✓ %s %s is installed" plugin version)
                        (when manager
                          (let ((install-cmd (vm-cmd manager "install" plugin version)))
                            (message "  ✗ %s %s is NOT installed (run: %s)"
                                     plugin version install-cmd)))))))))))
      (message "✗ No version config file found. Searched: %s"
               (when manager
                 (mapconcat 'identity (vm-config-files manager) ", "))))))

;;;###autoload
(defun version-manager-quick-fix ()
  "Quick fix for common version manager issues.
Checks environment and suggests fixes."
  (interactive)
  (let* ((manager (version-manager-detect))
         (manager-name (if manager (vm-name manager) "version manager"))
         (issues '())
         (fixes '()))

    ;; Check if manager is detected
    (unless manager
      (push "No version manager detected" issues)
      (push "Install asdf (https://asdf-vm.com) or mise (https://mise.jdx.dev)" fixes)
      (push "Verify exec-path-from-shell-initialize runs on startup" fixes))

    ;; Check environment vars
    (when manager
      (dolist (var (vm-required-env-vars manager))
        (unless (getenv var)
          (push (format "%s not set" var) issues)
          (push "Verify exec-path-from-shell-initialize runs on startup" fixes))))

    ;; Check PATH
    (when manager
      (unless (and (getenv "PATH")
                  (string-match-p (vm-path-pattern manager) (getenv "PATH")))
        (push (format "PATH does not contain %s" manager-name) issues)
        (push "Restart Emacs to reload environment" fixes)))

    ;; Check node
    (unless (executable-find "node")
      (push "node executable not found" issues)
      (when manager
        (push (format "Install Node.js: %s"
                     (vm-cmd manager "install" "nodejs" "<version>"))
              fixes)))

    ;; Check copilot-language-server
    (unless (version-manager-find-copilot-server)
      (push "copilot-language-server not found in PATH or Emacs cache" issues)
      (push "Run: M-x version-manager-fix-copilot-server or M-x copilot-reinstall-server" fixes))

    ;; Report
    (if issues
        (let ((msg (format "Found %d issue(s):\n%s\n\nSuggested fixes:\n%s"
                           (length issues)
                           (mapconcat (lambda (i) (format "- %s" i)) issues "\n")
                           (mapconcat (lambda (f) (format "- %s" f)) fixes "\n"))))
          (message "%s" msg)
          (with-current-buffer (get-buffer-create "*version-manager-quick-fix*")
            (erase-buffer)
            (insert msg)
            (goto-char (point-min))
            (display-buffer (current-buffer))))
      (message "✓ No issues detected! %s environment looks good." manager-name))))

(provide 'check-version-manager)
;;; check-version-manager.el ends here
