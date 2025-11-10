;;; check-asdf-path.el --- Check asdf path resolution in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Script to verify asdf environment is correctly loaded in Emacs
;; Provides diagnostic tools and automatic fixes for common asdf issues

;;; Code:

(require 'shell)

;;;###autoload
(defun check-asdf-environment ()
  "Check if asdf environment is correctly loaded in Emacs."
  (interactive)
  (let ((results '()))
    (with-current-buffer (get-buffer-create "*asdf-check*")
      (erase-buffer)
      (insert "=== ASDF Environment Check ===\n\n")

      ;; Check environment variables
      (insert "## Environment Variables\n")
      (dolist (var '("ASDF_DIR" "ASDF_DATA_DIR" "PATH"))
        (let ((value (getenv var)))
          (insert (format "%-15s: %s\n" var (or value "NOT SET")))
          (push (cons var value) results)))
      (insert "\n")

      ;; Check PATH contains asdf
      (insert "## PATH contains asdf?\n")
      (let* ((path (getenv "PATH"))
             (has-asdf (and path (string-match-p "asdf" path))))
        (insert (format "Result: %s\n" (if has-asdf "YES ✓" "NO ✗")))
        (when has-asdf
          (insert "asdf paths in PATH:\n")
          (dolist (p (split-string path ":" t))
            (when (string-match-p "asdf" p)
              (insert (format "  - %s\n" p))))))
      (insert "\n")

      ;; Check executable-find
      (insert "## Executable Paths\n")
      (dolist (cmd '("node" "npm" "copilot-language-server" "asdf"))
        (let ((path (executable-find cmd)))
          (insert (format "%-25s: %s\n" cmd (or path "NOT FOUND")))
          (push (cons cmd path) results)))
      (insert "\n")

      ;; Check if executables are asdf shims
      (insert "## Are executables asdf shims?\n")
      (dolist (cmd '("node" "npm" "copilot-language-server"))
        (let ((path (executable-find cmd)))
          (when path
            (let ((is-shim (string-match-p "/\\.asdf/shims/" path)))
              (insert (format "%-25s: %s %s\n"
                              cmd
                              (if is-shim "YES (shim)" "NO (direct)")
                              (if is-shim "✓" "⚠")))))))
      (insert "\n")

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
      (let ((copilot-path (executable-find "copilot-language-server")))
        (if copilot-path
            (progn
              (insert (format "Path: %s\n" copilot-path))
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
          (insert "NOT FOUND\n")))
      (insert "\n")

      ;; Summary
      (insert "## Summary\n")
      (let* ((asdf-dir (getenv "ASDF_DIR"))
             (asdf-data-dir (getenv "ASDF_DATA_DIR"))
             (node-path (executable-find "node"))
             (copilot-path (executable-find "copilot-language-server"))
             (all-good (and asdf-dir asdf-data-dir node-path copilot-path
                            (string-match-p "asdf" (or (getenv "PATH") "")))))
        (if all-good
            (insert "✓ All checks passed! asdf environment is properly loaded.\n")
          (insert "✗ Some checks failed. Issues:\n")
          (unless asdf-dir (insert "  - ASDF_DIR not set\n"))
          (unless asdf-data-dir (insert "  - ASDF_DATA_DIR not set\n"))
          (unless (and (getenv "PATH") (string-match-p "asdf" (getenv "PATH")))
            (insert "  - PATH does not contain asdf\n"))
          (unless node-path (insert "  - node executable not found\n"))
          (unless copilot-path (insert "  - copilot-language-server not found\n"))))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun asdf-fix-copilot-server ()
  "Fix copilot-language-server installation issues.
Reinstalls copilot-language-server in the current Node.js version."
  (interactive)
  (let ((node-version (shell-command-to-string "node --version 2>&1")))
    (if (string-match "v[0-9]+" node-version)
        (when (yes-or-no-p (format "Reinstall copilot-language-server for Node.js %s? "
                                   (string-trim node-version)))
          (message "Installing copilot-language-server...")
          (let ((result (shell-command-to-string "npm install -g @github/copilot-language-server 2>&1")))
            (if (string-match-p "added\\|up to date" result)
                (progn
                  (message "✓ copilot-language-server installed successfully")
                  (shell-command "asdf reshim nodejs")
                  (message "✓ asdf reshim completed. Please restart Emacs."))
              (message "✗ Installation failed: %s" result))))
      (message "✗ Node.js not found. Please check asdf installation."))))

;;;###autoload
(defun asdf-reshim-all ()
  "Reshim all asdf plugins."
  (interactive)
  (message "Reshimming all asdf plugins...")
  (let ((result (shell-command-to-string "asdf reshim 2>&1")))
    (if (string-empty-p result)
        (message "✓ All asdf plugins reshimmed successfully")
      (message "Reshim output: %s" result))))

;;;###autoload
(defun asdf-verify-tool-versions ()
  "Verify .tool-versions file exists and check installed versions."
  (interactive)
  (let ((tool-versions-file (expand-file-name ".tool-versions" user-emacs-directory)))
    (if (file-exists-p tool-versions-file)
        (progn
          (message "✓ .tool-versions found at %s" tool-versions-file)
          (with-temp-buffer
            (insert-file-contents tool-versions-file)
            (let ((content (buffer-string)))
              (message "Contents:\n%s" content)
              ;; Check each line
              (dolist (line (split-string content "\n" t))
                (when (string-match "^\\([a-z-]+\\)\\s-+\\(.+\\)$" line)
                  (let* ((plugin (match-string 1 line))
                         (version (match-string 2 line))
                         (installed (shell-command-to-string
                                     (format "asdf list %s 2>&1 | grep -F '%s'" plugin version))))
                    (if (string-match-p version installed)
                        (message "  ✓ %s %s is installed" plugin version)
                      (message "  ✗ %s %s is NOT installed (run: asdf install %s %s)"
                               plugin version plugin version))))))))
      (message "✗ .tool-versions not found at %s" tool-versions-file))))

;;;###autoload
(defun asdf-quick-fix ()
  "Quick fix for common asdf issues.
Checks environment and suggests fixes."
  (interactive)
  (let ((issues '())
        (fixes '()))
    ;; Check ASDF_DIR
    (unless (getenv "ASDF_DIR")
      (push "ASDF_DIR not set" issues)
      (push "Verify exec-path-from-shell-initialize runs on startup" fixes))

    ;; Check PATH
    (unless (and (getenv "PATH") (string-match-p "asdf" (getenv "PATH")))
      (push "PATH does not contain asdf" issues)
      (push "Restart Emacs to reload environment" fixes))

    ;; Check node
    (unless (executable-find "node")
      (push "node executable not found" issues)
      (push "Install Node.js: asdf install nodejs <version>" fixes))

    ;; Check copilot-language-server
    (unless (executable-find "copilot-language-server")
      (push "copilot-language-server not found" issues)
      (push "Run: M-x asdf-fix-copilot-server" fixes))

    ;; Report
    (if issues
        (let ((msg (format "Found %d issue(s):\n%s\n\nSuggested fixes:\n%s"
                           (length issues)
                           (mapconcat (lambda (i) (format "- %s" i)) issues "\n")
                           (mapconcat (lambda (f) (format "- %s" f)) fixes "\n"))))
          (message "%s" msg)
          (with-current-buffer (get-buffer-create "*asdf-quick-fix*")
            (erase-buffer)
            (insert msg)
            (goto-char (point-min))
            (display-buffer (current-buffer))))
      (message "✓ No asdf issues detected!"))))

(provide 'check-asdf-path)
;;; check-asdf-path.el ends here
