(when (require 'flymake nil t)
  (defun flymake-tex-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-dir   (file-name-directory buffer-file-name))
           (local-file  (file-relative-name
                         temp-file
                         local-dir)))
      (list "platex" (list "-file-line-error" "-interaction=nonstopmode" local-file))))

  (defun flymake-tex-cleanup-custom ()
    (let* ((base-file-name (file-name-sans-extension (file-name-nondirectory flymake-temp-source-file-name)))
           (regexp-base-file-name (concat "^" base-file-name "\\.")))
      (mapc '(lambda (filename)
               (when (string-match regexp-base-file-name filename)
                 (flymake-safe-delete-file filename)))
            (split-string (shell-command-to-string "ls"))))
    (setq flymake-last-change-time nil))

  (push '("\\.tex$" flymake-tex-init flymake-tex-cleanup-custom) flymake-allowed-file-name-masks))

(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun th-evince-sync (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
         (buf (find-buffer-visiting fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(defvar *dbus-evince-signal* nil)

(defun enable-evince-sync ()
  (require 'dbus)
  (when (and
         (eq window-system 'x)
         (fboundp 'dbus-register-signal))
    (unless *dbus-evince-signal*
      (setf *dbus-evince-signal*
            (dbus-register-signal
             :session nil "/org/gnome/evince/Window/0"
             "org.gnome.evince.Window" "SyncSource"
             'th-evince-sync)))))

(add-hook 'latex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (auto-fill-mode -1)
             (enable-evince-sync)))

(setq auto-mode-alist (cons '("\\.tex\\'" . latex-mode) auto-mode-alist))
