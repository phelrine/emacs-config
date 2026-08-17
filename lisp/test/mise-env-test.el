;;; mise-env-test.el --- Tests for mise-env -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs --batch -L lisp -l ert -l mise-env \
;;     -l lisp/test/mise-env-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'mise-env)

(defmacro mise-env-test--with-tree (files &rest body)
  "Create a temporary project tree containing FILES, then run BODY.
FILES is a list of paths relative to the tree root.  ROOT is bound to
the tree's directory name."
  (declare (indent 1))
  `(let ((root (file-name-as-directory (make-temp-file "mise-env-test" t))))
     (unwind-protect
         (progn
           (dolist (f ,files)
             (let ((path (expand-file-name f root)))
               (make-directory (file-name-directory path) t)
               (write-region "" nil path nil 'silent)))
           ,@body)
       (delete-directory root t))))

(defun mise-env-test--root-from (dir)
  "Return `mise-env--project-root' as seen from DIR."
  (let ((default-directory dir))
    (when-let ((found (mise-env--project-root)))
      (file-truename found))))

(ert-deftest mise-env-test-detects-dotted-config ()
  "`.mise.toml' marks a project root."
  (mise-env-test--with-tree '(".mise.toml")
    (should (equal (mise-env-test--root-from root) (file-truename root)))))

(ert-deftest mise-env-test-detects-undotted-config ()
  "`mise.toml' — mise's default file name — marks a project root."
  (mise-env-test--with-tree '("mise.toml")
    (should (equal (mise-env-test--root-from root) (file-truename root)))))

(ert-deftest mise-env-test-detects-local-config ()
  "`mise.local.toml' marks a project root.
This is the file name to use for machine-local settings in a shared
repository, since it is conventionally gitignored."
  (mise-env-test--with-tree '("mise.local.toml")
    (should (equal (mise-env-test--root-from root) (file-truename root)))))

(ert-deftest mise-env-test-detects-dotted-local-config ()
  "`.mise.local.toml' marks a project root."
  (mise-env-test--with-tree '(".mise.local.toml")
    (should (equal (mise-env-test--root-from root) (file-truename root)))))

(ert-deftest mise-env-test-detects-config-subdir ()
  "`.config/mise.toml' marks a project root."
  (mise-env-test--with-tree '(".config/mise.toml")
    (should (equal (mise-env-test--root-from root) (file-truename root)))))

(ert-deftest mise-env-test-detects-tool-versions ()
  "`.tool-versions' marks a project root."
  (mise-env-test--with-tree '(".tool-versions")
    (should (equal (mise-env-test--root-from root) (file-truename root)))))

(ert-deftest mise-env-test-finds-root-from-subdirectory ()
  "The search walks up from a nested directory."
  (mise-env-test--with-tree '("mise.toml" "src/lib/.keep")
    (should (equal (mise-env-test--root-from (expand-file-name "src/lib/" root))
                   (file-truename root)))))

(ert-deftest mise-env-test-nested-config-wins ()
  "The nearest config wins when an ancestor also has one."
  (mise-env-test--with-tree '(".tool-versions" "sub/mise.toml")
    (should (equal (mise-env-test--root-from (expand-file-name "sub/" root))
                   (file-truename (expand-file-name "sub/" root))))))

(ert-deftest mise-env-test-no-config-returns-nil ()
  "A tree with no mise config has no root."
  (mise-env-test--with-tree '("README.md")
    (should-not (mise-env-test--root-from root))))

(defmacro mise-env-test--with-fake-mise (stdout stderr &rest body)
  "Run BODY with `mise-env-executable' bound to a script emitting STDOUT/STDERR."
  (declare (indent 2))
  `(let ((script (make-temp-file "fake-mise" nil ".sh")))
     (unwind-protect
         (progn
           (write-region (format "#!/bin/sh\nprintf '%%s\\n' %s >&2\nprintf '%%s\\n' %s\n"
                                 (shell-quote-argument ,stderr)
                                 (shell-quote-argument ,stdout))
                         nil script nil 'silent)
           (set-file-modes script #o755)
           (let ((mise-env-executable script))
             ,@body))
       (delete-file script))))

(ert-deftest mise-env-test-fetch-env-parses-json ()
  "The JSON mise prints becomes an alist."
  (mise-env-test--with-fake-mise "{\"CLAUDE_CONFIG_DIR\":\"/stub/config\"}" ""
    (should (equal (cdr (assq 'CLAUDE_CONFIG_DIR
                              (mise-env--fetch-env default-directory)))
                   "/stub/config"))))

(ert-deftest mise-env-test-fetch-env-ignores-stderr ()
  "Warnings on stderr do not corrupt the JSON parse.
mise prints things like \"mise WARN missing: pnpm@11.13.0\" to stderr;
mixing that into the parse buffer used to make the whole lookup fail."
  (mise-env-test--with-fake-mise "{\"CLAUDE_CONFIG_DIR\":\"/stub/config\"}"
      "mise WARN  missing: pnpm@11.13.0"
    (should (equal (cdr (assq 'CLAUDE_CONFIG_DIR
                              (mise-env--fetch-env default-directory)))
                   "/stub/config"))))

(provide 'mise-env-test)
;;; mise-env-test.el ends here
