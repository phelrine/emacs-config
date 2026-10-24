;;; mise-env-test.el --- Tests for mise-env -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs --batch -L lisp -l ert -l mise-env \
;;     -l lisp/test/mise-env-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
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
    (when-let* ((found (mise-env--project-root)))
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

(defmacro mise-env-test--counting-fetches (&rest body)
  "Run BODY with an empty cache and `mise-env--fetch-env' stubbed.
Bound for BODY: `fetches', how many times mise would have been run."
  (declare (indent 0))
  `(let ((mise-env--project-cache (make-hash-table :test 'equal))
         (fetches 0))
     (cl-letf (((symbol-function 'mise-env--fetch-env)
                (lambda (_dir) (setq fetches (1+ fetches)) '((FOO . "bar")))))
       ,@body)))

(ert-deftest mise-env-test-get-env-caches ()
  "An unchanged tree is looked up once."
  (mise-env-test--with-tree '(".tool-versions")
    (mise-env-test--counting-fetches
      (mise-env--get-env root)
      (mise-env--get-env root)
      (should (= fetches 1)))))

(ert-deftest mise-env-test-get-env-sees-new-ancestor-config ()
  "A config file added in an ancestor after the first lookup is picked up.
mise merges every config up the tree, so a `mise.local.toml' dropped
into a parent directory changes the project's environment even though
the project root itself stays the same.  The cache used to keep
serving the old result until Emacs restarted."
  (mise-env-test--with-tree '("sub/.tool-versions")
    (let ((sub (expand-file-name "sub/" root)))
      (mise-env-test--counting-fetches
        (mise-env--get-env sub)
        (write-region "" nil (expand-file-name "mise.local.toml" root) nil 'silent)
        (mise-env--get-env sub)
        (should (= fetches 2))))))

(ert-deftest mise-env-test-get-env-sees-edited-config ()
  "Editing a config file the project already had invalidates the cache."
  (mise-env-test--with-tree '("mise.toml")
    (let ((config (expand-file-name "mise.toml" root)))
      (mise-env-test--counting-fetches
        (mise-env--get-env root)
        (set-file-times config (time-add (current-time) 10))
        (mise-env--get-env root)
        (should (= fetches 2))))))

(ert-deftest mise-env-test-reapply-drops-stale-vars ()
  "Re-applying replaces a buffer's earlier mise env instead of layering on it.
`lsp-workspace-restart' re-runs `lsp' in buffers that were set up
before a version bump; a variable only the old env had must not leak
into the server started from them."
  (with-temp-buffer
    (mise-env--apply-env '((PATH . "/old/bin") (OLD_ONLY . "1")))
    (mise-env--apply-env '((PATH . "/new/bin")))
    (should-not (getenv "OLD_ONLY"))
    (should (equal exec-path '("/new/bin/")))))

(ert-deftest mise-env-test-update-accepts-advice-args ()
  "`mise-env-update' can be a :before advice on `lsp', which takes ARG."
  (with-temp-buffer
    (let ((default-directory "/"))
      (cl-letf (((symbol-function 'mise-env--project-root) #'ignore))
        (mise-env-update 'some-lsp-arg)))))

(provide 'mise-env-test)
;;; mise-env-test.el ends here
