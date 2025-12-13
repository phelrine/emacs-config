;;; gh.el --- GitHub CLI (gh) wrapper for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; Wrapper functions for GitHub CLI (gh).
;; Requires gh CLI to be installed and authenticated.

;;; Code:

(defgroup gh nil
  "GitHub CLI wrapper."
  :group 'tools)

(defcustom gh-executable "gh"
  "Path to gh CLI executable."
  :type 'string
  :group 'gh)

;;; Low-level functions

(defun gh-command (&rest args)
  "Execute gh command with ARGS and return output as string."
  (shell-command-to-string
   (mapconcat #'shell-quote-argument (cons gh-executable args) " ")))

(defun gh-command-json (&rest args)
  "Execute gh command with ARGS and return parsed JSON.
Returns nil if parsing fails."
  (let ((output (apply #'gh-command args)))
    (ignore-errors
      (json-parse-string output :array-type 'list :object-type 'alist))))

;;; Search functions

(defun gh-search-issues (&rest query-args)
  "Search issues with QUERY-ARGS.
QUERY-ARGS are passed directly to `gh search issues`.
Returns a list of issue alists."
  (apply #'gh-command-json
         "search" "issues"
         "--json" "repository,number,title,url"
         query-args))

(defun gh-search-prs (&rest query-args)
  "Search pull requests with QUERY-ARGS.
QUERY-ARGS are passed directly to `gh search prs`.
Returns a list of PR alists."
  (apply #'gh-command-json
         "search" "prs"
         "--json" "repository,number,title,url"
         query-args))

;;; Utility functions

(defun gh-item-repo-name (item)
  "Get repository name from ITEM."
  (alist-get 'nameWithOwner (alist-get 'repository item)))

(defun gh-item-number (item)
  "Get issue/PR number from ITEM."
  (alist-get 'number item))

(defun gh-item-title (item)
  "Get title from ITEM."
  (alist-get 'title item))

(defun gh-item-url (item)
  "Get URL from ITEM."
  (alist-get 'url item))

(defun gh-format-item (item)
  "Format ITEM as \"repo#number: title\"."
  (format "%s#%d: %s"
          (gh-item-repo-name item)
          (gh-item-number item)
          (gh-item-title item)))

(provide 'gh)
;;; gh.el ends here
