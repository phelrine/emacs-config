;;; gh-copy-url.el --- Copy GitHub issue/PR URLs -*- lexical-binding: t -*-

;;; Commentary:
;; Utility functions to search and copy GitHub issue/PR URLs.
;; Uses gh.el for GitHub CLI interaction.
;; Supports embark actions for flexible operations.

;;; Code:

(require 'gh)

(defgroup gh-copy-url nil
  "Copy GitHub issue/PR URLs."
  :group 'gh)

(defcustom gh-copy-url-limit 50
  "Maximum number of results to fetch."
  :type 'integer
  :group 'gh-copy-url)

;;; Embark integration

(defvar gh-copy-url-embark-actions
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") #'gh-copy-url-action-copy-url)
    (define-key map (kbd "b") #'gh-copy-url-action-browse)
    (define-key map (kbd "m") #'gh-copy-url-action-copy-markdown)
    (define-key map (kbd "o") #'gh-copy-url-action-copy-org)
    map)
  "Embark actions for gh-copy-url candidates.")

(defun gh-copy-url-action-copy-url (candidate)
  "Copy URL of CANDIDATE to kill-ring."
  (when-let ((url (get-text-property 0 'gh-url candidate)))
    (kill-new url)
    (message "Copied: %s" url)))

(defun gh-copy-url-action-browse (candidate)
  "Open CANDIDATE in browser."
  (when-let ((url (get-text-property 0 'gh-url candidate)))
    (browse-url url)))

(defun gh-copy-url-action-copy-markdown (candidate)
  "Copy CANDIDATE as Markdown link to kill-ring."
  (when-let ((url (get-text-property 0 'gh-url candidate))
             (title (get-text-property 0 'gh-title candidate)))
    (let ((markdown (format "[%s](%s)" title url)))
      (kill-new markdown)
      (message "Copied: %s" markdown))))

(defun gh-copy-url-action-copy-org (candidate)
  "Copy CANDIDATE as Org link to kill-ring."
  (when-let ((url (get-text-property 0 'gh-url candidate))
             (title (get-text-property 0 'gh-title candidate)))
    (let ((org-link (format "[[%s][%s]]" url title)))
      (kill-new org-link)
      (message "Copied: %s" org-link))))

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(gh-item . gh-copy-url-embark-actions)))

;;; Internal functions

(defun gh-copy-url--make-candidate (item)
  "Create a candidate string from ITEM with text properties."
  (let* ((display (gh-format-item item))
         (url (gh-item-url item))
         (title (gh-item-title item)))
    (propertize display
                'gh-url url
                'gh-title title)))

(defun gh-copy-url--completion-table (candidates)
  "Create completion table from CANDIDATES with gh-item category."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . gh-item))
      (complete-with-action action candidates string pred))))

(defun gh-copy-url--select (items prompt &optional no-results-message)
  "Show PROMPT for selection from ITEMS.
Returns selected candidate or nil.
NO-RESULTS-MESSAGE is shown when ITEMS is empty."
  (if (not items)
      (progn
        (message "%s" (or no-results-message "No results found"))
        nil)
    (let* ((candidates (mapcar #'gh-copy-url--make-candidate items))
           (table (gh-copy-url--completion-table candidates)))
      (completing-read prompt table nil t))))

(defun gh-copy-url--select-and-act (items prompt &optional no-results-message)
  "Show PROMPT for selection from ITEMS and copy URL.
NO-RESULTS-MESSAGE is shown when ITEMS is empty.
Default action is copy URL. Use embark (C-.) for other actions."
  (when-let ((selected (gh-copy-url--select items prompt no-results-message)))
    (gh-copy-url-action-copy-url selected)))

;;; Issue commands

;;;###autoload
(defun gh-copy-url-issue-assigned ()
  "Search issues assigned to me and copy URL to kill-ring."
  (interactive)
  (gh-copy-url--select-and-act
   (gh-search-issues "--assignee" "@me" "--state" "open"
                     "--limit" (number-to-string gh-copy-url-limit))
   "Assigned Issue: "
   "No assigned issues found"))

;;;###autoload
(defun gh-copy-url-issue-authored ()
  "Search issues authored by me and copy URL to kill-ring."
  (interactive)
  (gh-copy-url--select-and-act
   (gh-search-issues "--author" "@me" "--state" "open"
                     "--limit" (number-to-string gh-copy-url-limit))
   "My Issue: "
   "No authored issues found"))

;;;###autoload
(defun gh-copy-url-issue-mentioned ()
  "Search issues where I am mentioned and copy URL to kill-ring."
  (interactive)
  (gh-copy-url--select-and-act
   (gh-search-issues "--mentions" "@me" "--state" "open"
                     "--limit" (number-to-string gh-copy-url-limit))
   "Mentioned Issue: "
   "No mentioned issues found"))

;;; PR commands

;;;###autoload
(defun gh-copy-url-pr-authored ()
  "Search PRs authored by me and copy URL to kill-ring."
  (interactive)
  (gh-copy-url--select-and-act
   (gh-search-prs "--author" "@me" "--state" "open"
                  "--limit" (number-to-string gh-copy-url-limit))
   "My PR: "
   "No authored PRs found"))

;;;###autoload
(defun gh-copy-url-pr-review-requested ()
  "Search PRs where review is requested from me and copy URL to kill-ring."
  (interactive)
  (gh-copy-url--select-and-act
   (gh-search-prs "--review-requested" "@me" "--state" "open"
                  "--limit" (number-to-string gh-copy-url-limit))
   "Review PR: "
   "No review-requested PRs found"))

;;;###autoload
(defun gh-copy-url-pr-assigned ()
  "Search PRs assigned to me and copy URL to kill-ring."
  (interactive)
  (gh-copy-url--select-and-act
   (gh-search-prs "--assignee" "@me" "--state" "open"
                  "--limit" (number-to-string gh-copy-url-limit))
   "Assigned PR: "
   "No assigned PRs found"))

(provide 'gh-copy-url)
;;; gh-copy-url.el ends here
