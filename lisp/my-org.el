;;; my-org.el --- Personal org-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; org-mode の個人設定

;;; Code:

(defvar my/org-weekly-dir (expand-file-name "weekly" org-directory)
  "週次レポートのディレクトリ")

(defun my/org-weekly-file ()
  "今週の週次ファイルパスを取得"
  (expand-file-name
   (format-time-string "%G-W%V.org")
   my/org-weekly-dir))

(defun my/org-gptel-generate (prompt-template start-marker end-marker)
  "ファイル内容を元に gptel で生成。START-MARKER から END-MARKER までの内容を使用。"
  (let* ((content (save-excursion
                    (goto-char (point-min))
                    (let ((start (if (re-search-forward start-marker nil t)
                                     (match-beginning 0) (point-min))))
                      (goto-char start)
                      (buffer-substring-no-properties
                       start
                       (or (re-search-forward end-marker nil t) (point-max))))))
         (prompt (format prompt-template content)))
    (gptel-request prompt
      :callback (lambda (response info)
                  (if response
                      (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward "^#\\+RESULTS:" nil t)
                          (forward-line 1)
                          (delete-region (point) (point-max))
                          (insert response)))
                    (message "gptel エラー: %s" (plist-get info :error)))))
    "生成中..."))

;; org-capture テンプレート
(customize-set-variable 'org-capture-templates
  `(("t" "TODO" entry
     (file+headline my/org-weekly-file "今週のTODO")
     "** TODO %?\n" :prepend t)
    ("w" "Weekly Report" plain
     (file my/org-weekly-file)
     (file ,(expand-file-name "templates/weekly.org" org-directory)))
    ("m" "Weekly Memo" entry
     (file+headline my/org-weekly-file "業務メモ")
     "** %U %?\n" :prepend t)))

;; Enable ASSIGNEE property inheritance
(with-eval-after-load 'org
  (add-to-list 'org-use-property-inheritance "ASSIGNEE"))

;; Display assignee (ASSIGNEE property) in org-agenda
(defun my/org-agenda-prefix-format ()
  "Custom prefix format with assignee from ASSIGNEE property."
  (let ((assignee (or (org-entry-get nil "ASSIGNEE" t) "")))
    (if (string-empty-p assignee)
        "       "
      (format "%-6s " (truncate-string-to-width assignee 6 nil nil "…")))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%(my/org-agenda-prefix-format)%?-12t% s")
          (todo . " %i %-12:c%(my/org-agenda-prefix-format)")
          (tags . " %i %-12:c%(my/org-agenda-prefix-format)")
          (search . " %i %-12:c%(my/org-agenda-prefix-format)")))

  ;; Filter agenda by ASSIGNEE property
  (defun my/org-agenda-collect-assignees ()
    "Collect all ASSIGNEE values from agenda files."
    (let (assignees)
      (dolist (file (org-agenda-files))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^:ASSIGNEE:\\s-*\\(.+\\)$" nil t)
             (push (match-string-no-properties 1) assignees)))))
      (delete-dups assignees)))

  (defvar my/org-agenda-assignee-filter nil
    "Current ASSIGNEE filter value.")

  (defun my/org-agenda-skip-by-assignee ()
    "Skip entries not matching `my/org-agenda-assignee-filter'."
    (when my/org-agenda-assignee-filter
      (let ((assignee (org-entry-get nil "ASSIGNEE" t)))
        (unless (and assignee (string= assignee my/org-agenda-assignee-filter))
          (org-end-of-subtree t)))))

  (defun my/org-agenda-filter-by-assignee ()
    "Filter agenda by ASSIGNEE property."
    (interactive)
    (let ((assignee (completing-read
                     "Assignee: "
                     (my/org-agenda-collect-assignees))))
      (setq my/org-agenda-assignee-filter assignee)
      (org-agenda-redo)))

  (defun my/org-agenda-clear-assignee-filter ()
    "Clear ASSIGNEE filter and all other filters."
    (interactive)
    (setq my/org-agenda-assignee-filter nil)
    (org-agenda-redo t))

  (setq org-agenda-skip-function-global #'my/org-agenda-skip-by-assignee))

;; org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") #'my/org-agenda-filter-by-assignee)
  (define-key org-agenda-mode-map (kbd "C") #'my/org-agenda-clear-assignee-filter))

;; org-agenda-files の動的更新
(defun my/org-agenda-files-update (&rest _)
  "Update `org-agenda-files' to current weekly file."
  (setq org-agenda-files (list (my/org-weekly-file))))

(advice-add 'org-agenda :before #'my/org-agenda-files-update)
(advice-add 'org-agenda-list :before #'my/org-agenda-files-update)
(advice-add 'org-todo-list :before #'my/org-agenda-files-update)

(provide 'my-org)
;;; my-org.el ends here
