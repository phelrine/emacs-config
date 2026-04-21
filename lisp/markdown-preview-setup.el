;;; markdown-preview-setup.el --- Configure markdown-preview-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; markdown-preview-mode のカスタム設定:
;; - github-markdown-css で GitHub 風 light テーマを適用
;; - mermaid.js で ```mermaid ブロックをクライアント側描画
;; - JS / CSS 資産は lisp/markdown-preview/ 配下に外出し

;;; Code:

(require 'markdown-preview-mode)

(defconst markdown-preview-setup-assets-dir
  (expand-file-name
   "markdown-preview"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing preview.css, mermaid-init.js, mermaid-update.js.")

(defconst markdown-preview-setup-github-css-url
  "https://cdn.jsdelivr.net/npm/github-markdown-css@5/github-markdown-light.min.css"
  "CDN URL for GitHub-flavored markdown stylesheet (light variant).")

(defconst markdown-preview-setup-mermaid-url
  "https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"
  "CDN URL for the Mermaid.js runtime.")

(defun markdown-preview-setup--asset (name)
  "Read asset NAME from `markdown-preview-setup-assets-dir' as a string."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name markdown-preview-setup-assets-dir))
    (buffer-string)))

(setq markdown-preview-stylesheets
      (list markdown-preview-setup-github-css-url
            (format "<style>%s</style>"
                    (markdown-preview-setup--asset "preview.css"))))

(add-to-list 'markdown-preview-javascript
             markdown-preview-setup-mermaid-url)

(setq markdown-preview-script-oninit
      (markdown-preview-setup--asset "mermaid-init.js")
      markdown-preview-script-onupdate
      (markdown-preview-setup--asset "mermaid-update.js"))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-command-map (kbd "P") #'markdown-preview-mode))

(provide 'markdown-preview-setup)

;;; markdown-preview-setup.el ends here
