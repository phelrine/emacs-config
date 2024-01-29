;;; sql-ts-mode.el --- SQL mode powered by tree-sitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'treesit)
(require 'sql)

(defcustom sql-ts-mode-indent-offset 4
  "Default indentation offset for `sql-ts-mode`."
  :type 'integer
  :group 'sql-ts-mode)

(defcustom sql-ts-mode-indent-rules
  '((sql
     ((query ((_ "(" (_) @node ")"))) parent-bol sql-ts-mode-indent-offset)
     ((node-is ")") parent-bol 0)
     ((node-is "select_expression") parent-bol sql-ts-mode-indent-offset)
     ((parent-is "select_expression") parent-bol 0)
     ((node-is "from") parent-bol 0)
     ((node-is "relation") parent-bol sql-ts-mode-indent-offset)
     ((node-is "join") parent-bol 0)
     ((node-is "where") parent-bol 0)
     ((node-is "expression") parent-bol sql-ts-mode-indent-offset)
     ((node-is "group_by") parent-bol 0)
     ((node-is "order_by") parent-bol 0)
     ((node-is "limit") parent-bol 0)
     ((parent-is "where") parent-bol sql-ts-mode-indent-offset)
     ((parent-is "group_by") parent-bol sql-ts-mode-indent-offset)
     ((parent-is "order_by") parent-bol sql-ts-mode-indent-offset)
     ((node-is "keyword_end") parent-bol 0)
     ((parent-is "case") parent-bol sql-ts-mode-indent-offset)))
  "Indentation rules for `sql-ts-mode`, powered by tree-sitter."
  :type '(repeat (list symbol))
  :group 'sql-ts-mode)

;;;###autoload
(define-derived-mode sql-ts-mode sql-mode "SQL[TS]"
  "Major mode for editing SQL files with tree-sitter support."
  :group 'sql
  :syntax-table sql-mode-syntax-table
  (unless (treesit-ready-p 'sql)
    (error "Tree-sitter for SQL isn't available"))
  (treesit-parser-create 'sql)
  (setq treesit-simple-indent-rules sql-ts-mode-indent-rules)
  (treesit-major-mode-setup))

;; Add tree-sitter source for SQL if it's not present
(unless (assoc 'sql treesit-language-source-alist)
  (add-to-list 'treesit-language-source-alist
               '(sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")))

;;; Integration with lsp-mode

(with-eval-after-load 'lsp-mode
  (require 'lsp-sqls)
  (with-no-warnings
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-sqls--make-launch-cmd)
                      :major-modes '(sql-ts-mode)
                      :priority -1
                      :language-id "sql"
                      :action-handlers (ht ("executeParagraph" #'lsp-sql-execute-paragraph)
                                           ("executeQuery" #'lsp-sql-execute-query)
                                           ("showDatabases" #'lsp-sql-show-databases)
                                           ("showSchemas" #'lsp-sql-show-schemas)
                                           ("showConnections" #'lsp-sql-show-connections)
                                           ("switchDatabase" #'lsp-sql-switch-database)
                                           ("switchConnections" #'lsp-sql-switch-connection))
                      :server-id 'sqls
                      :initialized-fn (lambda (workspace)
                                        (-> workspace
                                            (lsp--workspace-server-capabilities)
                                            (lsp:set-server-capabilities-execute-command-provider? t))
                                        (with-lsp-workspace workspace
                                          (lsp-sqls-setup-workspace-configuration)))))))

(provide 'sql-ts-mode)

;;; sql-ts-mode.el ends here
