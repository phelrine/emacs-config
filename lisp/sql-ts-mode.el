(require 'treesit)
(require 'treesit-auto)
(require 'sql)
(require 'lsp-mode)
(require 'lsp-sqls)

;;; Code:

(define-derived-mode sql-ts-mode sql-mode "SQL"
  "Major mode for editing PHP files, powered by tree-sitter."
  :group 'sql
  :syntax-table sql-mode-syntax-table
  (unless (treesit-ready-p 'sql)
    (error "Tree-sitter for SQL isn't available"))
  (treesit-parser-create 'sql)
  (treesit-major-mode-setup))

(add-to-list 'treesit-auto-recipe-list
             (make-treesit-auto-recipe
              :lang 'sql
              :ts-mode 'sql-ts-mode
              :remap 'sql-mode
              :url "https://github.com/m-novikov/tree-sitter-sql"
              :revision "master"
              :source-dir "src"
              :ext "\\.sql\\'"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-sqls--make-launch-cmd)
                  :major-modes '(sql-mode sql-ts-mode)
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
                                      (lsp-sqls-setup-workspace-configuration)))))

(provide 'sql-ts-mode)

;;; sql-ts-mode.el ends here
