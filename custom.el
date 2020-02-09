(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-expand-on-auto-complete t)
 '(ac-show-menu-immediately-on-auto-complete nil)
 '(auto-package-update-delete-old-versions t t)
 '(auto-package-update-hide-results t t)
 '(beacon-color "light green")
 '(beacon-mode t)
 '(bm-buffer-persistence t)
 '(bm-cycle-all-buffers t)
 '(bm-fringe-face (quote bm-face))
 '(bm-fringe-persistent-face (quote bm-face))
 '(bm-highlight-style (quote bm-highlight-line-and-fringe))
 '(bm-persistent-face (quote bm-face))
 '(cc-other-file-alist
   (quote
    (("\\.cpp$"
      (".hpp" ".h"))
     ("\\.h$"
      (".c" ".cpp" ".m" ".mm"))
     ("\\.hpp$"
      (".cpp" ".c"))
     ("\\.m$"
      (".h"))
     ("\\.mm$"
      (".h")))))
 '(company-backends
   (quote
    (company-capf company-dabbrev-code company-files company-elisp company-yasnippet)))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay nil)
 '(company-lsp-enable-recompletion nil)
 '(company-statistics-mode t)
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(ein:output-type-preference (quote ein:output-type-prefer-pretty-text-over-html))
 '(enable-recursive-minibuffers t)
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GOPATH")))
 '(global-auto-revert-mode t)
 '(global-company-mode 1)
 '(global-eldoc-mode 1)
 '(global-git-gutter+-mode t)
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(helm-boring-file-regexp-list (quote ("~$" "\\.meta$")))
 '(helm-ff-skip-boring-files t)
 '(helm-show-completion-display-function (quote helm-show-completion-default-display-function))
 '(history-length 5000)
 '(indent-tabs-mode nil)
 '(ivy-mode 1)
 '(ivy-rich-mode 1)
 '(ivy-use-virtual-buffers t)
 '(lsp-auto-configure t)
 '(lsp-auto-guess-root t)
 '(lsp-prefer-flymake nil)
 '(lsp-solargraph-use-bundler t)
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-doc-use-childframe t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(migemo-coding-system (quote utf-8))
 '(migemo-command "cmigemo")
 '(migemo-options (quote ("-q" "--emacs")))
 '(migemo-regex-dictionary nil)
 '(migemo-user-dictionary nil)
 '(package-selected-packages
   (quote
    (lsp-ivy tide groovy-mode typescript-mode restclient ein yasnippet yarn-mode yaml-mode which-key web-mode vue-mode volatile-highlights use-package undo-tree treemacs-projectile treemacs-magit swift-mode solarized-theme smartparens rubocopfmt rspec-mode robe restart-emacs recentf-ext rainbow-delimiters projectile-rails popwin open-junk-file omnisharp objc-font-lock nlinum nginx-mode migemo lsp-ui lsp-treemacs json-mode ivy-rich ivy-hydra indent-tools highlight-indent-guides haml-mode gradle-mode govet go-tag go-projectile go-impl go-gen-test gitignore-mode gitconfig-mode gitattributes-mode git-gutter-fringe+ flymake-haml flycheck-color-mode-line flutter expand-region exec-path-from-shell doom-modeline dockerfile-mode dired-hide-dotfiles diminish ddskk dart-mode dap-mode coverage counsel-projectile company-statistics company-quickhelp company-lsp company-inf-ruby color-theme-modern bundler bm beacon auto-package-update auto-async-byte-compile apib-mode all-the-icons-ivy all-the-icons-dired)))
 '(popwin-mode 1)
 '(projectile-completion-system (quote ivy))
 '(recentf-max-saved-items 1000)
 '(ruby-insert-encoding-magic-comment nil)
 '(save-place-mode 1)
 '(savehist-mode 1)
 '(scroll-margin 0)
 '(show-paren-mode 1)
 '(show-trailing-whitespace t)
 '(skk-use-jisx0201-input-method t)
 '(sort-fold-case t t)
 '(sourcekit-sourcekittendaemon-executable "/usr/local/bin/sourcekittendaemon")
 '(sourcekit-verbose t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(whitespace-display-mappings (quote ((space-mark 12288 [9633]) (tab-mark 9 [187 9]))))
 '(whitespace-space-regexp "\\(　+\\)")
 '(whitespace-style (quote (face tabs tab-mark spaces space-mark)))
 '(xcode-completing-read-function (quote ivy-completing-read))
 '(yas-prompt-functions (quote (yas-popup-isearch-prompt yas/completing-prompt))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
