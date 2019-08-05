(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-expand-on-auto-complete t)
 '(ac-show-menu-immediately-on-auto-complete nil)
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
 '(clang-format-style
   "{BasedOnStyle: google, IndentWidth: 4, BreakBeforeBraces: Linux, ObjCBlockIndentWidth: 4, ColumnLimit: 0, ObjCSpaceAfterProperty: true}")
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
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GOPATH")))
 '(global-auto-revert-mode t)
 '(global-company-mode 1)
 '(global-eldoc-mode 1)
 '(global-git-gutter+-mode t)
 '(global-hl-line-mode t)
 '(global-wakatime-mode t)
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
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-doc-use-webkit t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(migemo-coding-system (quote utf-8))
 '(migemo-command "cmigemo")
 '(migemo-options (quote ("-q" "--emacs")))
 '(migemo-regex-dictionary nil)
 '(migemo-user-dictionary nil)
 '(package-selected-packages
   (quote
    (ivy-hydra xpm major-mode-icons company-quickhelp company-box beacon diminish all-the-icons-ivy undo-tree spinner yasnippet yaml-mode which-key web-mode wakatime-mode vue-mode volatile-highlights use-package swift-mode solarized-theme smartparens rubocopfmt robe restart-emacs recentf-ext rainbow-delimiters projectile-rails popwin open-junk-file omnisharp objc-font-lock nlinum nginx-mode migemo magit lsp-ui json-mode ivy-rich indent-tools highlight-indent-guides haml-mode govet go-tag go-projectile go-impl go-gen-test gitignore-mode gitconfig-mode gitattributes-mode git-gutter-fringe+ flyspell-lazy flyspell-correct-ivy flycheck-color-mode-line flutter expand-region exec-path-from-shell ein doom-modeline dockerfile-mode dired-hide-dotfiles ddskk dart-mode dap-mode counsel-projectile company-statistics company-lsp color-theme-modern bundler bm auto-async-byte-compile apib-mode all-the-icons-dired)))
 '(popwin-mode 1)
 '(projectile-completion-system (quote ivy))
 '(recentf-max-saved-items 1000)
 '(ruby-insert-encoding-magic-comment nil)
 '(save-place-mode t)
 '(savehist-mode t)
 '(scroll-margin 0)
 '(show-paren-mode 1)
 '(skk-use-jisx0201-input-method t)
 '(sort-fold-case t t)
 '(sourcekit-sourcekittendaemon-executable "/usr/local/bin/sourcekittendaemon")
 '(sourcekit-verbose t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(volatile-highlights-mode t)
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
 '(bm-face ((t (:background "spring green" :overline nil :underline t))))
 '(highlight-indent-guides-even-face ((t (:background "wheat1"))))
 '(highlight-indent-guides-odd-face ((t (:background "wheat2"))))
 '(objc-font-lock-background ((t (:inherit nil))))
 '(whitespace-space ((t (:foreground "DarkGoldenrod1"))))
 '(whitespace-tab ((t (:foreground "dark blue" :inverse-video nil)))))
