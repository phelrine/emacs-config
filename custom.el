(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-expand-on-auto-complete t)
 '(ac-show-menu-immediately-on-auto-complete nil)
 '(auto-package-update-delete-old-versions t)
 '(auto-package-update-hide-results t)
 '(beacon-color "light green")
 '(beacon-mode t)
 '(bm-buffer-persistence t)
 '(bm-cycle-all-buffers t)
 '(bm-fringe-face 'bm-face)
 '(bm-fringe-persistent-face 'bm-face)
 '(bm-highlight-style 'bm-highlight-line-and-fringe)
 '(bm-persistent-face 'bm-face)
 '(cc-other-file-alist
   '(("\\.cpp$"
      (".hpp" ".h"))
     ("\\.h$"
      (".c" ".cpp" ".m" ".mm"))
     ("\\.hpp$"
      (".cpp" ".c"))
     ("\\.m$"
      (".h"))
     ("\\.mm$"
      (".h"))))
 '(company-backends
   '(company-capf company-dabbrev-code company-files company-elisp company-yasnippet))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay nil)
 '(company-lsp-enable-recompletion nil)
 '(company-statistics-mode t)
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(ein:output-type-preference 'ein:output-type-prefer-pretty-text-over-html)
 '(enable-recursive-minibuffers t)
 '(exec-path-from-shell-check-startup-files nil t)
 '(exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH"))
 '(forge-topic-list-limit '(50 . 0))
 '(global-auto-revert-mode t)
 '(global-company-mode 1)
 '(global-eldoc-mode 1)
 '(global-git-gutter+-mode t)
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(helm-boring-file-regexp-list '("~$" "\\.meta$"))
 '(helm-ff-skip-boring-files t)
 '(helm-show-completion-display-function 'helm-show-completion-default-display-function)
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
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-childframe t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(migemo-coding-system 'utf-8)
 '(migemo-command "cmigemo")
 '(migemo-options '("-q" "--emacs"))
 '(migemo-regex-dictionary nil)
 '(migemo-user-dictionary nil)
 '(package-selected-packages
   '(slack npm-mode npm browse-at-remote gotest realgud-pry realgud multiple-cursors prettier-mode prettier go-gen-test go-impl go-tag govet go-projectile lua-mode restart-emacs apib-mode nginx-mode docker-compose-mode dockerfile-mode docker tide typescript-mode vue-mode web-mode groovy-mode flutter lsp-dart dart-mode go-mode flymake-haml haml-mode coverage bundler rspec-mode projectile-rails rubocopfmt robe inf-ruby ein swift-mode objc-font-lock omnisharp csharp-mode auto-async-byte-compile restclient indent-tools highlight-indent-guides rainbow-delimiters smartparens flycheck-color-mode-line flycheck migemo ddskk beacon volatile-highlights undo-tree expand-region nlinum doom-modeline solarized-theme color-theme-modern open-junk-file popwin yasnippet shell-pop gist github-review forge git-gutter-fringe+ gitignore-mode gitconfig-mode gitattributes-mode magit dap-mode lsp-ivy lsp-ui lsp-mode company-quickhelp company-statistics company all-the-icons-dired dired-hide-dotfiles which-key counsel-projectile ivy-hydra ivy-rich all-the-icons-ivy counsel ivy projectile all-the-icons ace-window avy centaur-tabs recentf-ext exec-path-from-shell auto-package-update hydra diminish use-package))
 '(popwin-mode 1)
 '(projectile-completion-system 'ivy)
 '(projectile-rails-expand-snippet nil)
 '(recentf-max-saved-items 1000)
 '(ring-bell-function 'ignore)
 '(ruby-insert-encoding-magic-comment nil)
 '(save-place-mode 1)
 '(savehist-mode 1)
 '(scroll-margin 0)
 '(shell-pop-shell-type
   '("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))
 '(shell-pop-term-shell "/bin/zsh")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30)
 '(show-paren-mode 1)
 '(show-trailing-whitespace t)
 '(skk-use-jisx0201-input-method t)
 '(sort-fold-case t t)
 '(sourcekit-sourcekittendaemon-executable "/usr/local/bin/sourcekittendaemon")
 '(sourcekit-verbose t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(whitespace-display-mappings '((space-mark 12288 [9633]) (tab-mark 9 [187 9])))
 '(whitespace-space-regexp "\\(　+\\)")
 '(whitespace-style '(face tabs tab-mark spaces space-mark))
 '(xcode-completing-read-function 'ivy-completing-read)
 '(yas-prompt-functions '(yas-popup-isearch-prompt yas/completing-prompt)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
