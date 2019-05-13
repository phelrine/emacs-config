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
    (company-bbdb company-nxml company-css company-eclim company-semantic company-cmake
                  (company-dabbrev-code :with company-clang)
                  (company-clang company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(ein:output-type-preference (quote ein:output-type-prefer-pretty-text-over-html))
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GOPATH")))
 '(global-auto-revert-mode t)
 '(global-git-gutter+-mode t)
 '(global-hl-line-mode t)
 '(global-wakatime-mode t)
 '(global-whitespace-mode t)
 '(helm-boring-file-regexp-list (quote ("~$" "\\.meta$")))
 '(helm-ff-skip-boring-files t)
 '(helm-show-completion-display-function (quote helm-show-completion-default-display-function))
 '(history-length 5000)
 '(indent-tabs-mode nil)
 '(ivy-display-function (quote ivy-posframe-display-at-point))
 '(ivy-use-virtual-buffers t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(migemo-coding-system (quote utf-8))
 '(migemo-command "cmigemo")
 '(migemo-options (quote ("-q" "--emacs")))
 '(migemo-regex-dictionary nil)
 '(migemo-user-dictionary nil)
 '(package-selected-packages
   (quote
    (dumb-jump dumb wakatime-mode apib-mode json-reformat web-mode go-gen-test go-impl go-tag govet go-projectile go-mode ein objc-font-lock omnisharp csharp-mode projectile volatile-highlights highlight-indent-guides nlinum flycheck-color-mode-line flycheck migemo rainbow-delimiters ddskk auto-async-byte-compile undo-tree expand-region doom-modeline solarized-theme color-theme-modern yaml-mode open-junk-file exec-path-from-shell popwin yasnippet git-gutter-fringe+ magit company-lsp lsp-ui company-statistics company dired-hide-dotfiles dired-k bm which-key counsel ivy-posframe ivy use-package)))
 '(recentf-max-saved-items 1000)
 '(save-place-mode t)
 '(savehist-mode t)
 '(show-paren-mode 1)
 '(show-trailing-whitespace t)
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
 '(flymake-errline ((t (:foreground "orange" :background "blue"))) t)
 '(flymake-warnline ((t (:background "yellow"))) t)
 '(highlight-indent-guides-even-face ((t (:background "wheat1"))))
 '(highlight-indent-guides-odd-face ((t (:background "wheat2"))))
 '(objc-font-lock-background ((t (:inherit nil))))
 '(whitespace-space ((t (:foreground "DarkGoldenrod1"))))
 '(whitespace-tab ((t (:foreground "dark blue" :inverse-video nil)))))
