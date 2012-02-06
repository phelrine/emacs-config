(require 'twittering-mode)

(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
(setq twittering-scroll-mode t)
(setq twittering-display-remaining t)
(setq twittering-status-format
      "%i%s %S
%FILL[  ]{%T}
%FILL[  ]{- %@ from %f%r%R -}\n")

(require 'inertial-scroll)
(add-hook 'twittering-mode-hook
          (lambda ()
            (local-set-key (kbd "<wheel-up>") 'inertias-down-wheel)
            (local-set-key (kbd "<wheel-down>") 'inertias-up-wheel)
            (local-set-key (kbd "<mouse-4>") 'inertias-down-wheel)
            (local-set-key (kbd "<mouse-5>") 'inertias-up-wheel)
            (local-set-key (kbd "M-v") 'inertias-down)
            (local-set-key (kbd "C-v") 'inertias-up)
            (local-set-key (kbd "o") 'other-window)
            ))

(add-hook 'twittering-edit-mode-hook 'skk-mode)

(defun twit-two-pane ()
  (interactive)
  (if (not (one-window-p)) (delete-other-windows))
  (twittering-home-timeline)
  (split-window-horizontally)
  (other-window 1)
  (twittering-replies-timeline))

(global-set-key (kbd "C-x t") 'twit-two-pane)