(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)
(global-set-key (kbd "C-x C-p") popwin:keymap)

(nconc popwin:special-display-config
       '(("magit" :regexp t :height 0.4 :stick t)))
