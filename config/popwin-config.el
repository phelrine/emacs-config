(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)
(global-set-key (kbd "C-x C-p") popwin:keymap)

(push '("magit" :regexp t :height 0.4) popwin:special-display-config)
