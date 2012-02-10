(require 'elscreen)

(defun anything-elscreen()
  (interactive)
  (anything-other-buffer '(anything-c-source-elscreen) "*Anything Elscreen*"))

(setq elscreen-display-tab nil)
(global-set-key (kbd "C-z C-b") 'anything-elscreen)
