(defun my-keybindings-md-hook ()
  (local-set-key (kbd "<f3>") 'markdown-next-visible-heading)
  (local-set-key (kbd "<f2>") 'markdown-previous-visible-heading))
  ;; (local-set-key (kbd "<f1>") 'TBD))

(add-hook 'markdown-mode-hook #'my-keybindings-md-hook)
