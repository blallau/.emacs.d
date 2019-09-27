(use-package rst-mode
  :defer t
  :mode (("\\.rst\\'" . rst-mode))
  :config
  (defun my-keybindings-rst-hook ()
    (local-set-key (kbd "<f3>") 'rst-forward-section)
    (local-set-key (kbd "<f2>") 'rst-backward-section)
    (local-set-key (kbd "<f1>") 'rst-toc))
  (add-hook 'rst-mode-hook #'my-keybindings-rst-hook)
