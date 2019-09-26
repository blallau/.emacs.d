(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
  ;; :init (setq markdown-command "multimarkdown"))
  :config
  (defun my-keybindings-md-hook ()
    (local-set-key (kbd "<f3>") 'markdown-next-visible-heading)
    (local-set-key (kbd "<f2>") 'markdown-previous-visible-heading))
  (add-hook 'markdown-mode-hook #'my-keybindings-md-hook)
