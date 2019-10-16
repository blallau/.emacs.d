(use-package ansible-doc
  :ensure t
  :config
  (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  (add-hook 'yaml-mode-hook
            (lambda ()
              (local-set-key (kbd "<f1>") 'ansible-doc))))
