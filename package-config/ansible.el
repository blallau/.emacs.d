;; (use-package ansible-doc
;;   :ensure t
;;   :config
;;   (add-hook 'yaml-mode-hook #'ansible-doc-mode)
;;   (add-hook 'yaml-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "<f1>") 'ansible-doc)
;;               (ansible 1))))

(use-package ansible-doc
  :ensure t
  :config
  (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  (add-hook 'yaml-mode-hook
            (lambda ()
              (local-set-key (kbd "<f1>") 'ansible-doc))))

;; Minor mode
(use-package jinja2-mode
  :defer t
  :mode ("\\.j2\\'" . jinja2-mode))
