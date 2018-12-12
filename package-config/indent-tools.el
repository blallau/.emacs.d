(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'indent-tools)
(global-set-key (kbd "C-c >") 'indent-tools-hydra/body)
(add-hook 'yaml-mode-hook
 (lambda () (define-key yaml-mode-map (kbd "C-c >") 'indent-tools-hydra/body))
)
