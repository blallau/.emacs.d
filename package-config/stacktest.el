;; (use-package stacktest
;;   :init
;;   (add-hook 'python-mode-hook 'stacktest-mode)
;;   :bind (:map python-mode-map
;;               (("<f10>" . stacktest-one)
;;                ("<C-f10>" . stacktest-pdb-one)
;;                ("C-S-<f10>" . stacktest-module))))

;; (require 'stacktest)

;; ;; optionally enable for all python files
;; (add-hook 'python-mode-hook #'stacktest-mode)

;; (defun my-keybindings-stacktest-hook ()
;;   (local-set-key (kbd "<f10>") 'stacktest-one)
;;   (local-set-key (kbd "<C-f10>") 'stacktest-pdb-one)
;;   (local-set-key (kbd "C-S-<f10>") 'stacktest-module))

;; (add-hook 'python-mode-hook #'my-keybindings-stacktest-hook)
