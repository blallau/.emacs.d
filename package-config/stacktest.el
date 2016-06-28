(use-package stacktest
  :init
  (add-hook 'python-mode-hook 'stacktest-mode)
  :bind (:map python-mode-map
              (("<f10>" . stacktest-one)
               ("<C-f10>" . stacktest-pdb-one)
               ("C-S-<f10>" . stacktest-module))))
