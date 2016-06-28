(use-package multi-term
  :defer t
  :config
  (setq multi-term-program "/bin/bash"
        term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-y" "C-r")))
