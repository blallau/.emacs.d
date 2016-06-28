(use-package avy
  :config
  ;; How many seconds to wait for the second char
  (setq avy-timeout-seconds 0.8
        ;; ignore case
        avy-case-fold-search t
        ;; use all windows on the selected frame
        avy-all-windows t)
  :bind
  (("C-:" . avy-goto-char-timer)))
