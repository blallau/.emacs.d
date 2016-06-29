;; Highlight matching parenthesis (useful when coding)
(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-style 'mixed
        blink-matching-paren t
        blink-matching-paren-on-screen t
        blink-matching-paren-dont-ignore-comments t))
