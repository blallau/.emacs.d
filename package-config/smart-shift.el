;; make it easy to shift current line or region to
;; left/right according to current major mode indentation.
(use-package smart-shift
  :defer t
  :bind
  ("C-c <right>" . smart-shift-right)
  ("C-c <left>" . smart-shift-left)
  :config
  (advice-add 'smart-shift-override-local-map :override #'ignore))
