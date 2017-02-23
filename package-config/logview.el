(use-package logview
  :defer t
  :mode ("\\.log\\'" . logview-mode)
  :config
  (setq logview-show-ellipses nil))
