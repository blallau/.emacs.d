(use-package golden-ratio
  :defer t
  :ensure t
  :config
  (setq golden-ratio-exclude-modes '(dired-mode
                                     ediff-mode
                                     erc-mode
                                     flycheck-error-list-mode
                                     )
        golden-ratio-exclude-buffer-regexp
        `(,(rx bos "*which-key*" eos)))
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1))
