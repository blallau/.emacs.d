(use-package ange-ftp
  :init
  (progn
    (setq ange-ftp-try-passive-mode t)
    (setq ange-ftp-passive-host-alist '(("mafreebox.freebox.fr" . "on"))))
  :no-require t)
