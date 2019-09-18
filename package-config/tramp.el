(use-package tramp
  :config
  (progn
    (setq tramp-default-method "ssh")
    ;; No messages
    (setq tramp-message-show-message nil)
    (setq tramp-use-ssh-controlmaster-options nil)
    ;; Connect to my freebox as 'freebox' user.
    (add-to-list 'tramp-default-user-alist
                 '("ftp" "\\`mafreebox\\.freebox\\.fr\\'" "freebox"))))
