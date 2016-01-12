;; (require 'yasnippet)

;; (yas-reload-all)
;; (add-hook 'python-mode-hook #'yas-minor-mode)

;; (defun yas--magit-email-or-default ()
;;   "Get email from GIT or use default"
;;   (if (magit-toplevel ".")
;;       (magit-get "user.email")
;;     user-mail-address))

;; (defun yas--magit-user-name ()
;;   "Get user-name from GIT"
;;   (when (magit-toplevel ".")
;;     (magit-get "user.name")))
