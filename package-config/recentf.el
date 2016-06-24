;; prevent tramp from messing up recentf
(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode +1)
(setq recentf-max-saved-items 50
      recentf-max-menu-items 20
      recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
      recentf-exclude '("/tmp/" "/ssh:"))

;; trigger recentf-save-list each 5mn
(run-at-time (current-time) 300 'recentf-save-list)
