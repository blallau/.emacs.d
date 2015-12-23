(require 'git-messenger) ;; You need not to load if you install with package.el
;(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
(global-set-key (kbd "<f5>") 'git-messenger:popup-message)

(define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)

;; Enable magit-commit-mode after typing 's', 'S', 'd'
;;(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
