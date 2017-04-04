(require 'autorevert)
;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)
