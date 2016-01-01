(require 'magit)

;; Configure git-commit
(setq git-commit-summary-max-length 50)
(setq git-commit-fill-column 72)

;; Configure Magit
;; Disable the `highlight` face that Magit uses to highlight diffs. It's unreadable with my color scheme.
;; An unreadable highlight face is a common issue on the Magit tracker.
(defun disable-magit-highlight-in-buffer () (face-remap-add-relative 'magit-item-highlight '()))
(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)
(add-hook 'magit-commit-mode-hook 'disable-magit-highlight-in-buffer)
(add-hook 'magit-diff-mode-hook 'disable-magit-highlight-in-buffer)

(setq magit-log-arguments (quote ("--graph" "--color" "--decorate" "++header" "--no-merges" "-n256")))
(setq magit-revert-buffers t)
(setq magit-save-repository-buffers t)

;(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-<f5>") 'magit-blame)
(global-set-key (kbd "<f6>") 'magit-status)
(global-set-key (kbd "C-<f6>") 'magit-log-buffer-file)
