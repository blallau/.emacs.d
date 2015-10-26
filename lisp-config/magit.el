(defvar pc:magit-from-buffer
  "Buffer from where magit-status were last called."
  nil)

(defun pc:magit-status-buffer-switch (buf)
  "My replacement for `magit-status-buffer-switch-function'.
`magit-status' does not split windows (switch to magit buffer
instead). Also store the current buffer to switch back to it when
quitting.
TODO: store the whole frame config instead?"
  (setq pc:magit-from-buffer (current-buffer))
  (switch-to-buffer buf))
(setq magit-status-buffer-switch-function 'pc:magit-status-buffer-switch)

;; Disable the `highlight` face that Magit uses to highlight diffs. It's unreadable with my color scheme.
;; An unreadable highlight face is a common issue on the Magit tracker.
(defun disable-magit-highlight-in-buffer () (face-remap-add-relative 'magit-item-highlight '()))
(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)
(add-hook 'magit-commit-mode-hook 'disable-magit-highlight-in-buffer)
(add-hook 'magit-diff-mode-hook 'disable-magit-highlight-in-buffer)

;(magit-define-popup-switch 'magit-log-popup "nm" "No merges" "--no-merges")

;; Configure Magit
(setq git-commit-summary-max-length 50)
(setq git-commit-fill-column 72)

(setq magit-commit-signoff t)
(setq magit-remote-ref-format 'remote-slash-branch)
;(setq magit-completing-read-function 'magit-iswitchb-completing-read)
(setq magit-save-some-buffers nil)

; magit 1.4.0 (no revert before git)
(setq magit-auto-revert-mode t)
(setq magit-last-seen-setup-instructions "1.4.0")

;(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-<f5>") 'magit-blame)
(global-set-key (kbd "<f6>") 'magit-status)

;; ;deactive log merges by default
;; (eval-after-load 'magit-mode
;;   (progn
;; ;    '(magit-key-mode-insert-switch 'logging "-nm" "No merges" "--no-merges")
;;     '(magit-key-mode-insert-switch 'logging "-m" "No merge commits" "--no-merges")
;;     ))


;(lambda nil (interactive)
;  (magit-key-mode-toggle-option (quote logging) "--no-merges"))

;(advice-add #'magit-key-mode-popup-logging :after
;            (lambda ()
;              (magit-key-mode-toggle-option (quote logging) "--verbose")))

;(lambda nil (interactive)
;  (magit-key-mode-toggle-option (quote logging) "--no-merges"))
