(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . "ⓕ")
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  ;; controls how Flycheck indicates errors in buffers.
  (flycheck-indication-mode 'right-fringe)
  ;; Removed checks on idle/change for snappiness
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-highlighting-mode 'symbols)
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc make))

  (setq flycheck-display-errors-delay 0.5)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  ;; make Emacs display buffers in a sane way.
  ;; Emacs 24.1 with the new display-buffer-alist
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.23)))
  :bind
  (("<f7>" . flycheck-previous-error)
   ("<f8>" . flycheck-next-error)
   ("<C-f8>" . flycheck-list-errors)))
