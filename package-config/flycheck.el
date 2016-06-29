(use-package flycheck
  :defer t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; make Emacs display buffers in a sane way.
  ;; Emacs 24.1 with the new display-buffer-alist
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2)))
  :bind
  (("<f7>" . flycheck-previous-error)
   ("<f8>" . flycheck-next-error)
   ("<C-f8>" . flycheck-list-errors)))
