(require 'flycheck)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "<f7>") 'flycheck-previous-error)
(global-set-key (kbd "<f8>") 'flycheck-next-error)

;; make Emacs display buffers in a sane way.
;; Emacs 24.1 with the new display-buffer-alist
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.2)))

(global-set-key (kbd "<C-f7>") 'flycheck-list-errors)
(global-set-key (kbd "<C-f8>") 'flycheck-list-errors)
