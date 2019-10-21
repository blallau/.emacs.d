(use-package company
  :defer t
  :bind
  ("M-TAB" . company-complete)
;;  ("M-;" . company-yasnippet)
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  :config
  (define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1)))
  :hook
  (after-init . global-company-mode))

;; Ansible keywords completion for Emacs
(use-package company-ansible
  :defer t)
