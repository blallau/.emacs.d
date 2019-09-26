(use-package git-link
  :config
  (setq git-link-open-in-browser t
        ;; use the latest commit's hash in the link instead of the branch name
        git-link-use-commit t))
;;(global-set-key (kbd "C-c g l") 'git-link)
