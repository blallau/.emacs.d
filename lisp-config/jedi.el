;; Use projectile to detect python project
(require 'projectile)
;; load the known projects
(projectile-load-known-projects)

(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)

(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "<f3>") 'jedi:goto-definition)
            (local-set-key (kbd "<f2>") 'jedi:goto-definition-pop-marker)
            (local-set-key (kbd "<f1>") 'jedi:show-doc)
;;            (local-set-key (kbd "M-/") 'jedi:get-in-function-call)
        ))
