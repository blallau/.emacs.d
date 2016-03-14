(use-package org-projectile
  :bind (("C-c n p" . org-projectile:project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile:projects-file "~/work/org/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p")))
  :ensure t)
