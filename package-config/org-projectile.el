(require 'org-projectile)
(setq org-projectile:projects-file "~/work/org/projects.org")
(setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))

(add-to-list 'org-capture-templates (org-projectile:project-todo-entry))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c n p") 'org-projectile:project-todo-completing-read)
