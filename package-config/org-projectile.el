(require 'org-projectile)
(setq org-projectile:projects-file "~/org/projects.org")
(setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
(setq org-projectile:capture-template "* TODO %? :prj:\n"
      org-projectile:linked-capture-template "* TODO %? %A :prj:\n")

(add-to-list 'org-capture-templates (org-projectile:project-todo-entry))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c n p") 'org-projectile:project-todo-completing-read)
;; disable prompt
(setq org-confirm-elisp-link-function nil)
