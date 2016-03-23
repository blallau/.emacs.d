(require 'org-projectile)
(setq org-projectile:projects-file "~/org/projects.org")
(setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
(setq org-projectile:capture-template "* ☛ TODO %? :prj:\n"
      org-projectile:linked-capture-template "* %? %A :prj:\n")

(add-to-list 'org-capture-templates (org-projectile:project-todo-entry))

(global-set-key (kbd "C-c c") 'org-capture)
;; disable prompt
(setq org-confirm-elisp-link-function nil)

(defun org-projectile:capture-for-current-project (&optional capture-template)
  (interactive)
  (let ((project-name (projectile-project-name)))
    (if (projectile-project-p)
        (org-projectile:capture-for-project project-name capture-template)
      (error (format "%s is not a recognized projectile project." project-name)))))
(global-set-key (kbd "C-c n p") 'org-projectile:capture-for-current-project)
