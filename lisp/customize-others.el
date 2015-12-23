;; Save for Future Sessions
;; File used for storing customization information.
(setq custom-file "~/.emacs.d/lisp/custom.el")
(load custom-file 'noerror)

;; Emacs Configure
(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq inhibit-startup-message t)
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; disable PAGER mode for projectile-git-grep
(setenv "GIT_PAGER" "/bin/cat")

;; customize grep-find command
;;(setq grep-find-command "find . -type f '!' -wholename '*/.git/*' -print0 | xargs -0 -e grep -nH -e ")
(defun helm-projectile-grep-ori ()
  "helm projectile without git"
  ()
  (
   (setq projectile-use-git-grep nil)
   (eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories ".git")
     (add-to-list 'grep-find-ignored-directories ".tox")
     (add-to-list 'grep-find-ignored-directories "locale")
     (add-to-list 'grep-find-ignored-directories "tests")))
   )
  'helm-projectile-grep
  )

;(setq grep-find-command "find . -type f '!' -wholename '*/.tox/*' -a '!' -wholename '*/.git/*' -a '!' -wholename '*/.eggs/*' -a '!' -wholename '*/tests/*' -print0 | xargs -0 -e grep -nH -e ")
(global-set-key (kbd "C-<f4>") 'helm-projectile-grep-ori)

;; checks whether the parent directories exist for a given file
;; and offers to create them if they do not exist.
(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory '%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "<f1>") 'describe-function)
        ))
