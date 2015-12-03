;; disable PAGER mode for projectile-git-grep
(setenv "GIT_PAGER" "/bin/cat")

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

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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
     (add-to-list 'grep-find-ignored-directories "tests")))
   )
  'helm-projectile-grep
  )

;(setq grep-find-command "find . -type f '!' -wholename '*/.tox/*' -a '!' -wholename '*/.git/*' -a '!' -wholename '*/.eggs/*' -a '!' -wholename '*/tests/*' -print0 | xargs -0 -e grep -nH -e ")
(global-set-key (kbd "C-<f4>") 'helm-projectile-grep-ori)

;; Don't break out a separate frame for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;
;; Next and previous buffer ignore *...* buffer
;;
(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
"Advice around `next-buffer' to avoid going into the *Messages* buffer."
;;(when (string= "*Messages*" (buffer-name))
(when (string-match "\\*.*\\*" (buffer-name))
(next-buffer)))
;; activate the advice
(ad-activate 'next-buffer)
(defadvice previous-buffer (after avoid-messages-buffer-in-next-buffer)
"Advice around `previous-buffer' to avoid going into the *Messages* buffer."
;;(when (string= "*Messages*" (buffer-name))
(when (string-match "\\*.*\\*" (buffer-name))
  (previous-buffer)))

;; activate the advice
(ad-activate 'previous-buffer)

;;;;;;;;;;;;
;; ShortCuts
;;;;;;;;;;;;
(global-set-key "\M-g" 'goto-line)
;;(global-set-key "\M-\"" 'indent-for-comment)
(global-set-key "\M-\"" 'comment-dwim)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Easier Window Switching using meta key
(windmove-default-keybindings 'meta)

;; http://www.emacswiki.org/emacs/FullScreen
(defun ome-toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(global-set-key (kbd "<f11>") 'ome-toggle-fullscreen)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
    (concat dired-omit-files "\\|\\.pyc$"))
(setq dired-recursive-deletes 'always)

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

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
