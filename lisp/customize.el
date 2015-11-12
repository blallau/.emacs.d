;; disable PAGER mode for projectile-git-grep
(setenv "GIT_PAGER" "/bin/cat")

;; Save for Future Sessions
;; File used for storing customization information.
(setq custom-file load-file-name)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(expand-region-guess-python-mode nil)
 '(expand-region-preferred-python-mode (quote python))
 '(magit-diff-use-overlays nil)
 '(magit-log-arguments
   (quote
    ("--graph" "--color" "--decorate" "++header" "--no-merges" "-n256")))
; '(magit-log-section-arguments (quote ("--decorate")))
; '(magit-log-select-arguments (quote ("-n256" "--decorate")))
 '(magit-use-overlays nil)
 '(python-indent-guess-indent-offset nil)
 '(sp-autoescape-string-quote nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

;;;;;;;;;;;;
;; ShortCuts
;;;;;;;;;;;;
(global-set-key "\M-g" 'goto-line)
;;(global-set-key "\M-\"" 'indent-for-comment)
(global-set-key "\M-\"" 'comment-dwim)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; passer d'une fenetre du meme cadre a l'autre.
(global-set-key [(control tab)] 'other-window)

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
