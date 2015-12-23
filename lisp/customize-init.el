;;default to home directory
(setq default-directory "~/")

; prevent tramp from messing up recentf
(require 'recentf)
    (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)

(add-to-list 'auto-mode-alist
             '("\\.emacs\\-[a-z\\-]" . emacs-lisp-mode))

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq undo-tree-history-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; disable .#foo style symlinks
(setq create-lockfiles nil)
