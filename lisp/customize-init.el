;; allow functions to be redefined with defadvice
;; ex: ad-handle-definition: `tramp-read-passwd' got redefined
(setq ad-redefinition-action 'accept)

;; File used for storing customization information.
;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;default to home directory
(setq default-directory "~/")

(setq user-mail-address "bertrand.lallau@gmail.com")

;; disable PAGER mode for projectile-git-grep
(setenv "GIT_PAGER" "/bin/cat")


(add-to-list 'auto-mode-alist
             '("\\.emacs\\-[a-z\\-]" . emacs-lisp-mode))

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(require 'undo-tree)
(setq undo-tree-history-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; disable .#foo style symlinks
(setq create-lockfiles nil)

;; disable ~ files
(setq make-backup-files nil)

(use-package use-package-ensure-system-package
  :ensure t)
