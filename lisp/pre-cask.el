;;default to home directory
(setq default-directory "~/")

;;replace selection when typing
(delete-selection-mode 1)

; prevent tramp from messing up recentf
(require 'recentf)
    (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)

(setq tramp-debug-buffer t)

(add-to-list 'auto-mode-alist
             '("\\.emacs\\-[a-z\\-]" . emacs-lisp-mode))


;; bug workaround
(setq warning-suppress-types nil)


;;disable gui crap
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; hide menu and tool bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq inhibit-splash-screen t)

(defcustom mf-display-padding-width 50
  "*Any extra display padding that you want to account for while
determining the maximize number of columns to fit on a display"
  :type 'integer
  :group 'maxframe)

;; hippie-expand
(global-set-key (kbd "C-.") 'hippie-expand)

;default list
(setq hippie-expand-try-functions-list
  '(try-complete-file-name-partially
    try-complete-file-name
    try-expand-all-abbrevs
    try-expand-list
    try-expand-line
    try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol))

(setq tramp-default-method "ssh")

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

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
