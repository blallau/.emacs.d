;;;;;;;;;;
;; GUI
;;;;;;;;;;

;; disable gui crap
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; hide menu and tool bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; disable help popup in modeline
(setq show-help-function nil)
;; disable splsh screen
(setq inhibit-splash-screen t)

;; http://www.emacswiki.org/emacs/FullScreen
(defun ome-toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(global-set-key (kbd "<f11>") 'ome-toggle-fullscreen)


;;;;;;;;;;
;; Others
;;;;;;;;;;

;; Don't break out a separate frame for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
    (concat dired-omit-files "\\|\\.pyc$"))
(setq dired-recursive-deletes 'always)
