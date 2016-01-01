;;;;;;;;;;
;; GUI
;;;;;;;;;;

(setq frame-title-format
      '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
		"%b"))))

;; disable gui crap
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; hide menu and tool bar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; disable help popup in modeline
(setq show-help-function nil)
;; disable splash screen
(setq inhibit-splash-screen t)
;; disable startup message
(setq inhibit-startup-message t)

;;;;;;;;;;;
;; theme
;;;;;;;;;;;
; color theme
(require 'moe-theme)
(moe-dark)
(setq moe-theme-mode-line-color 'blue)

;; define theme
(deftheme my-theme
  "My theme")

;; Set faces
(custom-theme-set-faces
 'my-theme ;; you must use the same theme name here...
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal))))
; '(cursor  ((t (:background ,color-4))))
; '(fringe  ((t (:background ,color-3))))
       ;;; etc...
       ;;; don't use these settings of course,
       ;;; they're horrible.
 )
(provide-theme 'my-theme)

;; http://www.emacswiki.org/emacs/FullScreen
(defun ome-toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(ome-toggle-fullscreen)

(global-set-key (kbd "<f11>") 'ome-toggle-fullscreen)

;;;;;;;;;;;
;; Fringe
;;;;;;;;;;;
;; specify the fringe width for windows
;; this sets both the left and right fringes
(require 'fringe)
;; make the left fringe 4 pixels wide and the right disappear
(fringe-mode 0)
;;(fringe-mode '(4 . 0))

(column-number-mode 't)

;;;;;;;;;;;;;;;;
;; Zoom in/out
;;;;;;;;;;;;;;;;
(require 'face-remap)
(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc)
  (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1))

(global-set-key (kbd "M-0")
		'(lambda ()
		   (interactive)
		   (global-text-scale-adjust (- text-scale-mode-amount))
		   (global-text-scale-mode -1)))
(global-set-key (kbd "M-+")
		'(lambda ()
		   (interactive)
		   (global-text-scale-adjust 1)))
(global-set-key (kbd "M--")
		'(lambda ()
		   (interactive)
		   (global-text-scale-adjust -1)))

;;;;;;;;;;
;; Others
;;;;;;;;;;
;; Don't break out a separate frame for ediff
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
    (concat dired-omit-files "\\|\\.pyc$"))
(setq dired-recursive-deletes 'always)
