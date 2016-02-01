;;;;;;;;;;
;; GUI
;;;;;;;;;;

(setq frame-title-format
      '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
		"%b"))))

;; disable gui crap
(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (tool-bar-mode -1))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which function in header
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode 1)

(add-to-list 'which-func-modes 'c-mode)
(add-to-list 'which-func-modes 'emacs-lisp-mode)
(add-to-list 'which-func-modes 'java-lisp-mode)
(add-to-list 'which-func-modes 'python-mode)
(add-to-list 'which-func-modes 'ruby-mode)
(add-to-list 'which-func-modes 'sh-mode)

;; Show the current function name in the header line
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line,
      ;; because it's mostly invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

;;;;;;;;;;
;; Others
;;;;;;;;;;
;; Don't break out a separate frame for ediff
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'dired-x)
(setq-default dired-omit-files-p t)
;; To toggle the mode
(define-key dired-mode-map (kbd "C-o") 'dired-omit-mode)

(setq-default dired-omit-verbose nil)
(setq dired-omit-files (concat dired-omit-files "\\|\\.pyc$"))

;; Stop asking me whether I want to recursively delete or copy
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; Print sizes in human readable format (e.g., 1K 234M 2G)
(setq dired-listing-switches "-alh")

;; Enables "Do What I Mean" mode
;; If I'm in a split frame with two dired buffers,
;; the default target to copy (and rename) will be the other window.
(setq dired-dwim-target t)
