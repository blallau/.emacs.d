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

;; Easier Window Switching using meta key
(windmove-default-keybindings 'meta)


;; unique buffer names dependent on file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; checks whether the parent directories exist for a given file
;; and offers to create them if they do not exist.
(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory '%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; scroll 2 windows with mouse
(defun my-scroll-down (arg)
  "scroll-down all visible windows."
  (interactive "P")
  (let ((num-windows (count-windows))
        (count 0))
    (while (< count num-windows)
      (scroll-down 1)
      (other-window 1)
      (setq count (1+ count)))))

(defun my-scroll-up (arg)
  "scroll-up all visible windows."
  (interactive "P")
  (let ((num-windows (count-windows))
  (count 0))
    (while (< count num-windows)
    (scroll-up 1)
    (other-window 1)
    (setq count (1+ count)))))

(defun my-scroll-down-all (arg)
  "scroll-down all visible windows."
  (interactive "P")
  (let ((num-windows (count-windows))
  (count 0))
    (while (< count num-windows)
    (scroll-down)
    (other-window 1)
    (setq count (1+ count)))))

(defun my-scroll-up-all (arg)
  "scroll-up all visible windows."
  (interactive "P")
  (let ((num-windows (count-windows))
  (count 0))
    (while (< count num-windows)
    (scroll-up)
    (other-window 1)
    (setq count (1+ count)))))

(global-set-key [C-mouse-5] 'my-scroll-up)
(global-set-key [C-mouse-4] 'my-scroll-down)

(global-set-key [S-mouse-5] 'my-scroll-up-all)
(global-set-key [S-mouse-4] 'my-scroll-down-all)

;; cursor is always given 3 lines of context before the top or bottom of the window
(setq scroll-margin 3)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name)
