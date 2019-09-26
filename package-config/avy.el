;; move point to any position in Emacs – even in a different window using very few keystrokes.
;; For this, you look at the position where you want point to be, invoke Avy, and then enter
;; the sequence of characters displayed at that position.
;;
;; (use-package avy
;;   :config
;;   ;; How many seconds to wait for the second char
;;   (setq avy-timeout-seconds 0.8
;;         ;; ignore case
;;         avy-case-fold-search t
;;         ;; use all windows on the selected frame
;;         avy-all-windows t)
;;   :bind
;;   (("C-:" . avy-goto-char-timer)))
