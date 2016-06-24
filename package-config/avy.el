;; How many seconds to wait for the second char
(setq avy-timeout-seconds 0.8
      ;; ignore case
      avy-case-fold-search t
      ;; use all windows on the selected frame
      avy-all-windows t)
;; Input an arbitrary amount of consecutive chars, jump to the first one with a tree.
(global-set-key (kbd "C-:") 'avy-goto-char-timer)
