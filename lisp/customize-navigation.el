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
