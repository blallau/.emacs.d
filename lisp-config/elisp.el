;; ;; Elisp go-to-definition with M-. and back again with M-,
;; (autoload 'elisp-slime-nav-mode "elisp-slime-nav")
;; ;(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;; (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;   (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (elisp-slime-nav-mode t)
;;             (local-set-key (kbd "<f3>") 'elisp-slime-nav-find-elisp-thing-at-point)
;;             (local-set-key (kbd "<f2>") 'pop-tag-mark)
;;             (local-set-key (kbd "<f1>") 'describe-function)
;;         ))

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)
