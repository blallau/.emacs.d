(when (require 'elpy nil t)
  (elpy-enable))

(setq elpy-rpc-backend "jedi")

(when (executable-find "ipython")
  (elpy-use-ipython))

;; (when (el-get-package-installed-p 'flycheck)
;;   (setq elpy-default-minor-modes
;;         (remove 'flymake-mode
;;                 elpy-default-minor-modes)))

(define-key python-mode-map (kbd "RET")
  'newline-and-indent)

;; (custom-set-variables
;;  '(elpy-mode-hook nil)
;;  )

;; remove highlight indentation
;;(elpy-mode-hook nil)
; elpy-project-ignored-directories
;; (add-hook 'elpy-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "<f1>") 'elpy-doc)
;;             (local-set-key (kbd "<f2>") 'pop-tag-mark)
;;             (local-set-key (kbd "<f3>") 'elpy-goto-definition)

;;             (local-set-key (kbd "<f7>") 'elpy-flymake-previous-error)
;;             (local-set-key (kbd "<f8>") 'elpy-flymake-next-error)
;;             ; narrow the current buffer to the current function
;;             ; using C-x n d to restrict where this matches.
;;             (local-set-key (kbd "C-c C-e") 'elpy-multiedit-python-symbol-at-point)
;;             ; formats code using yapf or autopep8 formatter
;;             (local-set-key (kbd "C-c C-r f") 'elpy-format-code)
;;             (local-set-key (kbd "C-c C-r i") 'elpy-importmagic-fixup)
;;         ))

(local-set-key (kbd "<f1>") 'elpy-doc)
(local-set-key (kbd "<f2>") 'pop-tag-mark)
(local-set-key (kbd "<f3>") 'elpy-goto-definition)

(local-set-key (kbd "<f7>") 'elpy-flymake-previous-error)
(local-set-key (kbd "<f8>") 'elpy-flymake-next-error)
            ; narrow the current buffer to the current function
            ; using C-x n d to restrict where this matches.
(local-set-key (kbd "C-c C-e") 'elpy-multiedit-python-symbol-at-point)
            ; formats code using yapf or autopep8 formatter
(local-set-key (kbd "C-c C-r f") 'elpy-format-code)
(local-set-key (kbd "C-c C-r i") 'elpy-importmagic-fixup)
