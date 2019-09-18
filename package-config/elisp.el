;; (defun byte-compile-current-buffer ()
;;   "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
;;   (interactive)
;;   (when (and (eq major-mode 'emacs-lisp-mode)
;;              (file-exists-p (byte-compile-dest-file buffer-file-name)))
;;     (byte-compile-file buffer-file-name)))

;; (add-hook 'after-save-hook #'byte-compile-current-buffer)

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "<f1>") 'describe-function)
;;             (local-set-key (kbd "<f9>") 'edebug-defun)
;;             ))

;;(use-package emacs-lisp-mode
;;  :mode
;;  (("*scratch*" . emacs-lisp-mode))
;;  :init
;;  (add-hook 'emacs-lisp-mode-hook
;;            (lambda ()
;;              (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)
;;              (setq-local outline-regexp ";; ----------\\|^;;;")))
;;
;;  (defun byte-compile-current-buffer ()
;;    "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
;;    (interactive)
;;    (when (and (eq major-mode 'emacs-lisp-mode)
;;               (file-exists-p (byte-compile-dest-file buffer-file-name)))
;;      (byte-compile-file buffer-file-name)))
;;  (add-hook 'after-save-hook #'byte-compile-current-buffer)
;;
;;  (general-define-key
;;   :keymaps 'emacs-lisp-mode-map
;;   :prefix "ê"
;;   "" '(:ignore t :which-key "Emacs Help")
;;   "f" 'counsel-describe-function
;;   "k" 'counsel-descbinds
;;   "v" 'counsel-describe-variable
;;   "e" 'eval-last-sexp
;;   "b" 'eval-buffer
;;   "c" '(sam--eval-current-form-sp :which-key "eval-current")
;;   "u" 'use-package-jump
;;   "t" '(lispy-goto :which-key "goto tag")))
