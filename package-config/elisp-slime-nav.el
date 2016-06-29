;; (use-package elisp-slime-nav
;;   :defer t

;;   :bind
;;   (("<f1>" . describe-foo-at-point)
;;    ("<f2>" . pop-tag-mark)
;;    ("<f3>" . elisp-slime-nav-find-elisp-thing-at-point))

;;   :config
;;   (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;     (add-hook hook #'elisp-slime-nav-mode))

;;   ;; describe this point lisp only
;;   (defun describe-foo-at-point ()
;;     "Show the documentation of the Elisp function and variable near point.
;;     This checks in turn:
;;     -- for a function name where point is
;;     -- for a variable name where point is
;;     -- for a surrounding function call
;;     "
;;     (interactive)
;;     (let (sym)
;;       ;; sigh, function-at-point is too clever.  we want only the first half.
;;       (cond ((setq sym (ignore-errors
;;                          (with-syntax-table emacs-lisp-mode-syntax-table
;;                            (save-excursion
;;                              (or (not (zerop (skip-syntax-backward "_w")))
;;                                  (eq (char-syntax (char-after (point))) ?w)
;;                                  (eq (char-syntax (char-after (point))) ?_)
;;                                  (forward-sexp -1))
;;                              (skip-chars-forward "`'")
;;                              (let ((obj (read (current-buffer))))
;;                                (and (symbolp obj) (fboundp obj) obj))))))
;;              (describe-function sym))
;;             ((setq sym (variable-at-point)) (describe-variable sym))
;;             ;; now let it operate fully -- i.e. also check the
;;             ;; surrounding sexp for a function call.
;;             ((setq sym (function-at-point)) (describe-function sym))))))

(eval-when-compile
  (require 'elisp-slime-nav))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook #'elisp-slime-nav-mode))

;; describe this point lisp only
(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
    This checks in turn:
    -- for a function name where point is
    -- for a variable name where point is
    -- for a surrounding function call
    "
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-at-point)) (describe-function sym)))))

(defun my-keybindings-lisp-hook ()
  (local-set-key (kbd "<f3>") 'elisp-slime-nav-find-elisp-thing-at-point)
  (local-set-key (kbd "<f2>") 'pop-tag-mark)
  (local-set-key (kbd "<f1>") 'describe-foo-at-point))

(add-hook 'emacs-lisp-mode-hook #'my-keybindings-lisp-hook)
