;; (require 'linum)
;; (require 'magit)
;; (require 'couette)

;; (defconst coverage-results-file ".coverage" "PYTHON Coverage results file.")

;; (defun couette-enabled ()
;;   (let ((project-root (magit-toplevel)))
;;     (if (file-exists-p (concat project-root coverage-results-file))
;;         t
;;       nil)))

;; (defun my-couette ()
;;   (interactive)
;;   (when (derived-mode-p 'python-mode)
;;     (progn
;;       (when (couette-enabled)
;;         (linum-mode t)
;;         (couette-mode)))))

;; (add-hook 'python-mode-hook #'my-couette)
