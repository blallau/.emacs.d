;; Make IPython the default shell

;; (use-package anaconda-mode
;;   :bind (:map anaconda-mode-map
;;               ("M-[" . python-nav-backward-block)
;;               ("M-]" . python-nav-forward-block)
;;               ("M-'" . anaconda-mode-find-references)
;;               ))

;; (use-package company-anaconda
;;   :config
;;   (eval-after-load "company"
;;     '(add-to-list 'company-backends 'company-anaconda)))

(use-package python
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
        ;;      python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

  ;; IPython command line args
  ;; use –colors=LightBG if you have a white background
  (add-hook 'python-mode-hook
            (lambda ()
              (setq py-python-command-args (quote ("--colors=Linux" "-i")))))

  ;; Python hook to Highlight TODO, FIXME, ...
  (add-hook 'python-mode-hook #'fic-mode)

  ;; Delete trailing whitespace when saving (compliance with PEP8)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  ;;----------------------------------------------------------------------------
  ;; ipdb
  ;; Highlight ipdb lines:
  (defun annotate-pdb ()
    (interactive)
    (highlight-lines-matching-regexp "import pdb")
    (highlight-lines-matching-regexp "pdb.set_trace()"))
  (add-hook 'python-mode-hook 'annotate-pdb)

  ;;----------
  ;; Keybinding to add breakpoint:
  (defun python-add-breakpoint ()
    (interactive)
    (newline-and-indent)
    (insert "import pdb; pdb.set_trace()")
    (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))

  (defun python-add-stacktrace ()
    (interactive)
    (newline-and-indent)
    (insert "import traceback; traceback.print_stack()")
    (highlight-lines-matching-regexp "^[ ]*import traceback; traceback.print_stack()"))

  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-b") 'python-add-breakpoint)
              (local-set-key (kbd "C-c C-t") 'python-add-stacktrace))))

;; ;; column witdh indicator
;; (require 'fill-column-indicator)
;; (setq fci-rule-column 79)
;; ;(setq fci-rule-color "DeepSkyBlue4")
;; (setq fci-rule-color "dim gray")
;; (setq fci-rule-use-dashes t)
;; (setq fci-dash-pattern 0.3)

;; (add-hook 'python-mode-hook 'fci-mode)

;(add-hook 'python-mode-hook 'whitespace-mode)

;; Hook up flymake to use the PEP8 style
;; (add-hook 'python-mode-hook
;;        (lambda ()
;;                (unless (eq buffer-file-name nil) (flymake-mode 1))
;;                (local-set-key [f5] 'flymake-goto-prev-error)
;;                (local-set-key [f6] 'flymake-goto-next-error)
;;        ))
;; (when (load "flymake" t)
;;        (defun flymake-pylint-init ()
;;                (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;                (local-file (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;                (list "pep8" (list "–repeat" local-file))))
;;        (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pylint-init)))
;; (defun my-flymake-show-help ()
;;        (when (get-char-property (point) 'flymake-overlay)
;;                (let ((help (get-char-property (point) 'help-echo)))
;;                        (if help (message "%s" help)))))
;; (add-hook 'post-command-hook 'my-flymake-show-help)
