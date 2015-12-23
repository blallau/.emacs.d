;; disable elpy-module-highlight-indentation
(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

;; (when (require 'elpy nil t)
;;   (elpy-enable))
;; (when (executable-find "ipython")
;;   (elpy-use-ipython))

(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-timeout 1)

(add-hook 'elpy-mode-hook
          (lambda ()
            (local-set-key (kbd "<f1>") 'elpy-doc)
            (local-set-key (kbd "<f2>") 'pop-tag-mark)
            (local-set-key (kbd "<f3>") 'elpy-goto-definition)

            (local-set-key (kbd "<f7>") 'elpy-flymake-previous-error)
            (local-set-key (kbd "<f8>") 'elpy-flymake-next-error)
            ; narrow the current buffer to the current function
            ; using C-x n d to restrict where this matches.
            (local-set-key (kbd "C-c C-e") 'elpy-multiedit-python-symbol-at-point)
            ; formats code using yapf or autopep8 formatter
            ;(local-set-key (kbd "C-c C-r f") 'elpy-format-code)
            ;(local-set-key (kbd "C-c C-r i") 'elpy-importmagic-fixup)
        ))
