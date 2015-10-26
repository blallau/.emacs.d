;;
(require 'stacktest)

;; optionally enable for all python files
(add-hook 'python-mode-hook 'stacktest-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "<f10>") 'stacktest-one)
            (local-set-key (kbd "C-<f10>") 'stacktest-module)
;;            (local-set-key (kbd "<f11>") 'stacktest-again)
        ))
