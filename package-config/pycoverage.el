(require 'linum)
(require 'pycoverage)

(defun my-coverage ()
  (interactive)
  (when (derived-mode-p 'python-mode)
    (progn
      (linum-mode)
      (pycoverage-mode)
      )
    )
  )

(add-hook 'python-mode-hook 'my-coverage)
