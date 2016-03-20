(require 'go-mode)

(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook '(lambda ()
                           (local-set-key (kbd "<f3>") 'godef-jump)
                           (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                           (local-set-key (kbd "C-c C-g") 'go-goto-imports)
                           (local-set-key (kbd "C-c C-f") 'gofmt)
                           (local-set-key (kbd "C-c C-k") 'godoc)))

;; eldoc
(defun go-mode-setup ()
  (go-eldoc-setup))

(add-hook 'go-mode-hook 'go-mode-setup)

;; autocomplete
;;
;; go get -u github.com/nsf/gocode
(require 'auto-complete-config)
(require 'go-autocomplete)
