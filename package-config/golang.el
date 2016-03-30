(require 'go-mode)
(require 'go-projectile)

;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get golang.org/x/tools/cmd/oracle

(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook '(lambda ()
                           (local-set-key (kbd "<f1>") 'godoc-at-point)
                           (local-set-key (kbd "<f2>") 'pop-tag-mark)
                           (local-set-key (kbd "<f3>") 'godef-jump)
                           (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                           (local-set-key (kbd "C-c C-g") 'go-goto-imports)
                           (local-set-key (kbd "C-c C-f") 'gofmt)
                           (local-set-key (kbd "C-c C-k") 'godoc)))

;; eldoc
(defun go-mode-setup ()
  (go-eldoc-setup))

(add-hook 'go-mode-hook 'go-mode-setup)

;;(load "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
;;(add-hook 'go-mode-hook 'go-oracle-mode)

;; autocomplete
;;
(require 'auto-complete-config)
(require 'go-autocomplete)
(ac-config-default)
