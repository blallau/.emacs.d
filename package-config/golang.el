;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get golang.org/x/tools/cmd/oracle

(require 'go-mode)
(require 'go-projectile)

(eval-after-load 'go-mode
  '(progn
    ;; Set $GOPATH
    (go-projectile-set-gopath)
    ;; Set $PATH to $PATH:~/.emacs.d/gotools/bin
    (go-projectile-tools-add-path)))


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
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; oracle
(defvar oracle-file (concat go-projectile-tools-path "/src/"
                            (cdr (assq 'oracle go-projectile-tools))
                            "/oracle.el"))
(if (file-exists-p oracle-file)
    (load-file oracle-file))

;; (add-hook 'go-mode-hook 'go-oracle-mode)

;; autocomplete
;;
(require 'auto-complete-config)
(require 'go-autocomplete)
(ac-config-default)
