;; https://writequit.org/org/settings.html#sec-1-34

(use-package helm
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-h b" . helm-descbinds)
   ("C-x M-o" . helm-occur)
   ("M-y" . helm-show-kill-ring)
   ("C-h a" . helm-apropos)
   ("M-x" . helm-M-x)
   ("C-x C-b" . helm-buffers-list)
   ("C-x b" . helm-mini))
  :init (progn
          ;;(helm-autoresize-mode 1)
          (helm-mode 1))
  :diminish ""
  :config
  (progn
    (use-package helm-config)
    ;; (use-package helm-files
    ;;   :config
    ;;   (progn
    ;;     (setq helm-ff-file-compressed-list '("gz" "bz2" "zip" "7z" "tgz"))))
    ;; (use-package helm-grep
    ;;   :config
    ;;   (progn
    ;;     (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    ;;     (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    ;;     (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)))
    ;; (use-package helm-man)
    ;; (use-package helm-misc)
    ;; (use-package helm-aliases)
    ;; (use-package helm-elisp)
    ;; (use-package helm-imenu)
    ;; (use-package helm-semantic)
    ;; (use-package helm-ring)
    ;; (use-package helm-bookmark
    ;;   :bind (("C-x M-b" . helm-bookmarks)))
    ;; (use-package helm-projectile
    ;;   :bind (("C-x f" . helm-projectile)
    ;;          ("C-c p f" . helm-projectile-find-file)
    ;;          ("C-c p s" . helm-projectile-switch-project)))
    (use-package helm-eshell
      :init (add-hook 'eshell-mode-hook
                      (lambda ()
                        (define-key eshell-mode-map (kbd "M-l")
                          'helm-eshell-history))))
    (use-package helm-descbinds
      :init (helm-descbinds-mode t))
    ;; (use-package helm-ag
    ;;   :bind ("C-M-s" . helm-ag-this-file))
    (setq
     ;; helm-idle-delay 0.01
          helm-exit-idle-delay 0.1
          helm-input-idle-delay 0.01
          helm-buffers-fuzzy-matching t
          ;; truncate long lines in helm completion
          helm-truncate-lines t
          ;; may be overridden if 'ggrep' is in path (see below)
          helm-grep-default-command
          "grep -a -d skip %e -n%cH -e %p %f"
          helm-grep-default-recurse-command
          "grep -a -d recurse %e -n%cH -e %p %f"
          ;; ;; do not display invisible candidates
          ;; helm-quick-update t
          ;; ;; be idle for this many seconds, before updating in delayed sources.
          ;; helm-idle-delay 0.01
          ;; be idle for this many seconds, before updating candidate buffer
          helm-input-idle-delay 0.01
          ;; open helm buffer in same window
          helm-split-window-default-side 'same
          ;; open helm buffer inside current window, don't occupy whole other window
          helm-split-window-in-side-p t
          ;; limit the number of displayed canidates
          helm-candidate-number-limit 200
          ;; don't use recentf stuff in helm-ff
          helm-ff-file-name-history-use-recentf nil
          ;; move to end or beginning of source when reaching top or bottom
          ;; of source
          helm-move-to-line-cycle-in-source t
          ;; don't displace the header line
          helm-display-header-line nil
          ;; fuzzy matching
          helm-buffers-fuzzy-matching t
          ;; helm-semantic-fuzzy-match t
          ;; helm-imenu-fuzzy-match t
          ;; helm-completion-in-region-fuzzy-match t
          )
    ;; helm-mini instead of recentf
    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

    (use-package helm-swoop
      :bind (("M-i" . helm-swoop)
             ("M-I" . helm-swoop-back-to-last-point)
             ("C-c M-i" . helm-multi-swoop))
      :config
      (progn
        ;; When doing isearch, hand the word over to helm-swoop
        (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
        ;; From helm-swoop to helm-multi-swoop-all
        (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
        ;; Save buffer when helm-multi-swoop-edit complete
        (setq helm-multi-swoop-edit-save t
              ;; If this value is t, split window inside the current window
              helm-swoop-split-with-multiple-windows nil
              ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
              helm-swoop-split-direction 'split-window-vertically
              ;; If nil, you can slightly boost invoke speed in exchange for text color
              helm-swoop-speed-or-color nil)))))
