;; IVY-MODE
(require 'ivy-mode)
(setq ivy-mode t)

(setq magit-completing-read-function 'ivy-completing-read)
(setq projectile-completion-system 'ivy)

;; a space inserted each time you press TAB
(setq ivy-tab-space t)

;;don't display ../ and ./ while completing file names
(setq ivy-extra-directories nil)
;;confirm when you create a new file or buffer
(setq confirm-nonexistent-file-or-buffer t)

;;recently visited files as well as all your bookmarks
;;are appended to the end of the buffer list
(setq ivy-use-virtual-buffers t)

;; see not only the number of matched candidates,
;; but also the index of the current one
(setq ivy-count-format "(%d/%d) ")

(use-package recentf
  :config
  (setq recentf-exclude
        '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
          ".*png$"))
  (setq recentf-max-saved-items 60))

;; COUNSEL
(require 'counsel)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> s") 'counsel-find-symbol)
(global-set-key (kbd "<f4> g") 'counsel-git-grep)
