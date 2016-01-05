(eval-when-compile
  (require 'git-gutter))

(custom-set-variables
 '(git-gutter:handled-backends '(git)))
(global-git-gutter-mode +1)

(custom-set-variables
 '(git-gutter:hide-gutter t))
;(add-hook 'python-mode-hook 'git-gutter-mode)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
