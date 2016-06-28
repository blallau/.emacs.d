(use-package git-gutter
  :defer t
  :config
  (progn
    (setq git-gutter:handled-backends '(git)
          git-gutter:hide-gutter t)
    (global-git-gutter-mode +1))
  :bind
  (
   ;; Jump to next/previous hunk
   ("C-x p" . git-gutter:previous-hunk)
   ("C-x n" . git-gutter:next-hunk)
   ;; Stage current hunk
   ("C-x v s" . git-gutter:stage-hunk)
   ;; Revert current hunk
   ("C-x v r" . git-gutter:revert-hunk)))
