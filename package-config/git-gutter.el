(use-package git-gutter
  :init
  (setq git-gutter:handled-backends '(git)
        git-gutter:hide-gutter t)
  (global-git-gutter-mode +1)
  :config
  (defun git-gutter-reset-to-head-parent()
    (interactive)
    (let (parent (filename (buffer-file-name)))
      (if (eq git-gutter:vcs-type 'svn)
          (setq parent "PREV")
        (setq parent (if filename (concat (shell-command-to-string (concat "git --no-pager log --oneline -n1 --pretty='format:%H' " filename)) "^") "HEAD^")))
      (git-gutter:set-start-revision parent)
      (message "git-gutter:set-start-revision HEAD^")))

  (defun git-gutter-reset-to-default ()
    (interactive)
    (git-gutter:set-start-revision nil)
    (message "git-gutter reset"))
  :bind
  (
   ;; ("C-x p" . git-gutter:previous-hunk)
   ;; ("C-x n" . git-gutter:next-hunk)
   ;; Stage current hunk
   ("C-x v s" . git-gutter:stage-hunk)
   ;; Revert current hunk
   ("C-x v r" . git-gutter:revert-hunk)
   ;;
   ("<C-f5>" . git-gutter-reset-to-head-parent)
   ("<C-S-f5>" . git-gutter-reset-to-default)
   ;; Jump to next/previous hunk
   ("<C-f2>" . git-gutter:previous-hunk)
   ("<C-f3>" . git-gutter:next-hunk)))
