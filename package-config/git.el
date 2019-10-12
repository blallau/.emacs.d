(use-package magit
  :config
  ;; Configure git-commit
  (setq git-commit-summary-max-length 50
        fill-column 72)

  ;; Disable mode-line lighter of the Magit-Blame mode
  (setq magit-blame-mode-lighter "")

  ;; Configure Magit
  ;; Disable the `highlight` face that Magit uses to highlight diffs. It's unreadable with my color scheme.
  ;; An unreadable highlight face is a common issue on the Magit tracker.
  (defun disable-magit-highlight-in-buffer () (face-remap-add-relative 'magit-item-highlight '()))
  (add-hook 'magit-status-mode-hook #'disable-magit-highlight-in-buffer)
  (add-hook 'magit-commit-mode-hook #'disable-magit-highlight-in-buffer)
  (add-hook 'magit-diff-mode-hook #'disable-magit-highlight-in-buffer)

  (setq magit-log-arguments (quote ("--graph"
                                    "--color"
                                    "--decorate"
                                    ;; "++header"
                                    "--no-merges"
                                    "-n256"))
        magit-revert-buffers t
        magit-save-repository-buffers t)

  ;; follow symlinks without querying user
  (setq vc-follow-symlinks t)

  :bind
  (("<f5>" . magit-blame)
   ("<f6>" . magit-status)
   ("C-<f6>" . magit-checkout)
   ("S-<f6>" . magit-log-buffer-file)))


;; Magit interfaces for GitHub
(use-package magithub
  :after magit)


;; Gutter Icons indicating inserted, modified or deleted lines
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


;; Get the GitHub/Bitbucket/GitLab URL for a buffer location
(use-package git-link
  :config
  (setq git-link-open-in-browser t
        ;; use the latest commit's hash in the link instead of the branch name
        git-link-use-commit t))
;;(global-set-key (kbd "C-c g l") 'git-link)


;; Walk through git revisions of a file
(use-package git-timemachine
  :defer t
  :config
  (setq git-timemachine-show-minibuffer-details t)
  :bind (("<C-S-f6>" . git-timemachine)))


;; (use-package git-messenger
;;   :defer t
;;   :bind (("<f5>" . git-messenger:popup-message)
;;          :map git-messenger-map
;;          ("m" . git-messenger:copy-message)))
