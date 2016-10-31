(use-package magit
  :config
  ;; Configure git-commit
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72)

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
  (("C-<f5>" . magit-blame)
   ("<f6>" . magit-status)
   ("C-<f6>" . magit-checkout)
   ("S-<f6>" . magit-log-buffer-file)))
