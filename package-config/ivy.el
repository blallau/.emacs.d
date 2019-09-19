(use-package ivy :demand
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq
   ;; Add recent files and bookmarks to the ivy-switch-buffer
   ivy-use-virtual-buffers 'recentf
   ;; Displays the current and total number in the collection in prompt
   ivy-count-format "%d/%d"
   ;; number of result lines to display
   ivy-height 12
   ;; no regexp by default
   ivy-initial-inputs-alist nil
   )

  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-plus)))
  )
