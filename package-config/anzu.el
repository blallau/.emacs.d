(use-package anzu
  :defer t
  :bind
  (([remap query-replace] . anzu-replace-at-cursor-thing)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)))
