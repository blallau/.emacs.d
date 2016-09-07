(use-package lua-mode
  :defer t
  :mode ("\\.lua$'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :bind( ("<f1>" . lua-search-documentation)))
