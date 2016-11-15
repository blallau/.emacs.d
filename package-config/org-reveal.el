(use-package ox-reveal
  :config
  ;; directory within which js/reveal.js is."
  ;; (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
  (setq org-reveal-root (substitute-in-file-name "$HOME/work/pres/reveal.js/")
        org-reveal-title-slide 'nil))
