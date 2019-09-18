;; (use-package hideshow
;;   :bind (("C-c TAB" . hs-toggle-hiding)
;;          ("C-\\" . hs-toggle-hiding)
;;          ("M-+" . hs-show-all))
;;   :init (add-hook #'prog-mode-hook #'hs-minor-mode)
;;   :diminish hs-minor-mode
;;   :config
;;   (setq hs-special-modes-alist
;;         (mapcar 'purecopy
;;                 '((c-mode "{" "}" "/[*/]" nil nil)
;;                   (c++-mode "{" "}" "/[*/]" nil nil)
;;                   (java-mode "{" "}" "/[*/]" nil nil)
;;                   (js-mode "{" "}" "/[*/]" nil)
;;                   (json-mode "{" "}" "/[*/]" nil)
;;                   (javascript-mode  "{" "}" "/[*/]" nil)))))
