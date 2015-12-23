(require 'helm)

(helm-mode 1)

(setq helm-split-window-default-side 'same)

;(global-set-key (kbd "M-o") 'sr-open-file)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(require 'helm-descbinds)
(helm-descbinds-mode)
