;; weather from wttr.in
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Versailles")))

;; function to open wttrin with first city on list
(defun my-wttrin ()
    "Open `wttrin' without prompting, using first city in `wttrin-default-cities'"
    (interactive)
    ;; save window arrangement to register
    (window-configuration-to-register :pre-wttrin)
    (delete-other-windows)
    ;; save frame setup
    (save-frame-config)
    (set-frame-width (selected-frame) 130)
    (set-frame-height (selected-frame) 48)
    ;; call wttrin
    (wttrin-query (car wttrin-default-cities)))
