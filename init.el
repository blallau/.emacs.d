;; Emacs Config

(eval-and-compile
  (require 'cask "~/.cask/cask.el")
  (cask-initialize)
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  )

;; Keeps ~Cask~ file in sync with packages
;; install/uninstall via ~M-x list-packages~
(require 'pallet)
(pallet-mode t)

;; packages installed via package.el (Cask) MUST be initalized before tweaking them
(package-initialize)

(require 'benchmark-init)

;; Recompile .emacs.d/lisp & .emacs.d/lisp-config
(byte-recompile-directory "~/.emacs.d/lisp" 0)
(byte-recompile-directory "~/.emacs.d/package-config" 0)

;; load proxy conf
(load-library "proxy")

;; customize init
(load-library "customize-init")

;; customize
(load-library "customize-appearance")
(load-library "customize-edition")
(load-library "customize-navigation")
(load-library "customize-refresh")
(load-library "customize-shortcuts")

;; Load all files with *.elc in lisp-config
(mapc 'load-library (directory-files (expand-file-name "package-config" user-emacs-directory) t ".elc$"))
