;; Emacs Config

;; Add lisp directory to path
;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(eval-and-compile
  (require 'cask "~/.cask/cask.el")
  (cask-initialize)
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  )

;; packages installed via package.el (Cask) MUST be initalized before tweaking them
(package-initialize)

;; Recompile .emacs.d/lisp & .emacs.d/lisp-config
(byte-recompile-directory "~/.emacs.d/lisp" 0)
(byte-recompile-directory "~/.emacs.d/lisp-config" 0)

(load-library "proxy")
(load-library "pre-cask")
(load-library "customize")

;; Init All Plugin with Cask & Pallet
;(cask-initialize)

;; Load all files with *.elc in lisp-config
(mapc 'load-library (directory-files (expand-file-name "lisp-config" user-emacs-directory) t ".elc$"))
