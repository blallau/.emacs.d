;; Emacs Config
;; Add lisp directory to path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(package-initialize)
(require 'cask "~/.cask/cask.el")

(load-library "proxy")
(load-library "pre-cask")
(load-library "customize")

;; Init All Plugin with Cask & Pallet
(cask-initialize)

;; Load all files with *.elc?
(mapc 'load-library (directory-files (expand-file-name "lisp-config" user-emacs-directory) t ".elc?$"))
