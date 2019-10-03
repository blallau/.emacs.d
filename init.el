;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(defvar default-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

;; Turn on debugging, it will be turned off at the end.
(setq debug-on-error t
      debug-on-quit t)

;;----------------------------------------------------------------------------
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later with
;; `restore-garbage-collection-h'. Not resetting it will cause stuttering/freezes.

;; To speed up minibuffer commands (like helm and ivy), we defer garbage
;; collection while the minibuffer is active.
(defun defer-garbage-collection-h ()
  "TODO"
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  "TODO"
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold default-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

;; Not restoring these to their defaults will cause stuttering/freezes.
(add-hook 'emacs-startup-hook #'restore-garbage-collection-h)

;; When Emacs loses focus seems like a great time to do some garbage collection
;; all sneaky breeky like, so we can return a fresh(er) Emacs.
(add-hook 'focus-out-hook #'garbage-collect)
;;----------------------------------------------------------------------------

(eval-and-compile
  (require 'cask "~/.cask/cask.el")
  (cask-initialize)
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

;; Keeps ~Cask~ file in sync with packages
;; install/uninstall via ~M-x list-packages~
(require 'pallet)
(pallet-mode t)

;; packages installed via package.el (Cask)
;; MUST be initalized before tweaking them
(package-initialize t)
;; Load use-package, used for loading packages
(require 'use-package)

(setq package-check-signature nil)
(setq package-enable-at-startup nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  ;; update packages index
  (package-refresh-contents)
  ;; install fresh use-package
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(require 'benchmark-init)

;; Recompile .emacs.d/lisp & .emacs.d/lisp-config
(byte-recompile-directory "~/.emacs.d/lisp" 0)
(byte-recompile-directory "~/.emacs.d/package-config" 0)

;; load proxy conf
;; (load-library "proxy")

;; customize init
(load-library "customize-init")

;; customize
(load-library "customize-appearance")
(load-library "customize-edition")
(load-library "customize-functions")
(load-library "customize-navigation")
(load-library "customize-refresh")
(load-library "customize-shortcuts")

;; Load all files with *.elc in lisp-config
(mapc 'load-library (directory-files (expand-file-name "package-config" user-emacs-directory) t ".elc$"))

;; Start Emacs in server mode
(use-package server
  :config
  (unless (server-running-p) (server-start)))

;; Turn off debugging, now that initialization has ended
(setq debug-on-error nil
      debug-on-quit nil)

(provide 'init)
;; Finalizers
