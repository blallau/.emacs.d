;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;; Turn on debugging, it will be turned off at the end.
(setq debug-on-error t
      debug-on-quit t)

(let ((minver "24"))
  (when (version<= emacs-version "24.5")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24.5")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

(eval-and-compile
  (require 'cask "~/.cask/cask.el")
  (cask-initialize)
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

;; Keeps ~Cask~ file in sync with packages
;; install/uninstall via ~M-x list-packages~
(require 'pallet)
(pallet-mode t)

;; packages installed via package.el (Cask) MUST be initalized before tweaking them
(package-initialize)

;; Load use-package, used for loading packages
(require 'use-package)

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

;; Start Emacs in server mode
(server-start)

;; Start emacs on org-agenda
;;(add-hook 'after-init-hook '(lambda ()
;;                              (org-agenda-list)
;;                              (get-buffer "*Org Agenda*")))

(provide 'init)

;; Finalizers

;; Turn off debugging, now that initialization has ended
(setq debug-on-error nil
      debug-on-quit nil)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
