;; IVY
;;-----
(use-package ivy :demand
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq
   ;; Add recent files and bookmarks to the ivy-switch-buffer
   ivy-use-virtual-buffers 'recentf
   ;; Displays the current and total number in the collection in prompt
   ivy-count-format "%d/%d"
   ;; number of result lines to display
   ivy-height 12
   ;; no regexp by default
   ivy-initial-inputs-alist nil
   ;; How to enter an input that matches one of the candidates instead
   ;; of this candidate C-M-j
   ;; OR
   ;; make the prompt line selectable
   ivy-use-selectable-prompt t
   )

  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-plus)))
  )

;; ivy-pass - Interface for pass
;; https://github.com/ecraven/ivy-pass
;;
;; (use-package ivy-pass
;;   :after ivy
;;   :commands ivy-pass)

;; ivy-rich - More friendly interface for ivy
;; https://github.com/Yevgnen/ivy-rich
;;
(use-package ivy-rich
  :after ivy
  :config
  ;; (defun ivy-rich-switch-buffer-icon (candidate)
  ;;   (with-current-buffer
  ;;  	(get-buffer candidate)
  ;;     (let ((icon (all-the-icons-icon-for-mode major-mode)))
  ;;   (if (symbolp icon)
  ;;       (all-the-icons-icon-for-mode 'fundamental-mode)
  ;;     icon))))
  (setq ivy-rich-display-transformers-list
      '(ivy-switch-buffer
        (:columns
         (
          ;; (ivy-rich-switch-buffer-icon :width 2)
          (ivy-rich-candidate (:width 30))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))))
  (ivy-rich-mode 1))

;; ivy-xref - select from xref candidates with ivy
;; https://github.com/alexmurray/ivy-xref
;;
(use-package ivy-xref
  :ensure t
  :init (if (< emacs-major-version 27)
            (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))

;; COUNSEL
;;---------
(use-package counsel :ensure t
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-s"     . swiper)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)   ; search for recently edited
   ("C-c c"   . counsel-compile)
   ("C-c g"   . counsel-git)       ; search for files in git repo
   ("C-c j"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . counsel-ag)        ; Use ag for regexp
   ("C-x l"   . counsel-locate)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
;   ("<f2> i"  . counsel-info-lookup-symbol)
;   ("<f2> u"  . counsel-unicode-char)
   ("C-c C-r" . ivy-resume)        ; Resume last Ivy-based completion
   ("C-c o"   . counsel-find-file-extern)
   ("C-S-s"   . swiper-thing-at-point))
  :config
  (setq counsel-find-file-at-point t)
  (setq counsel-locate-cmd 'counsel-locate-cmd-default)
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

(use-package counsel-projectile
  :ensure t
  :bind (
         ("<f4>" . counsel-projectile-git-grep)
         ("<C-f4>" . counsel-projectile-ag)
         ("<S-f4>" . counsel-projectile-rg))
  :config
  (counsel-projectile-mode))
