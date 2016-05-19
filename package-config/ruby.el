(require 'ruby-mode)

(defun ome-ruby-mode-setup ()
  ;; Ah, this huge auto-mode-alist list comes from emacs prelude
  (add-to-list 'auto-mode-alist '("\\.cap\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemrc\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.irbrc\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Brewfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("[rR]akefile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode)))
(ome-ruby-mode-setup)

;; ROBE: code assistance tool that uses a Ruby REPL subprocess
;; with your application or gem code loaded, to provide information
;; about loaded classes and modules, and where each method is defined.
(add-hook 'ruby-mode-hook 'robe-mode)
;; auto-complete
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; M-x start-robe

(when (require 'smartparens nil 'noerror)
  (require 'smartparens-ruby))
