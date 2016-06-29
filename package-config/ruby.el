(use-package ruby-mode
  :defer t
  :mode
  (("\\.cap\\'" . ruby-mode)
   ("\\.erb\\'" . ruby-mode)
   ("\\.gemspec\\'" . ruby-mode)
   ("\\.gemrc\\'" . ruby-mode)
   ("\\.irbrc\\'" . ruby-mode)
   ("\\.jbuilder\\'" . ruby-mode)
   ("\\.rake\\'" . ruby-mode)
   ("\\.rb\\'" . ruby-mode)
   ("\\.ru\\'" . ruby-mode)
   ("\\.thor\\'" . ruby-mode)
   ("Brewfile\\'" . ruby-mode)
   ("Capfile\\'" . ruby-mode)
   ("Gemfile\\'" . ruby-mode)
   ("Guardfile\\'" . ruby-mode)
   ("[rR]akefile\\'" . ruby-mode)
   ("Thorfile\\'" . ruby-mode)
   ("Vagrantfile\\'" . ruby-mode))

  :bind
  (("<f3>" . robe-jump)
   ("<f2>" . pop-tag-mark)
   ("<f1>" . robe-doc))

  :config
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)

  (use-package flymake-ruby
    :config
    (add-hook 'ruby-mode-hook #'flymake-ruby-load))

  ;; ROBE: code assistance tool that uses a Ruby REPL subprocess
  ;; with your application or gem code loaded, to provide information
  ;; about loaded classes and modules, and where each method is defined.
  (add-hook 'ruby-mode-hook #'robe-mode)
  ;; auto-complete
  (add-hook 'robe-mode-hook #'ac-robe-setup)
  ;; M-x start-robe

  (when (require 'smartparens nil 'noerror)
    (require 'smartparens-ruby)))
