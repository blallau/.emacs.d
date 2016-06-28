(use-package git-messenger
  :defer t
  :bind (("<f5>" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message)))
