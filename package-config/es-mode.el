;; Major mode for editing Elasticsearch queries
(use-package es-mode
  :ensure t
  :mode ("\\.es$" . es-mode)
  :config
  (setq es-always-pretty-print t
        es-warn-on-delete-query nil))
