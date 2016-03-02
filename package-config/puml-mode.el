(require 'puml-mode)

;; Enable puml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
(add-to-list 'auto-mode-alist '("\\.uml\\'" . puml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))

(setq puml-output-type "svg")
(setq puml-plantuml-jar-path "~/plantuml.jar")
