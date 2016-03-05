(require 'org)

;; Hook to update all blocks before saving
(add-hook 'org-mode-hook
          (lambda() (add-hook 'before-save-hook
                              'org-update-all-dblocks t t)))

(setq org-startup-indented t)
(setq org-hide-leading-stars nil)

;; Tags
(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h)
                      (:endgroup . nil)
                      ("laptop" . ?l) ("pc" . ?p)))

;; Information to record when a task moves to the DONE state
(setq org-log-done 'time
      org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d)")
                          (sequence "BUG(b)" "REVIEW(r)" "|" "FIXED(f)"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))
                               ("REVIEW" . (:foreground "blue" :weight bold))))

;; Org directory config
(setq org-directory "~/work/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files (list (concat org-directory "/TODO-work.org")
                             (concat org-directory "/TODO-home.org")))

;; Priorities
;;
;; set priority range from A to C with default A
(setq org-highest-priority ?A
      org-lowest-priority ?C
      org-default-priority ?A)
;; set colours
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;; Todo
;;
(setq org-enforce-todo-dependencies t)

;; Capture
;;
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline (concat org-directory "/gtd.org") "Tasks")
         "* TODO %?\n %i\n %a")
        ("l" "Link" entry
         (file+headline (concat org-directory "/links.org") "Links to Read")
         "* %?\n %i")))

;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

;; Footnode
;;
(setq org-footnote-section nil
      org-footnote-auto-adjust t)

;; Agenda
;;
;; open agenda in current window
;;(setq org-agenda-window-setup (quote current-window))

;; Links
;;
;; follow the link at point with <RET>
(setq org-return-follows-link t)

;; With the following settings, you could link to a specific bug with
;; `[[jira:1234]]'
;; `[[lp:14134]]'
;; `[[review:14134/1]]'
(setq org-link-abbrev-alist
      '(("jira" . "http://pegjira.pegasus.theresis.org/browse/")
        ("lp" . "https://bugs.launchpad.net/bugs/")
        ("review" . "https://review.openstack.org/#/c/")))

;; Source code block
;;
;; Don't ask before every code block evaluation
(setq org-confirm-babel-evaluate nil)

;; syntax highlight code blocks
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)

;; Ditaa
(setq org-ditaa-jar-path "~/ditaa0_9.jar")
;; plantUML
(setq org-plantuml-jar-path "~/plantuml.jar")

(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

;; permit inline image display in the Emacs buffer.
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

;; Babel
;;
;; Some initial languages we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (org . t)
   (perl . t)
   (plantuml . t)
   (python . t)
   (ruby . t)
   (sh . t)
   (sql . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))

;;bind to keys
(define-key org-mode-map (kbd "C-<") 'org-begin-template)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
