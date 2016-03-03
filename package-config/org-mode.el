(require 'org)

(global-set-key (kbd "C-c l") 'org-store-link)
;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)
;;(define-key global-map "\C-cl" 'org-store-link)
;;(define-key global-map "\C-ca" 'org-agenda)

(setq org-startup-indented t)

;; Information to record when a task moves to the DONE state
(setq org-log-done t)

;; Org directory config
(setq org-directory "~/work/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list (concat org-directory "/TODO-work.org")
                             (concat org-directory "/TODO-home.org")))

;; set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;; set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

(setq org-enforce-todo-dependencies t)

;; ;; capture todo items using C-c c t
;; (define-key global-map (kbd "C-c c") 'org-capture)
;; (setq org-capture-templates
;;       '(("t" "todo" entry (file+headline "/work/TODO.org" "Tasks")
;;          "* TODO [#A] %?")))

;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

;; Src
;;
;; syntax highlight code blocks
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)

;; Footnode
;;
(setq org-footnote-section nil)
(setq org-footnote-auto-adjust t)

;; Agenda
;;
;; open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; LINKs
;;
;; follow the link at point with <RET>
(setq org-return-follows-link t)

(setq org-link-abbrev-alist
      '(("jira" . "http://pegjira.pegasus.theresis.org/browse/")
        ("launchpad" . "https://bugs.launchpad.net/bugs/")))

;; Babel
;;
;; Some initial languages we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa . t)
   (emacs-lisp . t)
   (perl . t)
   (python . t)
   (ruby . t)
   (sh . t)))

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

;;bind to key
(define-key org-mode-map (kbd "C-<") 'org-begin-template)
