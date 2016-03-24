(require 'org)

;; Hook to update all blocks before saving
(add-hook 'org-mode-hook
          (lambda() (add-hook 'before-save-hook
                              'org-update-all-dblocks t t)))

(setq org-startup-indented t)
(setq org-hide-leading-stars nil)

;;
;; Tags
;;
(setq org-group-tags t
      org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h)
                      (:endgroup . nil)))

;; Information to record when a task moves to the DONE state
(setq org-log-done 'time
      ;; Enable task switching shortcuts
      org-use-fast-todo-selection t
      org-todo-keywords '((sequence "☛ TODO(t)" "➱ IN-PROGRESS(i)" "⚑ WAITING(w)" "|" "✔ DONE(d)" "✘ CANCELED(c)")
                          (sequence "☠ BUG(b)" "➱ INPROGRESS(p)" "⚑ REVIEW(r)" "|" "✔ FIXED(f)" "✘ NOT FIXED(n)"))
      org-todo-keyword-faces '(("➱ IN-PROGRESS" . (:background "DeepSkyBlue" :foreground "blue" :weight bold
                                                               :box (:line-width 1 :color nil :style none)))
                               ("➱ INPROGRESS" . (:background "DeepSkyBlue" :foreground "blue" :weight bold
                                                              :box (:line-width 1 :color nil :style none)))
                               ("⚑ WAITING" . (:background "DeepSkyBlue" :foreground "blue" :weight bold
                                                           :box (:line-width 1 :color nil :style none)))
                               ("⚑ REVIEW" . (:background "DeepSkyBlue" :foreground "blue" :weight bold
                                                          :box (:line-width 1 :color nil :style none)))
                               ("✘ CANCELED" . (:background "#d7ff5f" :foreground "#008700" :weight bold
                                                            :strike-through t
                                                            :box (:line-width 1 :color nil :style none)))
                               ("✘ NOT FIXED" . (:background "#d7ff5f" :foreground "#008700" :weight bold
                                                             :strike-through t
                                                             :box (:line-width 1 :color nil :style none)))))

;; Org directory config
(setq org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org")
      ;; all files to include in order to compose agenda
      org-agenda-files (list (concat org-directory "/gtd.org")
                             (concat org-directory "/notes.org")))

;;
;; Priorities
;;
;; set priority range from A to C with default A
(setq org-highest-priority ?A
      org-lowest-priority ?C
      org-default-priority ?A
      ;; set colours
      org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;; %25ITEM %TODO %3PRIORITY %TAGS
(setq org-columns-default-format "%25ITEM(Task) %TODO(State) %3PRIORITY %TAGS(Tags)")

;;
;; Todo
;;

;; forces to mark all child tasks as “DONE” before you can mark the parent as “DONE”
(setq org-enforce-todo-dependencies t)

;;
;; Capture
;;
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "gtd.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
        ("l" "Link" entry
         (file+headline "links.org" "Links to Read")
         "* %?\n %i")))

;; leave a blank line at the end of the content of each task entry
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))

;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

;;
;; Footnode
;;
(setq org-footnote-section nil
      org-footnote-auto-adjust t)

;;
;; Agenda
;;
;; visual aid telling me which line I’m in
(add-hook 'org-agenda-finalize-hook (lambda () (hl-line-mode)))

;; When perform a text search (the “s” selection from the org-agenda pop-up)
;; include the archives for all of the files in Org’s agenda files list
(setq org-agenda-text-search-extra-files '(agenda-archives)
      ;;tag searches ignore tasks with scheduled and deadline dates
      org-agenda-tags-todo-honor-ignore-options t

      ;;compact the block agenda view
      org-agenda-compact-blocks t

      ;;open agenda in current window
      org-agenda-window-setup (quote current-window)
      ;;warn me of any deadlines in next 3 days
      org-deadline-warning-days 3
      ;;show me tasks scheduled or due in next fortnight (14 days)
      org-agenda-span (quote fortnight)

      org-agenda-skip-additional-timestamps-same-entry t

      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t

      ;;don't show tasks as scheduled if they are already shown as a deadline
      org-agenda-skip-scheduled-if-deadline-is-shown t
      ;;skip deadline prewarning when entry is also scheduled
      org-agenda-skip-deadline-prewarning-if-scheduled nil
      ;;don't show tasks that are scheduled or have deadlines in the
      ;;normal todo list
      org-agenda-todo-ignore-deadlines (quote all)
      org-agenda-todo-ignore-scheduled (quote all)
      ;;sort tasks in order of when they are due and then by priority
      org-agenda-sorting-strategy (quote
                                   ((agenda deadline-up priority-down)
                                    (todo priority-down category-keep)
                                    (tags priority-down category-keep)
                                    (search category-keep))))

(setq org-agenda-category-icon-alist
      '((".[Ee]macs.*" "/usr/share/icons/hicolor/16x16/apps/emacs.png" nil nil :ascent center)
        ("[Oo]rg" "~/.emacs.d/icons/org/org.png" nil nil :ascent center)
        ("[Tr]rip" "~/.emacs.d/icons/org/trip.png" nil nil :ascent center)
        ("[Aa]nniv" "~/.emacs.d/icons/org/anniversary.png" nil nil :ascent center)
        ("\\([Hh]olidays\\|[Vv]acation\\)" "~/.emacs.d/icons/org/holidays.png" nil nil :ascent center)
        (".*cinder" "~/.emacs.d/icons/org/openstack.png" nil nil :ascent center)
        ("designate" "~/.emacs.d/icons/org/openstack.png" nil nil :ascent center)
        ("glance" "~/.emacs.d/icons/org/openstack.png" nil nil :ascent center)
        ("heat" "~/.emacs.d/icons/org/openstack.png" nil nil :ascent center)
        ("keystone" "~/.emacs.d/icons/org/openstack.png" nil nil :ascent center)
        (".*neutron" "~/.emacs.d/icons/org/openstack.png" nil nil :ascent center)
        (".*nova" "~/.emacs.d/icons/org/openstack.png" nil nil :ascent center)
        ("octavia" "~/.emacs.d/icons/org/openstack.png" nil nil :ascent center)
        (".*" '(space . (:width (16))))))

;; How to identify stuck projects
(setq org-tags-exclude-from-inheritance '("prj")
      org-stuck-projects '("+prj/-✔ DONE-✘ CANCELED-✔ FIXED-✘ NOT FIXED"
                           ("☛ TODO" "☠ BUG") nil ""))

;;
;; Table
;;
;; only work with the internal format (like @3$2 or $4)
(setq org-table-use-standard-references nil)

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

;; turn on auto-fill
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;
;; Babel
;;
;; Some initial languages we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (go . t)
   (restclient . t)
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
