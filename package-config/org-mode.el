(require 'org)

;; ODT export
(use-package ox-odt)

;; AsciiDoc Export
(use-package ox-asciidoc)

;; RST Export
(use-package ox-rst)

;; Reveal Export
(use-package ox-reveal
  :config
  ;; directory within which js/reveal.js is."
  ;; (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
  (setq org-reveal-root (substitute-in-file-name "$HOME/work/pres/reveal.js/")
        org-reveal-title-slide 'nil))

;; activate PDF view on page
;; [[pdfview:file.pdf::52]]
;; install pdf-tools-install: M-x pdf-tools-install
;;(require 'org-pdfview)
;;(add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
;;(add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))

;;(pdf-tools-install)
;; turn off cua so copy works
;;(add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))

;; more fine-grained zooming
;; zooming with + and - than the default 25%, so I’ll set it to 10%
;;(setq pdf-view-resize-factor 1.1)

;; used annotation tools
;; (adding a highlight, adding a text note and deleting an annotation)
;;(define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
;;(define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
;;(define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)

;; (use-package pdf-tools
;;  :config
;;  ;; open pdfs scaled to fit page
;;  (setq-default pdf-view-display-size 'fit-page)
;;  ;; automatically annotate highlights
;;  (setq pdf-annot-activate-created-annotations t)
;;  ;; use normal isearch
;;  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;;  ;; turn off cua so copy works
;;  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
;;  ;; more fine-grained zooming
;;  (setq pdf-view-resize-factor 1.1)
;;  ;; keyboard shortcuts

;; YouTube
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

;; Org directory config
(setq org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org")
      ;; all files to include in order to compose agenda
      org-agenda-files (list (concat org-directory "/gtd.org")
                             (concat org-directory "/notes.org")))

;;
;; Hooks
;;
;; to update all blocks before saving
(add-hook 'org-mode-hook
          (lambda() (add-hook 'before-save-hook
                              'org-update-all-dblocks t t)))
(add-hook 'org-mode-hook #'org-display-inline-images)
(add-hook 'org-mode-hook #'turn-on-auto-fill)

;;
;; Appearance
;;
(setq org-startup-indented t
      ;; hide the multiple asterisks at the sub-section level
      org-hide-leading-stars nil
      ;; render text with delimiters [*/_]
      org-hide-emphasis-markers t
      ;; leave a blank line at the end of the content of each task entry
      org-blank-before-new-entry (quote ((heading) (plain-list-item)))
      ;; set maximum indentation for description lists
      org-list-description-max-indent 5
      ;; %25ITEM %TODO %3PRIORITY %TAGS
      org-columns-default-format "%25ITEM(Task) %TODO(State) %3PRIORITY %TAGS(Tags)"
      ;; prevent demoting heading also shifting text inside sections
      org-adapt-indentation nil

      ;;
      ;; Table
      ;;
      ;; only work with the internal format (like @3$2 or $4)
      org-table-use-standard-references nil

      ;;
      ;; Links
      ;;
      ;; With the following settings, you could link to a specific bug with
      ;; `[[jira:1234]]'
      ;; `[[lp:14134]]'
      ;; `[[review:14134/1]]'
      org-link-abbrev-alist
      '(("jira" . "http://pegjira.pegasus.theresis.org/browse/")
        ("lp" . "https://bugs.launchpad.net/bugs/")
        ("review" . "https://review.openstack.org/#/c/"))
      ;; follow the link at point with <RET>
      org-return-follows-link t
      ;; don't want links to be explicit
      org-descriptive-links t

      ;;
      ;; Footnode
      ;;
      org-footnote-section nil
      org-footnote-auto-adjust t)


;; Information to record when a task moves to the DONE state
(setq org-log-done 'time

      ;; forces to mark all child tasks as “DONE” before you can mark the parent as “DONE”
      org-enforce-todo-dependencies t

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
;;
;; Tags
;;
(setq org-group-tags t
      org-tags-exclude-from-inheritance '("prj")
      org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h)
                      (:endgroup . nil)))
;;
;; Priorities
;;
;; set priority range from A to C with default A
(setq org-highest-priority ?A
      org-lowest-priority ?C
      org-default-priority ?A
      ;; set colours
      org-priority-faces '((?A . (:foreground "red" :weight bold))
                           (?B . (:foreground "DarkOrange" :weight bold))
                           (?C . (:foreground "yellow" :weight bold))))
;;
;; Capture
;;
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "gtd.org" "Tasks")
         "* ☛ TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
        ("l" "Link" entry
         (file+headline "links.org" "Links to Read")
         "* %?\n %i")))
;;
;; Agenda
;;
;; visual aid telling me which line I’m in
(add-hook 'org-agenda-finalize-hook (lambda () (hl-line-mode)))

;; When perform a text search (the “s” selection from the org-agenda pop-up)
;; include the archives for all of the files in Org’s agenda files list
(setq org-agenda-text-search-extra-files '(agenda-archives)
      ;;blocked tasks hidden
      org-agenda-dim-blocked-tasks t

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
      org-agenda-skip-timestamp-if-deadline-is-shown t
      ;;skip deadline prewarning when entry is also scheduled
      org-agenda-skip-deadline-prewarning-if-scheduled nil

      org-agenda-skip-archived-trees nil

      org-agenda-category-icon-alist
      '((".[Ee]macs.*" "/usr/share/icons/hicolor/16x16/apps/emacs.png" nil nil :ascent center)
        ("[Oo]rg" "~/.emacs.d/icons/org/org.png" nil nil :ascent center)
        ("[Tr]rip" "~/.emacs.d/icons/org/trip.png" nil nil :ascent center)
        ("[Aa]nniv" "~/.emacs.d/icons/org/anniversary.png" nil nil :ascent center)
        ("\\([Hh]olidays\\|[Vv]acation\\)" "~/.emacs.d/icons/org/holidays.png" nil nil :ascent center)
        (".*" '(space . (:width (16)))))

      ;; How to identify stuck projects
      org-stuck-projects '("+prj/-✔ DONE-✘ CANCELED-✔ FIXED-✘ NOT FIXED"
                           ("☛ TODO" "☠ BUG") nil "")
      ;;
      ;; Agenda/Todo

      ;;don't show tasks that are scheduled or have deadlines in the
      ;;normal todo list
      org-agenda-todo-ignore-deadlines nil
      org-agenda-todo-ignore-scheduled nil

      ;;sort tasks in order of when they are due and then by priority
      org-agenda-sorting-strategy (quote
                                   ((agenda deadline-up priority-down)
                                    (todo priority-down category-keep)
                                    (tags priority-down category-keep)
                                    (search category-keep))))
;;
;; Source code block
;;
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
(add-hook 'org-babel-after-execute-hook #'shk-fix-inline-images)

;;
;; Babel
;;
;; Don't ask before every code block evaluation
(setq org-confirm-babel-evaluate nil)

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
;;   (sh . t)
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

(defun my-keybindings-org-hook ()
  (local-set-key (kbd "C-c a") 'org-agenda)
  (local-set-key (kbd "C-c c") 'org-capture)

  (local-set-key (kbd "<f3>") 'outline-next-visible-heading)
  (local-set-key (kbd "<f2>") 'outline-previous-visible-heading)
  (local-set-key (kbd "<f1>") 'rst-toc))

(add-hook 'org-mode-hook #'my-keybindings-org-hook)
