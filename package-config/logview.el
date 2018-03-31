;; (use-package logview
;;   :defer t
;;   :mode ("\\.log\\'" . logview-mode)
;;   :config
;;   (defun jump-to-file-and-line ()
;;     "Reads a line in the form .* FILENAME:LINE, opens that file in
;; another window and jumps to the line."
;;     (interactive)
;;     (save-excursion
;;       (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
;;         (string-match ".* \\(.*\\):\\([0-9]+\\)" line)
;;         (let ((file (match-string 1 line))
;;               (lnum (match-string 2 line)))
;;           (if (and file (file-exists-p file))
;;               (progn
;;                 (find-file-other-window file)
;;                 (and lnum (goto-line (string-to-number lnum))))
;;             (user-error "File '%s' not found" file))))))
;;   (setq logview-show-ellipses nil)
;;   :bind
;;   (("<f2>" . pop-tag-mark))
;;   (("<f3>" . jump-to-file-and-line)))
(setq logview-show-ellipses t)

(setq logview-additional-submodes
      (quote
       (("Openstack"
         (format . "TIMESTAMP THREAD LEVEL NAME ")
         (levels . "OSLO_LOG")
         (timestamp)
         (aliases)))))

(setq logview-additional-level-mappings
      (quote
       (
        ("SLF4J"
         (error "ERROR")
         (warning "WARN")
         (information "INFO")
         (debug "DEBUG")
         (trace "TRACE")
         (aliases "Log4j" "Log4j2" "Logback"))
        ("OSLO_LOG"
         (error "ERROR")
         (warning "WARNING")
         (information "INFO")
         (debug "DEBUG")
         (trace "TRACE")
         (aliases "OSLOLOG"))
        ("JUL"
         (error "SEVERE")
         (warning "WARNING")
         (information "INFO")
         (debug "CONFIG" "FINE")
         (trace "FINER" "FINEST")))))

(defun jump-to-file-and-line ()
  "Reads a line in the form .* FILENAME:LINE, opens that file in
another window and jumps to the line."
  (interactive)
  (logview--assert 'level)
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (string-match ".* \\(.*\\):\\([0-9]+\\)" line)
    (let ((file (match-string 1 line))
          (lnum (match-string 2 line)))
      (if (and file (file-exists-p file))
          (progn
            (find-file-other-window file)
            (and lnum (goto-line (string-to-number lnum))))
        (user-error "File '%s' not found" file)))))

(add-hook 'logview-mode-hook
          (lambda ()
            ;; (local-set-key (kbd "<f2>") #'pop-tag-mark)
            (local-set-key (kbd "<f3>") #'jump-to-file-and-line)))
