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

(setq logview-show-ellipses nil)
(add-hook 'logview-mode-hook
          (lambda ()
            ;; (local-set-key (kbd "<f2>") #'pop-tag-mark)
            (local-set-key (kbd "<f3>") #'jump-to-file-and-line)))
