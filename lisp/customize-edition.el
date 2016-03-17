;;replace selection when typing
(delete-selection-mode 1)

;; Tranpose two arguments of a function
(defun my/calculate-stops ()
  (save-excursion
    (let ((start
           (condition-case e
               (while t (backward-sexp))
             (error (point))))
          stops)
      (push start stops)
      (condition-case e
          (while t
            (forward-sexp)
            (when (looking-at "\\s-*,")
              (push (point) stops)))
        (error (push (point) stops)))
      (nreverse stops))))

(defun my/transpose-args ()
  (interactive)
  (when (looking-at "\\s-") (backward-sexp))
  (cl-loop with p = (point)
           with previous = nil
           for stop on (my/calculate-stops)
           for i upfrom 0
           when (<= p (car stop)) do
           (when previous
             (let* ((end (cadr stop))
                    (whole (buffer-substring previous end))
                    middle last)
               (delete-region previous end)
               (goto-char previous)
               (setf middle (if (> i 1) (- (car stop) previous)
                              (string-match "[^, \\t]" whole
                                            (- (car stop) previous)))
                     last (if (> i 1) (substring whole 0 middle)
                            (concat (substring whole (- (car stop) previous) middle)
                                    (substring whole 0 (- (car stop) previous)))))
               (insert (substring whole middle) last)))
           (cl-return)
           end do (setf previous (car stop))))

(global-set-key (kbd "<C-f11>") 'my/transpose-args)

(require 're-builder)
(setq reb-re-syntax 'string)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; to setup spaces instead of tab
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode nil)

;; re-enable narrow-to-region by default
(put 'narrow-to-region 'disabled nil)
