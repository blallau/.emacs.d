;;; magit-review.el --- Magit plugin for Gerrit Code Review
;;
;; Copyright (C) 2015 Bertrand Lallau
;;
;; Author: Bertrand Lallau <bertrand.lallau@gmail.com>
;; URL: https://github.com/blallau/magit-review
;; Version: 0.0.1
;; Keywords: tools gerrit git
;; Package-Requires: ((emacs "25.0") (magit "2.4.0"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Magit plugin to make Gerrit code review easy-to-use from emacs and
;; without the need for a browser!
;;
;;; To Use:
;;
;; (require 'magit-review)
;;
;;
;; M-x `magit-status'
;; h R  <= magit-review uses the R prefix, see help
;;
;;; Workflow:
;;
;; 1) *check out branch => changes => (ma)git commit*
;; 2) R P  <= [ger*R*it *P*ush for review]
;; 3) R A  <= [ger*R*it *A*dd reviewer] (by email address)
;; 4) *wait for verification/code reviews* [approvals shown in status]
;; 5) R S  <= [ger*R*it *S*ubmit review]
;;
;;; Other Comments:
;; If your git remote for gerrit is not the default "origin", then
;; `magit-review-remote' should be adjusted accordingly (e.g. "gerrit")
;;
;; Recommended to auto add reviewers via git hooks (precommit), rather
;; than manually performing 'R A' for every review.
;;
;; `magit-review' will be enabled automatically on `magit-status'.
;;
;;; Code:

(require 'magit)

(if (locate-library "magit-popup")
    (require 'magit-popup))
(require 'json)

(eval-when-compile
  (require 'cl-lib))

;; Define a defvar-local macro for Emacs < 24.3
(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

(defvar-local magit-review-remote "gerrit"
  "Default remote name to use for gerrit (e.g. \"origin\", \"gerrit\")")

(defcustom magit-review-popup-prefix (kbd "R")
  "Key code to open magit-review popup"
  :group 'magit-review
  :type 'key-sequence)

(defun gerrit-command (cmd &rest args)
  (let ((gcmd (concat
	       " "
	       cmd
	       " "
	       (mapconcat 'identity args " ")
	       )))
;;    (message (format "Using cmd:%s" gcmd))
    gcmd))

(defun gerrit-query ()
  (gerrit-command "-v -l" ))

(defun gerrit-ssh-cmd (cmd &rest args)
  (apply #'call-process
	 "ssh" nil nil nil
	 (split-string (apply #'gerrit-command cmd args))))

(defun gerrit-review-submit (prj rev &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--submit"
		  (if msg msg "") rev))

(defun gerrit-code-review (prj rev score &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--code-review" score
		  (if msg msg "") rev))

(defun magit-review-get-remote-url ()
  (magit-git-string "ls-remote" "--get-url" magit-review-remote))

(defun magit-review-string-trunc (str maxlen)
  (if (> (length str) maxlen)
      (concat (substring str 0 maxlen)
	      "...")
    str))

(defun magit-review-create-branch-force (branch parent)
  "Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
Succeed even if branch already exist
\('git checkout -B BRANCH REVISION')."
  (cond ((run-hook-with-args-until-success
	  'magit-create-branch-hook branch parent))
	((and branch (not (string= branch "")))
	 (magit-save-repository-buffers)
	 (magit-run-git "checkout" "-B" branch parent))))

(defun magit-review-pretty-print-review (num subj branch topic &optional ins_num del_num)
  ;; window-width - two prevents long line arrow from being shown
  (let* ((wid (- (window-width) 2))
	 (numstr (propertize (format "%-8s" num) 'face 'magit-hash))
	 (nlen (length numstr))
	 (btmaxlen (/ wid 4))

	 (bt (propertize (magit-review-string-trunc (format "%s (%s)" branch topic)
						    btmaxlen)
			 'face 'magit-log-author))

	 (subjmaxlen (- wid nlen 6))

	 (subjstr (propertize (magit-review-string-trunc subj subjmaxlen)
			      'face
			      'magit-signature-good))
	 (btpadding (make-string
		     (max 0 (- wid (+ nlen 1 (length bt) (length subjstr))))
		     ? )))
    (format "%s%s%s%s\n" numstr subjstr btpadding bt)))

(defun json-review-list-to-clean ()
  (if (search-forward-regexp "^)\\]}'$" (point-max) t)
      t
    nil))

(defun magit-review-wash-review ()
  (progn
    ;; clean review list
    (let ((json-to-clean (save-excursion (json-review-list-to-clean))))
      (when json-to-clean
	(let ((beg (point)))
	  (search-forward-regexp "^\\[$")
	  (forward-line)
	  (delete-region beg (point-at-bol)))
	(search-forward-regexp "^\\]$")
	(delete-region (point-at-bol) (point-max))
	(goto-char (point-min))))
    (let* ((beg (point))
	   (jobj (json-read))
	   (end (point-at-eol))
	   (branch (cdr-safe (assoc 'branch jobj)))
	   (topic (cdr-safe (assoc 'topic jobj)))
	   (change_id (cdr-safe (assoc 'change_id jobj)))
	   (subj (cdr-safe (assoc 'subject jobj)))
	   (merg (cdr-safe (assoc 'mergeable jobj)))
	   (ins_numb (cdr-safe (assoc 'insertions jobj)))
	   (del_numb (cdr-safe (assoc 'deletions jobj)))
	   (num (cdr-safe (assoc '_number jobj))))
      (if (and beg end)
	  (delete-region beg end))
      (when (and num subj)
	(magit-insert-section (section subj)
	  (insert (propertize
		   (magit-review-pretty-print-review num subj branch topic)
		   'magit-review-jobj
		   jobj))
	  (add-text-properties beg (point) (list 'magit-review-jobj jobj)))
	t))))

(defun magit-review-wash-reviews (&rest args)
  (magit-wash-sequence #'magit-review-wash-review))

(defun magit-review-section (section title washer &rest args)
  (let ((magit-git-executable "git-review")
	(magit-git-global-arguments nil))
    (magit-insert-section (section title)
      (magit-insert-heading title)
      (magit-git-wash washer (split-string (car args)))
      (insert "\n"))))

(defun magit-review-remote-update (&optional remote)
  nil)

(defun magit-review-review-at-point ()
  (get-text-property (point) 'magit-review-jobj))

(defun magit-review-browse-review ()
  "Browse the Gerrit Review with a browser."
  (interactive)
  (let ((jobj (magit-review-review-at-point)))
    (if jobj
	(browse-url (cdr (assoc 'url jobj))))))

(defun magit-insert-gerrit-reviews ()
  (magit-review-section 'gerrit-reviews
			"Reviews:" 'magit-review-wash-reviews
			(gerrit-query)))

(defun magit-review-popup-args (&optional something)
  (or (magit-review-arguments) (list "")))

(defun magit-review-submit-review (args)
  "Submit a Gerrit Code Review"
  ;; "ssh -x -p 29418 user@gerrit gerrit review REVISION  -- --project PRJ --submit "
  (interactive (magit-review-popup-args))
  (gerrit-ssh-cmd "review"
		  (cdr-safe (assoc
			     'revision
			     (cdr-safe (assoc 'currentPatchSet
					      (magit-review-review-at-point)))))
		  "--project"
		  "--submit"
		  args)
  (let* ((branch (or (magit-get-current-branch)
		     (error "Don't push a detached head.  That's gross")))
	 (branch-remote (and branch (magit-get "branch" branch "remote"))))
    (magit-fetch-from-upstream branch-remote)))

(defun magit-review-push-review (status)
  (let* ((branch (or (magit-get-current-branch)
		     (error "Don't push a detached head.  That's gross")))
	 (commitid (or (when (eq (magit-section-type (magit-current-section))
				 'commit)
			 (magit-section-value (magit-current-section)))
		       (error "Couldn't find a commit at point")))
	 (rev (magit-rev-parse (or commitid
				   (error "Select a commit for review"))))

	 (branch-remote (and branch (magit-get "branch" branch "remote"))))

    ;; (message "Args: %s "
    ;;	     (concat rev ":" branch-pub))

    (let* ((branch-merge (if (string= branch-remote ".")
			     (completing-read
			      "Remote Branch: "
			      (let ((rbs (magit-list-remote-branch-names)))
				(mapcar
				 #'(lambda (rb)
				     (and (string-match (rx bos
							    (one-or-more (not (any "/")))
							    "/"
							    (group (one-or-more any))
							    eos)
							rb)
					  (concat "refs/heads/" (match-string 1 rb))))
				 rbs)))
			   (and branch (magit-get "branch" branch "merge"))))
	   (branch-pub (progn
			 (string-match (rx "refs/heads" (group (one-or-more any)))
				       branch-merge)
			 (format "refs/%s%s/%s" status (match-string 1 branch-merge) branch))))


      (when (string= branch-remote ".")
	(setq branch-remote magit-review-remote))

      (magit-run-git-async "push" "-v" branch-remote
			   (concat rev ":" branch-pub)))))

(defun magit-review-create-review ()
  (interactive)
  (magit-review-push-review 'publish))

(defun magit-review-create-draft ()
  (interactive)
  (magit-review-push-review 'drafts))

(defun magit-review-create-branch (branch parent))

(magit-define-popup magit-review-popup
  "Popup console for magit gerrit commands."
  'magit-review
  :actions '((?P "Push Commit For Review"                          magit-review-create-review)
	     (?W "Push Commit For Draft Review"                    magit-review-create-draft)
	     (?S "Submit Review"                                   magit-review-submit-review)
	     (?b "Browse Review"                                   magit-review-browse-review)))

;; Attach Magit Gerrit to Magit's default help popup
(magit-define-popup-action 'magit-dispatch-popup ?R "Gerrit"
  'magit-review-popup)

(defvar magit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map magit-review-popup-prefix 'magit-review-popup)
    map))

(define-minor-mode magit-review-mode "Gerrit support for Magit"
  :lighter " Gerrit" :require 'magit-topgit :keymap 'magit-review-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (or (magit-review-get-remote-url)
      (error "You *must* set `magit-review-remote' to a valid Gerrit remote"))
  (cond
   (magit-review-mode
    (magit-add-section-hook 'magit-status-sections-hook
			    'magit-insert-gerrit-reviews
			    'magit-insert-stashes t t)
    (add-hook 'magit-create-branch-command-hook
	      'magit-review-create-branch nil t)
    ;(add-hook 'magit-pull-command-hook 'magit-review-pull nil t)
    (add-hook 'magit-remote-update-command-hook
	      'magit-review-remote-update nil t)
    (add-hook 'magit-push-command-hook
	      'magit-review-push nil t))

   (t
    (remove-hook 'magit-after-insert-stashes-hook
		 'magit-insert-gerrit-reviews t)
    (remove-hook 'magit-create-branch-command-hook
		 'magit-review-create-branch t)
    ;(remove-hook 'magit-pull-command-hook 'magit-review-pull t)
    (remove-hook 'magit-remote-update-command-hook
		 'magit-review-remote-update t)
    (remove-hook 'magit-push-command-hook
		 'magit-review-push t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(defun magit-review-check-enable ()
  (let ((remote-url (magit-review-get-remote-url)))
    (when (boundp 'remote-url)
      ;; update keymap with prefix incase it has changed
      (define-key magit-review-mode-map magit-review-popup-prefix 'magit-review-popup)
      (magit-review-mode t))))

;; Hack in dir-local variables that might be set for magit gerrit
(add-hook 'magit-status-mode-hook #'hack-dir-local-variables-non-file-buffer t)

;; Try to auto enable magit-review in the magit-status buffer
(add-hook 'magit-status-mode-hook #'magit-review-check-enable t)
(add-hook 'magit-log-mode-hook #'magit-review-check-enable t)

(provide 'magit-review)

;;; magit-review.el ends here
