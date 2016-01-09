;;; magit-review.el --- Magit plugin for Gerrit Code Review
;;
;; Copyright (C) 2015 Bertrand Lallau
;;
;; Author: Bertrand Lallau <bertrand.lallau@gmail.com>
;; URL: https://github.com/blallau/magit-review
;; Version: 0.0.1
;; Keywords: tools gerrit git
;; Package-Requires: ((emacs "25.0") (magit "2.4.0") (projectile "0.13.0"))
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
;; Magit plugin for git review command (list, download, browse)
;;
;;; To Use:
;;
;; (require 'magit-review)
;;
;;
;; M-x `magit-status'
;; h R  <= magit-review uses the R prefix, see help
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
(require 'projectile)

(if (locate-library "magit-popup")
    (require 'magit-popup))
(require 'json)

(eval-when-compile
  (require 'cl-lib))

(defvar-local gerrit-review-url "https://review.openstack.org/#q,%s,n,z" "Gerrit review URL")

(defvar-local magit-review-remote "gerrit"
  "Default remote name to use for gerrit (e.g. \"origin\", \"gerrit\")")

(defvar-local git-review-protocol nil "Protocol used by project gerrit repository")

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
  (gerrit-command "-v -l"))

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

(defun magit-review-pp-https-review (num subj branch topic merg &optional ins_num del_num)
  ;; window-width - two prevents long line arrow from being shown
  (let* ((wid (- (window-width) 2))
	 (numstr (propertize (format "%-8s" num) 'face 'magit-hash))
	 (nlen (length numstr))
	 (btmaxlen (/ wid 4))

	 (bt (propertize (magit-review-string-trunc (if topic
							(format "%s (%s)" branch topic)
						      (format "%s" branch))
						    btmaxlen)
			 'face 'magit-log-author))

	 (subjmaxlen (- wid nlen btmaxlen 6))

	 (subjstr (propertize (magit-review-string-trunc subj subjmaxlen)
			      'face
			      (if (equal merg :json-false)
				  'magit-signature-bad
				'magit-signature-good)))
	 (padding (make-string
		     (max 0 (- wid (+ nlen 1 (length bt) (length subjstr))))
		     ? )))
    (format "%s%s%s%s\n" numstr subjstr padding bt)))

(defun magit-review-pp-ssh-review (num subj branch topic owner)
  ;; window-width - two prevents long line arrow from being shown
  (let* ((wid (- (window-width) 2))
	 (numstr (propertize (format "%-8s" num) 'face 'magit-hash))
	 (nlen (length numstr))
	 (btmaxlen (/ wid 5))
	 (ownermaxlen (/ wid 5))

	 (bt (propertize (magit-review-string-trunc (if topic
							(format "%s (%s)" branch topic)
						      (format "%s" branch))
						    btmaxlen)
			 'face 'magit-log-author))

	 (owner (propertize (magit-review-string-trunc owner ownermaxlen)
			    'face 'magit-log-author))

	 (subjmaxlen (- wid nlen ownermaxlen btmaxlen 7))

	 (subjstr (propertize (magit-review-string-trunc subj subjmaxlen)
			      'face 'magit-signature-good))
	 (padding (make-string
		     (max 0 (- wid (+ nlen 1 (length subjstr) 1 (length owner) 1 (length bt))))
		     ? )))
    (format "%s%s %s %s%s\n" numstr subjstr owner padding bt)))

(defun json-review-https-list-to-clean ()
  (if (search-forward-regexp "^)\\]}'$" (point-max) t)
      t
    nil))

(defun json-review-ssh-list-to-clean ()
  (if (search-forward-regexp "^{\"type\":\"stats\",\"rowCount\":[0-9]+.*}$" (point-max) t)
      t
    nil))

(defun magit-review-wash-review ()
  (cond
   ((string= git-review-protocol "https")
    (progn
      ;; clean review list
      (let ((json-to-clean (save-excursion (json-review-https-list-to-clean))))
	(when json-to-clean
	  (let ((beg (point)))
	    (search-forward-regexp "^\\[$")
	    (forward-line)
	    (delete-region beg (point-at-bol)))
	  (search-forward-regexp "^\\]$")
	  (delete-region (point-at-bol) (point-max))
	  (goto-char (point-min))))
      ;; process JSON

      ;; "id": "openstack%2Foctavia~master~Ic3d3d1d63a5cc352c5fc00dea58bb16915754a7c",
      ;; "project": "openstack/octavia",
      ;; "branch": "master",
      ;; "topic": "bug/1490033",
      ;; "hashtags": [

      ;; ],
      ;; "change_id": "Ic3d3d1d63a5cc352c5fc00dea58bb16915754a7c",
      ;; "subject": "WIP - Allow health manager to listen on mgmt net",
      ;; "status": "NEW",
      ;; "created": "2016-01-08 15:51:46.000000000",
      ;; "updated": "2016-01-08 17:13:10.000000000",
      ;; "mergeable": true,
      ;; "insertions": 15,
      ;; "deletions": 0,
      ;; "_number": 265322,
      ;; "owner": {
      ;;   "_account_id": 6951
      ;; }

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
	(when (and num subj branch)
	  (magit-insert-section (section subj)
	    (insert (propertize
		     (magit-review-pp-https-review num subj branch topic merg)
		     'magit-review-jobj
		     jobj))
	    (add-text-properties beg (point) (list 'magit-review-jobj jobj)))
	  t))))

   ((string= git-review-protocol "ssh")
    (progn
      ;; clean review list
      (let ((json-to-clean (save-excursion (json-review-ssh-list-to-clean))))
	(when json-to-clean
	  (let ((beg (point)))
	    (search-forward-regexp (format ".* Running: ssh -x.* %s query --format=JSON project:.* status:open$" magit-review-remote))
	    (forward-line)
	    (delete-region beg (point-at-bol)))
	  (search-forward-regexp "^{\"type\":\"stats\",\"rowCount\":[0-9]+,\"runTimeMilliseconds\":[0-9]+,\"moreChanges\":false}$")
	  (delete-region (point-at-bol) (point-max))
	  (goto-char (point-min))))

      ;; process JSON

      ;; "project": "openstack/octavia",
      ;; "branch": "master",
      ;; "topic": "bug/1490033",
      ;; "id": "Ic3d3d1d63a5cc352c5fc00dea58bb16915754a7c",
      ;; "number": "265322",
      ;; "subject": "WIP - Allow health manager to listen on mgmt net",
      ;; "owner": {
      ;;   "name": "Brandon Logan",
      ;;   "email": "brandon.logan@rackspace.com",
      ;;   "username": "brandon-logan"
      ;; },
      ;; "url": "https://review.openstack.org/265322",
      ;; "commitMessage": "WIP - Allow health manager to listen on mgmt net\n\nChange-Id: Ic3d3d1d63a5cc352c5fc00dea58bb16915754a7c\nCloses-Bug: #1490033\n",
      ;; "createdOn": 1452268306,
      ;; "lastUpdated": 1452273190,
      ;; "open": true,
      ;; "status": "NEW"

      (let* ((beg (point))
	     (jobj (json-read))
	     (end (point-at-eol))
	     (branch (cdr-safe (assoc 'branch jobj)))
	     (topic (cdr-safe (assoc 'topic jobj)))
	     (change_id (cdr-safe (assoc 'id jobj)))
	     (num (cdr-safe (assoc 'number jobj)))
	     (subj (cdr-safe (assoc 'subject jobj)))
	     (owner (cdr-safe (assoc 'owner jobj)))
	     (owner-name (cdr-safe (assoc 'name owner))))
	(if (and beg end)
	    (delete-region beg end))
	(when (and num subj branch)
	  (magit-insert-section (section subj)
	    (insert (propertize
		     (magit-review-pp-ssh-review num subj branch topic owner-name)
		     'magit-review-jobj
		     jobj))
	    (add-text-properties beg (point) (list 'magit-review-jobj jobj)))
	  t))))))

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

(defun magit-review-at-point ()
  (get-text-property (point) 'magit-review-jobj))

;; (defun magit-gerrit-view-patchset-diff ()
;;   "View the Diff for a Patchset"
;;   (interactive)
;;   (let ((jobj (magit-gerrit-review-at-point)))
;;     (when jobj
;;       (let ((ref (cdr (assoc 'ref (assoc 'currentPatchSet jobj))))
;; 	    (dir default-directory))
;; 	(let* ((magit-proc (magit-fetch magit-gerrit-remote ref)))
;; 	  (message (format "Waiting a git fetch from %s to complete..."
;; 			   magit-gerrit-remote))
;; 	  (magit-process-wait))
;; 	(message (format "Generating Gerrit Patchset for refs %s dir %s" ref dir))
;; 	(magit-diff "FETCH_HEAD~1..FETCH_HEAD")))))

(defun magit-gerrit-download-review ()
  "Download a Gerrit Review"
  (interactive)
  (let ((jobj (magit-review-at-point)))
    (when jobj
      (let ((ref (number-to-string (cdr (assoc '_number jobj))))
	    (topic (cdr (assoc 'topic jobj)))
	    (dir default-directory))
	(let* ((magit-proc (magit-run-git-async-no-revert "review" "-d" ref)))
	  (message (format "Waiting git review -d %s to complete..." ref))
	  (magit-process-wait))
	(message (format "Checking out to %s in %s" topic dir))))))

;; (defun gerrit-check-if-repo-modified ()
;;   "Check if current repo has been modified."
;;   (null (magit-git-items "status" "-z" "-uno" "--porcelain")))

;; (defun magit-gerrit-download ()
;;   (interactive (list (read-string "Review-ID: ")))
;;   (let* ((project (projectile-project-name))
;;          (default-directory (projectile-project-root)))
;;     (magit-with-toplevel
;;       (setq gerrit-project-cwd default-directory)
;;       (unless (gerrit-check-if-repo-modified)
;;         (error "%s has changes, not processing" project))
;;       (let ((proc (concat "git-review[" review-id "]")))
;;         (message "Starting git-review...")
;;         (start-process proc "*git review*" gerrit-review-program "-d" review-id)
;;         (set-process-sentinel
;;          (get-process proc)
;;          #'(lambda (process event)
;;              (let ((default-directory gerrit-project-cwd))
;;                (message event)
;;                (if (string= event "finished\n")
;;                    (magit-show-commit "HEAD")
;; 		 (error "Error while downloading review, check *git review* buffer.")))))))))

(defun magit-review-browse-review ()
  "Browse the Gerrit Review with a browser."
  (interactive)
  (let ((jobj (magit-review-at-point)))
    (when jobj
      (let ((gerrit-url (format gerrit-review-url (cdr (assoc '_number jobj)))))
	(browse-url gerrit-url)))))

(defun magit-insert-gerrit-reviews ()
  (magit-review-section 'gerrit-reviews
			"Reviews:" 'magit-review-wash-reviews
			(gerrit-query)))

(defun magit-review-popup-args (&optional something)
  (or (magit-review-arguments) (list "")))

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
	     ;;	     (?S "Submit Review"                                   magit-review-submit-review)
	     (?b "Browse Review"                                   magit-review-browse-review)
	     ;;	     (?A "Add Reviewer"                                    magit-gerrit-add-reviewer)
	     (?D "Download Review"                                 magit-gerrit-download-review)
	     ))

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
  (let* ((remote-url (magit-review-get-remote-url))
	 (url-type (url-type (url-generic-parse-url remote-url))))
    (cond
     ((string= url-type "ssh")
      (setq git-review-protocol "ssh")
      (define-key magit-review-mode-map magit-review-popup-prefix 'magit-review-popup)
      (magit-review-mode t))
     ((string= url-type "https")
      (setq git-review-protocol "https")
      ;; update keymap with prefix incase it has changed
      (define-key magit-review-mode-map magit-review-popup-prefix 'magit-review-popup)
      (magit-review-mode t))
     ((or (not (bound-and-true-p url-type)) (string= url-type ""))
      (error (format "%s repository url is not set" magit-review-remote))))))

;; Hack in dir-local variables that might be set for magit gerrit
(add-hook 'magit-status-mode-hook #'hack-dir-local-variables-non-file-buffer t)

;; Try to auto enable magit-review in the magit-status buffer
(add-hook 'magit-status-mode-hook #'magit-review-check-enable t)
(add-hook 'magit-log-mode-hook #'magit-review-check-enable t)

(defun magit-gerrit-switch-project ()
  (interactive)
  (when (projectile-project-p)
    ;; remove previous hook if any
    (remove-hook 'magit-status-mode-hook #'magit-review-check-enable t)
    (remove-hook 'magit-log-mode-hook #'magit-review-check-enable t)

    ;; add hook
    (add-hook 'magit-status-mode-hook #'magit-review-check-enable t)
    (add-hook 'magit-log-mode-hook #'magit-review-check-enable t)
    ))

(add-hook 'projectile-switch-project-hook #'magit-gerrit-switch-project)

(provide 'magit-review)

;;; magit-review.el ends here
