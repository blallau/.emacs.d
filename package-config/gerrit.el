;;; gerrit.el --- Gerrit reviews management tool

;; Copyright (C) 2015 Bertrand Lallau

;; Author: Bertrand Lallau <bertrand.lallau@gmail.com>
;; URL: https://github.com/blallau/gerrit.el
;; Version: 0.0.1
;; Keywords: tools gerrit git
;; Package-Requires: ((emacs "25.0") (magit "2.1.0") (projectile "0.13.0"))
;;
;;; Commentary:

;;; This is a mode that will allow listing and downloading a review
;;; from gerrit using the `git-review' software.
;;;
;;; This is using magit and projectile to download the change into.

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'magit)
(require 'projectile)

;; User variables
(defvar gerrit-review-program "git-review" "Gerrit review binary name.")

;; Internal variables
(defvar gerrit-project-cwd nil)

;; Functions
(defun gerrit-check-if-repo-modified ()
  "Check if current repo has been modified."
  (null (magit-git-items "status" "-z" "-uno" "--porcelain")))

;;;###autoload
(defun gerrit-download (review-id)
  (interactive (list (read-string "Review-ID: ")))
  (let* ((project (projectile-project-name))
         (default-directory (projectile-project-root)))
    (magit-with-toplevel
      (setq gerrit-project-cwd default-directory)
      (unless (gerrit-check-if-repo-modified)
        (error "%s has changes, not processing" project))
      (let ((proc (concat "git-review[" review-id "]")))
        (message "Starting git-review...")
        (start-process proc "*git review*" gerrit-review-program "-d" review-id)
        (set-process-sentinel
         (get-process proc)
         #'(lambda (process event)
             (let ((default-directory gerrit-project-cwd))
               (message event)
               (if (string= event "finished\n")
                   (magit-show-commit "HEAD")
		 (error "Error while downloading review, check *git review* buffer.")))))))))

;;; End gerrit.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gerrit)
;;; gerrit.el ends here
