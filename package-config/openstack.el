;; Copyright © 2015 Bertrand Lallau

;; Author: Bertrand LALLAU <bertrand.lallau@gmail.com>
;; URL: https://github.com/blallau/emacs-config
;; Package-Version: 1.0.0
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

(require 'projectile)

(defvar launchpad-url "https://bugs.launchpad.net/" "launchpad URL")
(defvar openstack-review-url "https://review.openstack.org/#q,%s,n,z" "Openstack review URL")
(defvar openstack-irc-logs-url "http://eavesdrop.openstack.org/irclogs/%23openstack-" "Openstack irc logs URL")

(defun open-openstack-ID-at-point ()
  (interactive)
  (when (projectile-project-p)
    (let ((identifiant (thing-at-point 'line))
          (project-name (projectile-project-name))
          (case-fold-search t))
      (cond
       ((string-match "[[:alpha:]]+-Bug:[[:space:]]*#?\\([[:digit:]]+\\)" identifiant)
        (browse-url (concat launchpad-url project-name "/+bug/" (match-string 1 identifiant))))
       ((string-match "Change-Id: \\([[:alnum:]]+\\)" identifiant)
        (browse-url (format openstack-review-url (match-string 1 identifiant))))
       (t
        (message "Not an Openstack ID (Closes-Bug or Change-Id)"))))))

(global-set-key (kbd "<f12>") 'open-openstack-ID-at-point)

(defun open-openstack-irc-logs-on-project ()
  (interactive)
  (when (projectile-project-p)
    (let ((project-name (projectile-project-name)))
      (cond
       ((string-match "[[:alpha:]]+-\\([[:alpha:]]+\\)" project-name)
        (browse-url (concat openstack-irc-logs-url (match-string 1 project-name) "/latest.log.html")))
       (t
        (browse-url (concat openstack-irc-logs-url project-name "/latest.log.html")))))))


(global-set-key (kbd "<C-f12>") 'open-openstack-irc-logs-on-project)
