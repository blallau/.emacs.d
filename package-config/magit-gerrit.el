(require 'magit-gerrit)

;; if remote url is not using the default gerrit port and
;; ssh scheme, need to manually set this variable
;;(setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")

;; if necessary, use an alternative remote instead of 'origin'
(setq-default magit-gerrit-remote "gerrit")
