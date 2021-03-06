(use-package erc
  :commands erc-start-or-switch
  :defer t
  :config
  ;; joining && autojoing

  ;; make sure to use wildcards for e.g. freenode as the actual server
  ;; name can be be a bit different, which would screw up autoconnect
  (erc-autojoin-mode t)
  (setq erc-autojoin-channels-alist
        '((".*\\.freenode.net" "#openstack-kolla" "#openstack-meeting-4")))

  ;; check channels
  (erc-track-mode t)
  (setq erc-track-exclude-types '("NICK" "JOIN" "PART" "QUIT" "MODE"
                                  "301" ; away notice
                                  "305" ; return from awayness
                                  "306" ; set awayness
                                  "324"
                                  "329"
                                  "332" ; topic notice
                                  "333" ; who set the topic
                                  "353" ; Names notice
                                  "324" ; modes
                                  "329" ; channel creation date
                                  "477")
        ;; don't show any of this
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK")

        erc-header-line-format "%t: %o"
        erc-join-buffer 'bury
        erc-warn-about-blank-lines nil
        erc-interpret-mirc-color t
        ;; server conf
        erc-server-reconnect-attempts t
        erc-server-reconnect-timeout 10

        erc-keywords '("octavia"))

  (defun erc-start-or-switch ()
    "Connect to ERC, or switch to last active buffer"
    (interactive)
    (if (get-buffer "irc.freenode.net:8000") ;; ERC already active?

        (erc-track-switch-buffer 1) ;; yes: switch to last active
      (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
        (erc :server "irc.freenode.net" :port 8000 :nick "blallau" :full-name "Bertrand LALLAU"))))

  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'match)

  ;; other random services (spelling)
  (use-package erc-services
    :init
    (add-to-list 'erc-modules 'spelling)
    (erc-services-mode 1)
    (erc-spelling-mode 1))
  (erc-update-modules)

  (erc-notifications-mode)
  (add-hook 'focus-out-hook #'erc-notifications-enable)
  (add-hook 'focus-in-hook #'erc-notifications-disable))

;; switch to ERC with Ctrl+c e
;;(global-set-key (kbd "<f12>") 'erc-start-or-switch) ;; ERC
