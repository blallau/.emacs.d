(use-package elfeed
  :defer t
  :config
  (setq elfeed-feeds
      '(
        ("http://assafmuller.com/feed" openstack neutron)
        ("http://galsagie.github.iois/atom.xml" openstack neutron)
        ("http://rmadapur.blogspot.com/feeds/posts/default?alt=rss" openstack neutron)
        ("http://www.opencloudblog.com/?feed=rss2" openstack neutron)
        ("http://blog.gampel.net/feeds/posts/default?alt=rss" openstack neutron)
        ("http://blog.russellbryant.net/feed/" openstack neutron)
        ("http://blog.johnlikesopenstack.com/feeds/posts/default?alt=rss" openstack neutron)
        "http://sdake.io/feed/"
        "https://dague.net/feed/"
        ("http://www.danplanet.com/blog/feed/" openstack nova)
        "https://kimizhang.wordpress.com/feed/"
        )
      url-queue-timeout 30)

  (defface openstack-elfeed-entry
    '((t :foreground "#f77"))
    "Marks an Openstack Elfeed entry.")

  (push '(openstack openstack-elfeed-entry)
        elfeed-search-face-alist)
  :bind (("C-x w" . elfeed)))
