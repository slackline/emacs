;;; MASTODON CONFIGURATION
;;; --------------------------------------
;;; https://codeberg.org/martianh/mastodon.el
(use-package mastodon
	     :ensure t
	     :config
	     (setq mastodon-instance-url "https://mastodon.social"
		   mastodon-active-user "slackline"))
