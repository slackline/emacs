;;; MESSAGING CONFIGURATION
;;; --------------------------------------
;;; https://codeberg.org/martianh/mastodon.el
(use-package mastodon
  :ensure t
  :config
  ;; (setq mastodon-instance-url "https://fosstodon.org"
  ;;       mastodon-active-user "nshephard")
  (setq mastodon-instance-url "https://mastodon.social"
	mastodon-active-user "slackline")
  )

;;; https://github.com/alphapapa/ement.el
;;;
;;; Doesn't yet support native room encryption, requires reverse proxy via https://github.com/matrix-org/pantalaimon/
(use-package ement
  :ensure t)

;;; https://github.com/isamert/emacs-slack
;; (use-package slack
;;   :ensure t
;;   )
