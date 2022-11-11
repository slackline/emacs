;; MPDEL CONFIGURATION
;; --------------------------------------
(use-package mpdel
	     :defer 2
	     :init
	     (setq libmpdel-hostname "192.168.1.21")
	     (setq libmpdel-port 6600))
;; (mpdel-mode)
(use-package ivy-mpdel
	     :defer 2)
