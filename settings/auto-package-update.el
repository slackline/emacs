;;; AUTO-PACKAGE-UPDATE CONFIGURATION
;;; --------------------------------------
;;; Also always ensures a package is installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Updating packages automatically
;; https://github.com/rranelli/auto-package-update.el
(use-package auto-package-update
	     :ensure t
	     :config
	     (setq auto-package-update-delete-old-versions t
		   auto-package-update-interval 2
		   auto-package-update-hide-results t))
