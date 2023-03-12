;;; PROJECTILE CONFIGURATION
;;; ------------------------
;;; https://github.com/bbatsov/projectile/
(use-package projectile
	     :ensure t
	     :defer 3
	     :bind
             ("C-c p p" . projectile-command-map))
