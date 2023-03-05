;;; PROJECT CONFIGURATION
;;; --------------------------------------
;;; https://elpa.gnu.org/packages/project.html
(use-package projectile
	     :ensure t
	     :defer 3
	     :config
	     (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
