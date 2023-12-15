;;; PROJECTILE CONFIGURATION
;;; ------------------------
;;; https://github.com/bbatsov/projectile/
;;; https://docs.projectile.mx/
(use-package projectile
  :ensure t
  :defer 3
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/org/" "~/org-roam" "~/.config/emacs" ("~/work/git/" . 3)))
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action #'projectile-commander))
