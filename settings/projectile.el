;;; PROJECTILE CONFIGURATION
;;; ------------------------
;;; https://github.com/bbatsov/projectile/
(use-package projectile
  :ensure t
  :defer 3
  :bind
  ("C-c C-p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/org/" "~/.config/emacs" ("~/work/git/" . 1))))
