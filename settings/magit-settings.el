;; MAGIT CONFIGURATION
;; --------------------------------------
(require 'magit)

;; Key bindings
(global-set-key (kbd "C-x g") 'magit-status)

;; Repositories
(setq magit-repository-directories
      `(("~/dotfiles" . 4)
	("~/thefloow/code/DS" . 2)))
