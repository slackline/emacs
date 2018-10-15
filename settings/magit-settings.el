;; MAGIT CONFIGURATION
;; --------------------------------------
(require 'magit)

;; Key bindings
(global-set-key (kbd "C-x g") 'magit-status)

;; Repositories
(setq magit-repository-directories
      `(("~/dotfiles" . 1)
	("~/dotfiles/emacs/.emacs.d" . 1)
	("~/dotfiles/oh-my-zsh/.oh-my-zsh" . 1)
	("~/thefloow/code/DS" . 2)))
