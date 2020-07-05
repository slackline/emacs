;; MAGIT CONFIGURATION
;; --------------------------------------
(require 'magit)
(require 'magit-todos)

;; Key bindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c P") 'magit-push-current-to-upstream)

;; Repositories
(setq magit-repository-directories
      `(("~/dotfiles" . 1)
	("~/dotfiles/emacs/.emacs.d" . 1)
	("~/dotfiles/oh-my-zsh/.oh-my-zsh" . 1)
	("~/thefloow/code/DS" . 2)))
