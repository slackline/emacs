;; MAGIT CONFIGURATION
;; --------------------------------------
(use-package magit
  :defer 0.5)
;; (require 'magit-todos)


;; Repositories
(setq magit-repository-directories
      `(("~/dotfiles" . 1)
	("~/.config/emacs/" . 1)
	("~/dotfiles/oh-my-zsh/.oh-my-zsh" . 1)
	("~/thefloow/code/DS" . 2)))

;; Automatically refresh buffers
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)
