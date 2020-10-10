;; MAGIT CONFIGURATION
;; --------------------------------------
(use-package magit
  :defer 0.5)
;; (require 'magit-todos)

;; Key bindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c P") 'magit-push-current-to-upstream)

;; Repositories
(setq magit-repository-directories
      `(("~/dotfiles" . 1)
	("~/.config/emacs/" . 1)
	("~/dotfiles/oh-my-zsh/.oh-my-zsh" . 1)
	("~/thefloow/code/DS" . 2)))

;; Automatically refresh buffers
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)
