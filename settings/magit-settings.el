;; MAGIT CONFIGURATION
;; --------------------------------------
(use-package magit
  :defer 0.5
  :init
  (setq magit-repository-directories
      `(("~/dotfiles" . 1)
	("~/.config/emacs/" . 1)
	("~/org/" . 1))))

;; Automatically refresh buffer
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)
