;;; https://codeberg.org/emacs-weirdware/scratch
;;;
;;; C-u M-x scratch prompts for which buffer mode to invoke
(use-package scratch
  :ensure t
  :bind ("C-c s" . scratch))

;; https://git.sr.ht/~swflint/scratch-plus
(use-package scratch-plus
  :ensure t
  :defer 0.5
  :hook
  (add-hook 'after-init-hook #'scratch-plus-mode)
  :config
  (setq scratch-plus-save-directory "~/.config/emacs/scratch")
  (setq scratch-plus-project-subdir ".scratch"))
