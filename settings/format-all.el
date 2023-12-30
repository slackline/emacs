;;; FORMAT-ALL CONFIGURATION
;;; --------------------------------------
;;; https://github.com/lassik/emacs-format-all-the-code/
(use-package format-all
  :ensure t
  :config
  (setq format-all-show-errors 'never)
  :hook
  (prog-mode . format-all-mode))
