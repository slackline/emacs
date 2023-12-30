;;; POWERLINE CONFIGURATION
;;; =======================
;;;
;;; GitHub        : https://github.com/milkypostman/powerline
;;; Customisation : https://jr0cket.co.uk/2015/01/custom-powerline-theme-for-Emacs-modeline.html
;;; Custom wave separator
(use-package powerline
  :init
  (powerline-default-theme)
  :config
  (setq powerline-default-separator 'wave)
  (setq-default mode-line-format (remove 'mode-line-buffer-identification mode-line-format)))
