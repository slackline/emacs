;;; Autoscratch CONFIGURATION
;;; --------------------------------------
;;; https://github.com/tlinden/autoscratch
(use-package autoscratch
  :ensure t
  :config
  (setq initial-major-mode 'autoscratch-mode
	initial-scratch-message ""
	setq inhibit-startup-screen t))
