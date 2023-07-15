;; VTERM CONFIGURATION
;; --------------------------------------
;; https://github.com/akermu/emacs-libvterm
(use-package vterm
	     :ensure t
	     :config
	     (setq vterm-always-compile-module t
		   vterm-copy-exclude-prompt t
		   vterm-kill-buffer-on-exit t))
