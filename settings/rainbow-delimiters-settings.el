;; RAINBOW DELIMITERS CONFIGURATION
;; --------------------------------------
;; https://github.com/Fanael/rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package rainbow-mode
	     :ensure t
	     :defer 1
  	     :hook
	     (prog-mode-hook . rainbow-mode)
	     )
