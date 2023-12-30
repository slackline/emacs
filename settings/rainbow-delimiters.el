;; RAINBOW DELIMITERS CONFIGURATION
;; --------------------------------------
;; https://github.com/Fanael/rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package rainbow-mode
  :ensure t
  :hook
  (prog-mode-hook . rainbow-mode))
