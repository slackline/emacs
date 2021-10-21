;; SMARTPARENS CONFIGURATION
;; --------------------------------------
;; smarparents for automatic parenthesis (and more?) matching
;;
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))
