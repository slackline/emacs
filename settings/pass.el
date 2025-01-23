;;; PASS CONFIGURATION
;;; --------------------------------------
;; https://github.com/NicolasPetton/pass
(use-package pass
  :ensure t)
;; https://github.com/rjekker/password-store-menu
(use-package password-store-menu
  :ensure t
  :config (password-store-menu-enable)
  :custom (password-store-menu-key "C-c C-p"))
;; https://github.com/volrath/password-store-otp.el
(use-package password-store-otp
  :ensure t)
