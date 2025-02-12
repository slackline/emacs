;; https://github.com/anticomputer/age.el
(use-package age
  :ensure t
  :demand t
  :custom
  (age-program "rage")
  ;; (age-default-identity "~/.ssh/age_yubikey")
  ;; (age-default-recipient
  ;;  '("~/.ssh/age_yubikey.pub"
  ;;    "~/.ssh/age_recovery.pub"))
  :config
  (age-file-enable))
