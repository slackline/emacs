;; KEYCHAIN CONFIGURATION
;; ----------------------
;;
;; GitHub : https://github.com/tarsius/keychain-environment
(use-package keychain-environment
  :ensure t)
(keychain-refresh-environment)
(global-set-key (kbd "C-c k") 'keychain-refresh-environment)
