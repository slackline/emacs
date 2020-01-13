;; TRAMP CONFIGURATION
;; --------------------------------------

(setq tramp-default-method "ssh")

;; Set default usernames for different hosts and a global default.
(add-to-list 'tramp-default-user-alist
             '("ssh" ".*ovh'" "arch"))
(add-to-list 'tramp-default-user-alist
             '("ssh" ".*openwrt" "admin") t)
(add-to-list 'tramp-default-user-alist
             '(nil nil "neil") t)
