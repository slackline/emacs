;; TRAMP CONFIGURATION
;; --------------------------------------
;; https://emacs.stackexchange.com/questions/47969/trouble-connecting-gnu-emacs-to-work-machine-through-ssh-tramp
;; https://emacs.stackexchange.com/a/48089

(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-default-method-alist '("" "neil" "ssh"))
  ;; Set prompt so it doesn't hang
  (setq shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *")
  (setq tramp-auto-save-directory "~/.config/emacs/tmp/")
  ;; Set default usernames for different hosts and a global default.
  (add-to-list 'tramp-default-user-alist
	       '("ssh" ".*ovh'" "arch") t)
  (add-to-list 'tramp-default-user-alist
	       '("ssh" ".*openwrt" "admin") t)
  (add-to-list 'tramp-default-user-alist
	       '(nil nil "neil") t)
  (add-to-list 'tramp-default-user-alist
	       '("ssh" ".*alarmpi-4b" "neil") t)
  )
