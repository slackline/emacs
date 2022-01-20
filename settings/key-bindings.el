;; KEY BINDINGS
;; --------------------------------------

;; Misc
;; (global-set-key [(control c) r] 'revert-buffer)
;; (global-set-key (kbd "C-c e") 'eval-region)
;; (global-set-key (kbd "C-c E") 'eval-buffer)

;; Magit
;; (global-set-key (kbd "C-x g") 'magit-status)
;; (global-set-key (kbd "C-c P") 'magit-push-current-to-upstream)

;; Org-mode
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
;; Cliplink setup
;; From : https://github.com/rexim/org-cliplink
;; (global-set-key (kbd "C-x p i") 'org-cliplink)

;; Keychain
;; (global-set-key (kbd "C-c k") 'keychain-refresh-environment)

;; Custom commands
;; (global-set-key (kbd "C-c r") 'rsync-html)

;;; Some generally useful key-bindings (mostly ESS specific) from
;;; http://stats.blogoverflow.com/page/2/
(define-key global-map [f1] 'Control-X-prefix)
(define-key global-map [f2] 'save-buffer)
(define-key global-map [f3] 'find-file)
(define-key global-map [f5] 'switch-to-buffer)
(define-key global-map [f6] 'other-window)
(define-key global-map [f8] 'kill-buffer)
(define-key global-map [f9] 'ess-load-file)

;; EIN commands
(local-set-key [f12] 'ein:worksheet-delete-cell)
(local-set-key [f5] 'ein:notebook-reconnect-kernel)
