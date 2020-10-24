;; KEY BINDINGS
;; --------------------------------------

;; Misc
(global-set-key [(control c) r] 'revert-buffer)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c P") 'magit-push-current-to-upstream)

;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; Cliplink setup
;; From : https://github.com/rexim/org-cliplink
(global-set-key (kbd "C-x p i") 'org-cliplink)
