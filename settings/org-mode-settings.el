;;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-font-lock-mode 1)
(setq org-directory        "~/Dropbox/org/")
(setq org-startup-indented 1)
;; Enable org-mode agenda/emacs diary integration
(setq org-agenda-include-diary t)
;; Enable org-mode babel R (see http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-R.html)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((R . t)))

;; MobileOrg
(setq org-mobile-directory "~/Dropbox/MobileOrg/")
;;; Include all org files
(setq org-agenda-files (file-expand-wildcards "~/Dropbox/org/*.org"))
;;; Set directory for pulling
(setq org-mobile-inbox-for-pull "~/Dropbox/MobileOrg/")
