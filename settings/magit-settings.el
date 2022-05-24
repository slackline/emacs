;; MAGIT CONFIGURATION
;; --------------------------------------
(use-package magit
  :defer 0.5
;;  :requires forge
  :init
  (setq magit-repository-directories
      `(("~/dotfiles" . 1)
	    ("~/.config/emacs/" . 1)
	    ("~/org/" . 1)
	    ("~/work/org-roam/" . 1)
	    ("~/work/python/tcx2gpx/" . 1)
	    ("~/work/python/wpweather/" . 1)
        ))
   (setq auth-sources '("~/.authinfo.gpg"))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; FORGE CONFIGURATION
;; --------------------------------------
;; https://magit.vc/manual/ghub.html#Getting-Started
;; https://magit.vc/manual/forge.html#Getting-Started
;; Unnecessary as
;; (use-package forge
;;   :after magit)

;; Automatically refresh buffer
;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

;; Git modes
(use-package git-modes
  :defer t
  :ensure t)

(use-package gh-notify
  :defer t
  :ensure t)
