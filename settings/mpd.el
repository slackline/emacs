;; MPD CONFIGURATION
;; --------------------------------------
;; Tried two packages for communicating with MPD mpdel never quite worked

;; https://gitea.petton.fr/mpdel/mpdel
(use-package mpdel
  :ensure t
  :init
  (setq libmpdel-hostname "192.168.1.28")
  (setq libmpdel-port 6600))
;; (mpdel-mode)
(use-package ivy-mpdel
  :ensure t
  :defer 2)

;; https://github.com/sp1ff/mpdmacs
;; (use-package mpdmacs
;; 	 :ensure t
;; 	 :init
;; 	 (setq mpdmacs-host "192.168.1.21")
;; 	 (setq mpdmacs-port 6600)
;; 	 :bind
;; 	 ("C-c m" . 'mpdmacs-mode-keymap)
;; 	 :hook (mpdmacs-mode-hook . mpdmacs-mode-keymap))
