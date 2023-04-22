;; THEME CONFIGURATION
;; --------------------------------------
;;
;; I've tried out lots, one day I might settle on one.
;; Modus Themes (Vivendi) https://protesilaos.com/modus-themes/
;; EF Themes https://protesilaos.com/emacs/ef-themes
;; Customisation : https://protesilaos.com/codelog/2023-01-01-modus-themes-4-0-0/
;; Old versions  : https://systemcrafters.net/emacs-from-scratch/the-modus-themes/
;;
;; Choose to render some code constructs in slanted text (italics).  The
;; default, shown below, is to not use italics, unless it is absolutely
;; necessary.
(use-package modus-themes
	     :ensure t                         ; omit this to use the built-in themes
	     :init
	     ;; Add all your customizations prior to loading the themes
	     (setq modus-themes-italic-constructs t
		   modus-themes-bold-constructs t
		   modus-themes-org-blocks '(tinted-background))
	     :config
             :bind
             ("<f12>" . modus-themes-toggle))

(modus-themes-select 'modus-vivendi) ;; OR modus-operandi

(use-package ef-themes
	     :ensure t
	     :init
	     :config
	     :bind
	     ("<f11>" . ef-themes-toggle))
(ef-themes-select  'ef-bio)

;; Set the font
(set-face-attribute 'default t :font "Hack")

;; Other themes I've tried

;; (load-theme 'darktooth t)
;; (load-theme 'dracula t)
;; (load-theme 'nova t)
;; (load-theme 'material t)
;; (load-theme 'ample-zen t)
;; (load-theme #'abyss t)
;; (load-theme 'humanoid-dark t)

;; Kaolin
;; (use-package kaolin-themes
;;    :init
;;      (load-theme 'kaolin-light t)        ;; light variant of the original kaolin-dark.
;;      (load-theme 'kaolin-aurora t)       ;; Kaolin meets polar lights.
;;      (load-theme 'kaolin-bubblegum t)    ;; Kaolin colorful theme with dark blue background.
;;      (load-theme 'kaolin-temple t)       ;; dark brown background with syntax highlighting based on orange and cyan shades.
;;      (load-theme 'kaolin-galaxy t)       ;; bright theme based on one of the Sebastian Andaur arts.
;;      (load-theme 'kaolin-valley-light t) ;; light version of kaolin-valley-dark theme.
;; Kaolin - Dark
;;      (load-theme 'kaolin-dark t)         ;; a dark jade variant inspired by Sierra.vim.
;;      (load-theme 'kaolin-eclipse t)      ;; a dark purple variant.
;;      (load-theme 'kaolin-valley-dark t)  ;; colorful Kaolin theme with brown background.
;;      (load-theme 'kaolin-ocean t)        ;; dark blue variant.
;;      (load-theme 'kaolin-mono-dark t))   ;; almost monochrome dark green Kaolin theme.
