;; THEME CONFIGURATION
;; --------------------------------------
;;
;; I've tried out lots, one day I might settle on one.

;; (load-theme 'darktooth t)
;; (load-theme 'dracula t)
;; (load-theme 'nova t)
;; (load-theme 'material t)
;; (load-theme 'ample-zen t)
;; (load-theme #'abyss t)

;; Humanoid
;; (load-theme 'humanoid-dark t)
;; (load-theme 'humanoid-light t)

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


;; Modus Themes (Vivendi) https://protesilaos.com/modus-themes/
;;
;; Choose to render some code constructs in slanted text (italics).  The
;; default, shown below, is to not use italics, unless it is absolutely
;; necessary.
(use-package modus-themes
    :ensure                         ; omit this to use the built-in themes
    :init
    ;; Add all your customizations prior to loading the themes
    (setq modus-themes-italic-constructs t
          modus-themes-bold-constructs nil
	  modus-themes-paren-match '(underline)
          modus-themes-region '(bg-only no-extend)
	  modus-themes-org-block '(tinted-background))

    ;; Load the theme files before enabling a theme (else you get an error).
    (modus-themes-load-themes)
    :config
    ;; Load the theme of your choice:
    (modus-themes-load-vivendi) ;; OR (modus-themes-load-operandi)
    :bind ("<f5>" . modus-themes-toggle))

;; Use proportionately-spaced fonts (variable-pitch) for headings.  The
;; default is to use whatever font the user has selected, typically a
;; monospaced typeface.
;; (setq modus-vivendi-theme-proportional-fonts nil)

;; Whether headings should be scaled or have the same height as body
;; text.  The default is to keep everything the same as the base size.
;; (setq modus-vivendi-theme-scale-headings nil)

;; Font scale that should apply to headings.  These are the default values.
;; (setq modus-vivendi-theme-scale-1 1.05)
;; (setq modus-vivendi-theme-scale-2 1.1)
;; (setq modus-vivendi-theme-scale-3 1.15)
;; (setq modus-vivendi-theme-scale-4 1.2)
(load-theme 'modus-vivendi t)
