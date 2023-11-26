;; THEME CONFIGURATION
;; --------------------------------------
;;
;; I've tried out lots, one day I might settle on one.
;; Modus Themes (Vivendi) https://protesilaos.com/modus-themes/
;; EF Themes https://protesilaos.com/emacs/ef-themes
;; Customisation : https://protesilaos.com/codelog/2023-01-01-modus-themes-4-0-0/
;; Old versions  : https://systemcrafters.net/emacs-from-scratch/the-modus-themes/
;;
;; Look out for https://git.sr.ht/~protesilaos/dired-preview
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

;; (modus-themes-select 'modus-vivendi) ;; OR modus-operandi

(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-disable-other-themes 'ef-themes-light-themes)
  :config
  :bind
  ("<f10>" . ef-themes-select-dark)
  ("<f11>" . ef-themes-toggle))
;; (ef-themes-select 'ef-dark)
;; (ef-themes-select 'ef-duo-dark)
(ef-themes-select 'ef-bio)
;; (ef-themes-select 'ef-symbiosis)
;; (ef-themes-select 'ef-night)
;; (ef-themes-select 'ef-trio-dark)
;; (ef-themes-select 'ef-autumn)
;; (ef-themes-select 'ef-winter)

;; Set the font
(set-face-attribute 'default t :font "Hack")


;; Mood line
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  :custom
  (setq mood-line-glyph-alist mood-line-glyphs-unicode))
;; Custom Modeline (https://protesilaos.com/codelog/2023-07-29-emacs-custom-modeline-tutorial/)
;; (setq-default mode-line-format
;;               '("%e"
;;                 prot-modeline-kbd-macro
;;                 prot-modeline-narrow
;;                 prot-modeline-input-method
;;                 prot-modeline-buffer-status
;;                 " "
;;                 prot-modeline-buffer-identification
;;                 "  "
;;                 prot-modeline-major-mode
;;                 prot-modeline-process
;;                 "  "
;;                 prot-modeline-vc-branch
;;                 "  "
;;                 prot-modeline-flymake
;;                 "  "
;;                 prot-modeline-align-right
;;                 prot-modeline-misc-info))


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
