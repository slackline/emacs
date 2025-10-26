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
;; Create themes : https://emacsfodder.github.io/emacs-theme-editor/#
;;
;; Consider using highlight-changes-mode at some point
;; https://tommorris.org/posts/2025/til-track-changes-in-emacs-with-highlight-changes-mode/
;;
;; Choose to render some code constructs in slanted text (italics).  The
;; default, shown below, is to not use italics, unless it is absolutely
;; necessary.
;;
;; 2025-10-01 see https://protesilaos.com/codelog/2025-10-01-emacs-modus-framework-ef-built-on-top/
(use-package modus-themes
  :ensure t ;; omit this to use the built-in themes
  :defer 0.5
  :init
  ;; Add all your customisation's prior to loading the themes
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-org-blocks '(tinted-background)
	modus-themes-include-derivatives-mode 1)
  :config
  :bind
  ("C-c C-t m" . modus-themes-toggle))

;; (modus-themes-select 'modus-vivendi) ;; OR modus-operandi
(use-package ef-themes
  :ensure t
  :defer 0.5
  :init
  (setq ef-themes-disable-other-themes 'ef-themes-light-themes)
  (setq ef-themes-take-over-modus-themes-mode 0)
  :config
  :bind
  ("C-c C-t d" . ef-themes-select-dark)
  ("C-c C-t e" . ef-themes-toggle))
(load-theme 'ef-dark :no-confirm)
;; (ef-themes-select 'ef-dark)
;; (ef-themes-select 'ef-duo-dark)
;; (ef-themes-select 'ef-bio)
;; (ef-themes-select 'ef-symbiosis)
;; (ef-themes-select 'ef-night)
;; (ef-themes-select 'ef-trio-dark)
;; (ef-themes-select 'ef-autumn)
;; (ef-themes-select 'ef-winter)

;; https://github.com/catppuccin/emacs
;; (use-package  catppuccin-theme
;;   :ensure t
;;   :defer 0.5
;;   :config
;;   (setq catppuccin-flavor 'mocha) ;; or 'frappe 'latte, 'macchiato, or 'mocha
;;   (catppuccin-reload))

;; Set the font
(set-face-attribute 'default t :font "Hack")

;; Code folding https://mastodon.social/@dotemacs@mastodon.xyz/114176758441575809
(use-package hideshow
  :ensure t
  :hook ((prog-mode . hs-minor-mode))
  :config
  (defvar toggle-fold-dont-eol-for-modes '(emacs-lisp-mode lisp-interaction-mode))

  (defun toggle-fold ()
    "Taken from: https://www.reddit.com/r/emacs/comments/746cd0/comment/dnwi2x1/"
    (interactive)
    (save-excursion
      (unless (member major-mode toggle-fold-dont-eol-for-modes)
        (end-of-line))
      (hs-toggle-hiding)))
  :bind ("C-c f" . toggle-fold))

;; Mood line
(use-package mood-line
  :ensure t
  :defer 0.5
  :config
  (mood-line-mode)
  :custom
  (setq mood-line-glyph-alist mood-line-glyphs-unicode))

;; https://github.com/tarsius/minions
(use-package minions
  :ensure t)

;; https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :ensure t
  :defer 0.5
  :custom
  (setq golden-ratio-auto-scale t))

;; https://github.com/j-hotlink/hotdesk
(use-package hotdesk
  :ensure t
  :config
  (setq hotdesk-mode 1))
;;; https://github.com/milkypostman/powerline
;;; Customisation : https://jr0cket.co.uk/2015/01/custom-powerline-theme-for-Emacs-modeline.html
;;; Custom wave separator
;; (use-package powerline
;;   :init
;;   (powerline-default-theme)
;;   :config
;;   (setq powerline-default-separator 'wave)
;;   (setq-default mode-line-format (remove 'mode-line-buffer-identification mode-line-format)))

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


;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :defer 1
  :config
  (setq highlight-indent-guides-method 'bitmap)
  :hook
  (prog-mode . highlight-indent-guides-mode))


;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :defer 1
  :init
  (setq smartparens-global-mode t)
  :hook
  (prog-mode . smartparens-mode)
  (text-mode . smartparens-mode)
  (markdown-mode . smartparens-mode)
  (latex-mode . smartparents-mode)
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer 1
  :init
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :defer 1
  :hook
  (prog-mode . rainbow-mode))


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
;;   :ensure t
;;   :init
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
