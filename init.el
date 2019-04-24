;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------
;; (setq debug-on-error t)
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(alert
    ;; apiwrap
    autopair
    auto-package-update
    ;;colour-parentheses
    darktooth-theme
    better-defaults
    blacken
    ein
    elpy
    ess
    ess-smart-underscore
    flycheck
    ;;flycheck-json
    ;;flycheck-pycheck
    ;;flycheck-yaml
    gitlab
    highlight-parentheses
    jedi
    ;; latex-extra
    latex-preview-pane
    magit
    ;;magithub
    material-theme
    mmm-mode
    ;;org-mode
    org-parser
    pass
    polymode
    ;; poly-R
    ;; poly-markdown
    poly-R
    poly-noweb
    projectile
    projectile-git-autofetch
    py-autopep8
    py-yapf
    pyvenv
    pylint
    pytest
    python-mode
    python-pytest
    wide-column
    yaml-mode))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; Automatically update packages (via auto-package-update)
(require 'auto-package-update)

;; BASIC CUSTOMIZATION
;; --------------------------------------

;; hide the startup message
(setq inhibit-startup-message t)
;; load material theme
(load-theme 'material t)
;; enable line numbers globally
(global-linum-mode t)
;; Delete trailing white space when saving in all modes except ein
;; https://emacs.stackexchange.com/a/40773/10100
(add-hook 'before-save-hook
	  (lambda ()
	    (unless (eq major-mode 'ein:notebook-multilang-mode)
	      ('delete-trailing-whitespace))))
;; revert-buffer key binding
(global-set-key [(control c) r] 'revert-buffer)
;; Uppercase region
(put 'upcase-region 'disabled nil)
;; Lowercase region
(put 'downcase-region 'disabled nil)

;; Add local lisp for miscellaneous things
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Reload a buffer from disk
;;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t))

;; Set the frames title to be that of the currently visited buffer
(setq frame-title-format "%b")

;; PACKAGE SPECIFIC CONFIGURATION
;; --------------------------------------
;; Splitting settings into individual files as this has become monolithic and unnavigable
;; Path where settings files are kept
;(add-to-list 'load-path "~/.emacs.d/settings")

;;; Autopair
;(require 'autopair-settings)
(load "~/.emacs.d/settings/autopair-settings.el")

;;; Colour parenthesis
;(require 'colour-parentesis-settings)
;(load "~/.emacs.d/settings/colour-parenthesis-settings.el")

;;; Colour theme
;(require 'color-theme-settings)
;(load "~/.emacs.d/settings/color-theme-settings.el")

;;; EIN
;(require 'ein-settings)
(load "~/.emacs.d/settings/ein-settings.el")

;;; ESS
;(require 'ess-settings)
(load "~/.emacs.d/settings/ess-settings.el")

;;; Highlight Parenthesis
;(require 'highlight-parentheses-settings)
(load "~/.emacs.d/settings/highlight-parentheses-settings.el")

;;; Latex
;(require 'latex-settings)
(load "~/.emacs.d/settings/latex-settings.el")

;;; Magit
;(require 'magit-settings)
(load "~/.emacs.d/settings/magit-settings.el")

;;; Org-mode
;(require 'org-mode-settings)
(load "~/.emacs.d/settings/org-mode-settings.el")

;;; Polymode
;(require 'polymode-settings)
(load "~/.emacs.d/settings/polymode-settings.el")

;;; Python
;(require 'python-settings)
(load "~/.emacs.d/settings/python-settings.el")

;;; pylint
;(require 'pylint-settings)
;(load "~/.emacs.d/settings/pylint-settings.el")

;;; Vimish-fold
;(require 'vimish-fold-settings)
;(load "~/.emacs.d/settings/vimish-fold-settings.el")

;;; Split the window and start an R session
(split-window-horizontally)
;; (other-window 1)
;; (split-window-vertically)
;; (R)
(other-window 1)
;(term "/bin/zsh")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-enabled-themes (quote (darktooth)))
 '(custom-safe-themes
   (quote
    ("88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" default)))
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (jedi latex-preview-pane py-yapf blacken gitlab mmm-mode python-pytest python-mode pytest ess-smart-underscore magit projectile-git-autofetch projectile poly-R poly-markdown elpy yasnippet yaml-mode wide-column vimish-fold pyvenv pylint py-autopep8 polymode package-utils package+ org-time-budgets material-theme julia-mode highlight-parentheses highlight-indentation flycheck find-file-in-project eink-theme ein darktooth-theme company better-defaults autopair auto-package-update)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
