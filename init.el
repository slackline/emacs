;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(alert
    apiwrap
    autopair
    auto-package-update
    ;;colour-parentheses
    better-defaults
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
    magit
    magithub
    material-theme
    ;;org-mode
    polymode
    ;; poly-R
    ;; poly-markdown
    projectile
    projectile-git-autofetch
    py-autopep8
    pyvenv
    pylint
    pytest
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

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Delete trailing white space when saving
(global-set-key [(control c) r] 'revert-buffer) ;; revert-buffer key binding
(put 'upcase-region 'disabled nil) ;; Uppercase region
(put 'downcase-region 'disabled nil) ;; Lowercase region

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
 '(package-selected-packages
   (quote
    (pytest ess-smart-underscore magit projectile-git-autofetch projectile poly-R poly-markdown elpy yasnippet yaml-mode wide-column vimish-fold pyvenv pylint py-autopep8 polymode package-utils package+ org-time-budgets material-theme julia-mode highlight-parentheses highlight-indentation flycheck find-file-in-project eink-theme ein darktooth-theme company better-defaults autopair auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
