;; init.el --- Emacs configuration

;; LOAD PACKAGES
;; --------------------------------------
;; (setq debug-on-error t)
(require 'package)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Load and install mypackages
(load "~/.emacs.d/settings/mypackages.el")

;; Automatically update packages (via auto-package-update)
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(require 'auto-package-update)

;; BASIC CUSTOMIZATION
;; --------------------------------------

;; hide the startup message
(setq inhibit-startup-message t)

;; enable line numbers globally
(global-linum-mode t)
;; Set the line length globally (https://emacs.stackexchange.com/a/30222)
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default fill-column 120)

;; Delete trailing white space when saving in all modes except ein
;; https://emacs.stackexchange.com/a/40773/10100
;; (add-hook 'before-save-hook
;; 	  (lambda ()
;; 	    (unless (eq major-mode 'ein:notebook-multilang-mode)
;; 	      ('delete-trailing-whitespace))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
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
(add-to-list 'load-path "~/.emacs.d/settings")

;;; Autopair
;(require 'autopair-settings)
(load "~/.emacs.d/settings/autopair-settings.el")

;;; Flyspell
;(require 'flyspell-settings)
(load "~/.emacs.d/settings/flyspell-settings.el")

;;; EBIB
;(require 'ebib-settings)
(load "~/.emacs.d/settings/ebib-settings.el")

;;; EIN
;(require 'ein-settings)
(load "~/.emacs.d/settings/ein-settings.el")

;;; ESS
;(require 'ess-settings)
(load "~/.emacs.d/settings/ess-settings.el")

;;; Highlight Parenthesis
;(require 'highlight-parentheses-settings)
(load "~/.emacs.d/settings/highlight-parentheses-settings.el")

;;; Keychain Environment
;(require 'keychain-envinronment-settings)
(load "~/.emacs.d/settings/keychain-environment-settings.el")

;;; JS2 Environment
;(require 'js2-settings)
(load "~/.emacs.d/settings/js2-settings.el")

;;; Latex
;(require 'latex-settings)
(load "~/.emacs.d/settings/latex-settings.el")

;;; Literate Calc Mode
;(require 'latex-settings)
(load "~/.emacs.d/settings/literate-calc-settings.el")

;;; Magit
;(require 'magit-settings)
(load "~/.emacs.d/settings/magit-settings.el")

;;; Markdown
;(require 'markdown-settings)
(load "~/.emacs.d/settings/markdown-settings.el")

;;; MPDel
;(require 'mpdel-settings)
(load "~/.emacs.d/settings/mpdel-settings.el")

;;; Org-mode
;(require 'org-mode-settings)
(load "~/.emacs.d/settings/org-mode-settings.el")

;;; Polymode
;(require 'polymode-settings)
(load "~/.emacs.d/settings/polymode-settings.el")

;;; Powerline
;(require 'powerline-settings)
(load "~/.emacs.d/settings/powerline-settings.el")

;;; Python
;(require 'python-settings)
(load "~/.emacs.d/settings/python-settings.el")

;;; pylint
;(require 'pylint-settings)
;(load "~/.emacs.d/settings/pylint-settings.el")

;;; Rainbow delimiters
;(require 'rainbow-delimiters-settings)
(load "~/.emacs.d/settings/rainbow-delimiters-settings.el")

;;; Themes
;(require "~/.emacs.d/settings/theme-settings.el")
(load "~/.emacs.d/settings/theme-settings.el")


;;; tramp settings
;(require "~/.emacs.d/settings/tramp-settings.el")
(load "~/.emacs.d/settings/tramp-settings.el")

;;; ytdl settings
;(require "~/.emacs.d/settings/ytdl-settings.el")
(load "~/.emacs.d/settings/ytdl-settings.el")

;;; libvterm settings https://github.com/akermu/emacs-libvterm
;(require "~/.emacs.d/settings/libvterm-settings.el")
;(load "~/.emacs.d/settings/libvterm-settings.el")



;;; Split the window and start an R session
;; (split-window-horizontally)
;; (other-window 1)
(split-window-vertically)
;; (R)
(other-window 1)
(term "/bin/zsh")


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; Set default major mode for *scratch*
(setq initial-major-mode 'python-mode)
