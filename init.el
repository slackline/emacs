;; init.el --- Emacs configuration

;; LOAD PACKAGES
;; --------------------------------------
;; (setq debug-on-error t)
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Load and install mypackages
(load "~/.config/emacs/settings/mypackages.el")

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Automatically update packages (via auto-package-update)
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(require 'auto-package-update)

;; SETUP use-package
(eval-when-compile
  (require 'use-package))

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
;;(global-set-key [(control c) r] 'revert-buffer)
;; Uppercase region
(put 'upcase-region 'disabled nil)
;; Lowercase region
(put 'downcase-region 'disabled nil)

;; Add local lisp for miscellaneous things
(add-to-list 'load-path "~/.config/emacs/lisp/")

;; revert-buffer key binding
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Reload a buffer from disk
;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
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
(add-to-list 'load-path "~/.config/emacs/settings")

;;; Keybindings
;(require 'key-bindings)
(load "~/.config/emacs/settings/key-bindings.el")

;;; Autopair
;(require 'autopair-settings)
(load "~/.config/emacs/settings/autopair-settings.el")

;;; Centaur Tabs
;(require 'centaur-tabs-settings)
(load "~/.config/emacs/settings/centaur-tabs-settings.el")

;;; Conda
;(require 'conda-settings)
(load "~/.config/emacs/settings/conda-settings.el")

;;; Flyspell
;(require 'flyspell-settings)
(load "~/.config/emacs/settings/flyspell-settings.el")

;;; EBIB
;(require 'ebib-settings)
(load "~/.config/emacs/settings/ebib-settings.el")

;;; EIN
;(require 'ein-settings)
(load "~/.config/emacs/settings/ein-settings.el")

;;; ESS
;(require 'ess-settings)
(load "~/.config/emacs/settings/ess-settings.el")

;;; Highlight Indents
;(require 'highlight-indent-guides-settings)
(load "~/.config/emacs/settings/highlight-indent-guides-settings.el")

;;; Highlight Parenthesis
;(require 'highlight-parentheses-settings)
(load "~/.config/emacs/settings/highlight-parentheses-settings.el")

;;; Keychain Environment
;(require 'keychain-envinronment-settings)
(load "~/.config/emacs/settings/keychain-environment-settings.el")

;;; JS2 Environment
;(require 'js2-settings)
(load "~/.config/emacs/settings/js2-settings.el")

;;; Latex
;(require 'latex-settings)
(load "~/.config/emacs/settings/latex-settings.el")

;;; Literate Calc Mode
;(require 'latex-settings)
(load "~/.config/emacs/settings/literate-calc-settings.el")

;;; Magit
;(require 'magit-settings)
(load "~/.config/emacs/settings/magit-settings.el")

;;; Markdown
;(require 'markdown-settings)
(load "~/.config/emacs/settings/markdown-settings.el")

;;; MPDel
;(require 'mpdel-settings)
(load "~/.config/emacs/settings/mpdel-settings.el")

;;; Org-mode
;(require 'org-mode-settings)
(load "~/.config/emacs/settings/org-mode-settings.el")

;;; Org-capture
;(require 'org-capture-settings)
(load "~/.config/emacs/settings/org-capture-settings.el")

;;; Polymode
;(require 'polymode-settings)
(load "~/.config/emacs/settings/polymode-settings.el")

;;; Powerline
;(require 'powerline-settings)
(load "~/.config/emacs/settings/powerline-settings.el")

;;; Python
;(require 'python-settings)
(load "~/.config/emacs/settings/python-settings.el")

;;; pylint
;(require 'pylint-settings)
;(load "~/.config/emacs/settings/pylint-settings.el")

;;; Rainbow delimiters
;(require 'rainbow-delimiters-settings)
(load "~/.config/emacs/settings/rainbow-delimiters-settings.el")

;;; Themes
;(require "~/.config/emacs/settings/theme-settings.el")
(load "~/.config/emacs/settings/theme-settings.el")


;;; tramp settings
;(require "~/.config/emacs/settings/tramp-settings.el")
(load "~/.config/emacs/settings/tramp-settings.el")

;;; ytdl settings
;(require "~/.config/emacs/settings/ytdl-settings.el")
(load "~/.config/emacs/settings/ytdl-settings.el")

;;; libvterm settings https://github.com/akermu/emacs-libvterm
;(require "~/.config/emacs/settings/libvterm-settings.el")
;(load "~/.config/emacs/settings/libvterm-settings.el")



;;; Split the window and start an R session
;; (split-window-horizontally)
;; (other-window 1)
;; (split-window-vertically)
;; (R)
;; (other-window 1)
;; (term "/bin/zsh")


(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)


;; Set default major mode for *scratch*
(setq initial-major-mode 'python-mode)
