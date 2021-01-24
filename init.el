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
;; Remove toolbar, never use it
(tool-bar-mode -1)

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

;;; Shell Interpreter (has to be called early so that commands are available for hooks)
(load "~/.config/emacs/settings/shell-interpreter-settings.el")

;;; Keybindings
(load "~/.config/emacs/settings/key-bindings.el")

;;; Autopair
(load "~/.config/emacs/settings/autopair-settings.el")

;;; Centaur Tabs
(load "~/.config/emacs/settings/centaur-tabs-settings.el")

;;; Conda
(load "~/.config/emacs/settings/conda-settings.el")

;;; Flyspell
(load "~/.config/emacs/settings/flyspell-settings.el")

;;; EBIB
(load "~/.config/emacs/settings/ebib-settings.el")

;;; EIN
(load "~/.config/emacs/settings/ein-settings.el")

;;; EIN
(load "~/.config/emacs/settings/elfeed-settings.el")

;;; ESS
(load "~/.config/emacs/settings/ess-settings.el")

;;; Highlight Indents
(load "~/.config/emacs/settings/highlight-indent-guides-settings.el")

;;; Highlight Parenthesis
(load "~/.config/emacs/settings/highlight-parentheses-settings.el")

;;; Highlight Parenthesis
(load "~/.config/emacs/settings/jedi-settings.el")

;;; JS Environment
(load "~/.config/emacs/settings/js-settings.el")

;;; Keychain Environment
(load "~/.config/emacs/settings/keychain-environment-settings.el")

;;; Latex
(load "~/.config/emacs/settings/latex-settings.el")

;;; Literate Calc Mode
(load "~/.config/emacs/settings/literate-calc-settings.el")

;;; LSP Mode
(load "~/.config/emacs/settings/lsp-settings.el")

;;; Magit
(load "~/.config/emacs/settings/magit-settings.el")

;;; Markdown
(load "~/.config/emacs/settings/markdown-settings.el")

;;; MPDel
(load "~/.config/emacs/settings/mpdel-settings.el")

;;; Org-mode
(load "~/.config/emacs/settings/org-mode-settings.el")

;;; Org-capture
(load "~/.config/emacs/settings/org-capture-settings.el")

;;; Polymode
(load "~/.config/emacs/settings/polymode-settings.el")

;;; Powerline
(load "~/.config/emacs/settings/powerline-settings.el")

;;; Python
(load "~/.config/emacs/settings/python-settings.el")
;; (load "~/.config/emacs/settings/python-lsp-settings.el")

;;; pylint
;(load "~/.config/emacs/settings/pylint-settings.el")

;;; Rainbow delimiters
(load "~/.config/emacs/settings/rainbow-delimiters-settings.el")

;;; Themes
(load "~/.config/emacs/settings/theme-settings.el")

;;; tramp settings
(load "~/.config/emacs/settings/tramp-settings.el")

;;; vterm settings
(load "~/.config/emacs/settings/vterm-settings.el")

;;; ytdl settings
(load "~/.config/emacs/settings/ytdl-settings.el")




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
;; (setq initial-major-mode 'python-mode)
