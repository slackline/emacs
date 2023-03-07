;;; init.el --- Emacs configuration
;;; LOAD PACKAGES
;;; --------------------------------------
;; (setq debug-on-error f)
;;
;; Load auto-compile early and ensure auto-compile-on-load-mode/auto-compile-on-save-mode are enabled
;; This will byte-compile any existing lisp that is _already_ byte-copmiled (i.e. there is .elc version)
;; of the file.
(setq load-prefer-newer t)
(package-initialize)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(require 'package)

;; Adding repositories along with priority https://emacs.stackexchange.com/a/2989/10100
(setq package-archives
      '(("GNU ELPA"	. "https://elpa.gnu.org/packages/")
        ("NonGNU ELPA"  . "https://elpa.nongnu.org/nongnu/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"	. "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA" . 10)
        ("GNU ELPA"	. 5)
        ("NonGNU ELPA"	. 5)
        ("MELPA Stable"	. 0)))
;; On some systems we have problems communicating with ELPA (https://emacs.stackexchange.com/a/62210)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add local lisp for miscellaneous things
;; (add-to-list 'load-path "~/.config/emacs/lisp/")

;; Modify exec-path
(setq exec-path (append '("~/bin"
                          "~/.local/bin"
                          "~/.cargo/bin/")
                        exec-path))

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

;; Hide compilation buffer https://emacs.stackexchange.com/a/110
(add-hook 'compilation-finish-functions (lambda (buf strg) (kill-buffer buf)))


;; SETUP use-package, will install if not already present
;;   https://ianyepan.github.io/posts/setting-up-use-package/
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Consider switching to elpaca
;; https://github.com/progfolio/elpaca
;; https://www.reddit.com/r/emacs/comments/11daqsz/elpaca_the_basics/

;; BASIC CUSTOMISATION
;; --------------------------------------
(use-package emacs
	     :init
	     (tool-bar-mode -1)
	     (scroll-bar-mode -1)
	     (menu-bar-mode 1)
	     (global-linum-mode t)
	     (global-hl-line-mode 1)
	     (savehist-mode 1)
	     (recentf-mode 1)
	     (global-auto-revert-mode 1)
	     :config
	     ;; Add local lisp for miscellaneous things
	     (add-to-list 'load-path "~/.config/emacs/lisp/") ; Local LISP
	     (setq inhibit-startup-message t)    ; hide the startup message
	     (setq global-visual-line-mode t)    ; Visual line wrap
	     (setq inhibit-startup-screen t)     ; Disable startup screen
	     (setq initial-scratch-message "")   ; Make *scratch* buffer blank
	     (setq confirm-kill-processes nil)   ; Stop confirming the killing of processes
	     (setq ring-bell-function 'ignore)   ; Disable bell sound
	     (setq global-auto-revert-non-file-buffers t) ; Update non-file buffers (Dired) when disk changes
	     (setq use-dialog-box nil)           ; No dialog pop-ups
	     (setq history-length 100)           ; Mini-buffer history
	     (setq-default fill-column 120)      ; Reset line-length
	     (setq undo-limit 320000)            ; Increase the undo history limits
             (setq vc-follow-symlinks t)         ; open source of symlink maintain vc (https://stackoverflow.com/a/30900018/1444043)
	     (setq undo-strong-limit 640000)
             (setq mode-line-compact t)
	     (setq-default indent-tabs-mode nil)
	     (setq-default tab-width 4)
	     (setq-default sh-basic-offset 2)
	     (setq-default sh-indentation 2)
	     (setq-default cursor-type 'bar)     ; Line-style cursor similar to other text editors
             ;; (set-cursor-color "#62088A") ; Dark purple (not very visible)
             (set-cursor-color "#0AFF00") ; Bright Green (stands out better)
	     (setq-default frame-title-format '("%f"))     ; Make window title the buffer name
	     :bind (("C-c U" . revert-buffer)
		    ("C-c D" . toggle-debug-on-error)
		    ;; Org
		    ("\C-cl" . org-store-link)
		    ("\C-cc" . org-capture)
		    ("\C-ca" . org-agenda)
		    ("\C-cb" . org-iswitchb)
		    ("C-x p i" . org-org-cliplink) ;; From : https://github.com/rexim/org-cliplink
		    ;; Magit /code review
		    ("C-x g" . magit-status)
		    ("C-c P" . magit-push-current-to-upstream)
		    ("C-c F" . magit-pull)
		    ("C-c R" . code-review-forge-pr-at-point))
	     :hook
	     ((latex-mode
	       markdown-mode
	       org-mode
	       prog-mode
	       text-mode) . auto-fill-mode)
	     (auto-fill-function . do-auto-fill)
	     (before-save . delete-trailing-whitespace) ;; https://emacs.stackexchange.com/a/40773/10100
	     (prog-mode-hook . highlight-indent-guides-mode)
	     )

;; Set httpd-system-name for IP based on system-name, used by simple-httpd config (see settings/elfeed-settings.el)
;; https://emacs.stackexchange.com/a/41726/10100
(setq httpd-system-name
      '(("kimura" . "127.0.0.1")
        ("vps-bb669593" . "152.228.170.148")
        ("alarmpi-4b" . "127.0.0.1")
        ("fisher" . "127.0.0.1")
        ("Neils-MBP.lan" . "127.0.0.1")
        ("haldane" . "127.0.0.1")
        ("mendel" . "127.0.0.1")))

;; Uppercase/Lowercase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Hook to make scripts executable on saving
;; https://emacsredux.com/blog/2021/09/29/make-script-files-executable-automatically/
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;; Reload a buffer from disk
;; Source: http://www.emacswiki.org/emacs/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; Set the frames title to be that of the currently visited buffer
;; (setq frame-title-format "%b")

;; PACKAGE SPECIFIC CONFIGURATION
;; --------------------------------------
;; Splitting settings into individual files as this has become monolithic and unnavigable
;; Path where settings files are kept
;; (add-to-list 'load-path "~/.config/emacs/settings")

;;; Shell Interpreter (has to be called early so that commands are available for hooks)
(load "~/.config/emacs/settings/shell-interpreter-settings.el")
(load "~/.config/emacs/settings/misc.el")

;;; General editor settings
(load "~/.config/emacs/settings/auto-package-update.el")
(load "~/.config/emacs/settings/centaur-tabs-settings.el")
(load "~/.config/emacs/settings/highlight-indent-guides-settings.el")
(load "~/.config/emacs/settings/highlight-parentheses-settings.el")
(load "~/.config/emacs/settings/rainbow-delimiters-settings.el")
(load "~/.config/emacs/settings/smartparens-settings.el")
(load "~/.config/emacs/settings/tree-sitter-settings.el")

;;; Python
(load "~/.config/emacs/settings/conda-settings.el")
(load "~/.config/emacs/settings/ein-settings.el")
(load "~/.config/emacs/settings/python-settings.el")

;;; Org / Bib
(load "~/.config/emacs/settings/ebib-settings.el")
(load "~/.config/emacs/settings/org-mode-settings.el")
(load "~/.config/emacs/settings/org-ref-settings.el")
(load "~/.config/emacs/settings/org-babel-settings.el")
(load "~/.config/emacs/settings/org-notifications-settings.el")
(load "~/.config/emacs/settings/org-roam-settings.el")
(load "~/.config/emacs/settings/org-gtd-settings.el")
(load "~/.config/emacs/settings/org-capture-settings.el")

;;; Misc
(load "~/.config/emacs/settings/keychain-environment-settings.el")
(load "~/.config/emacs/settings/literate-calc-settings.el")
(load "~/.config/emacs/settings/mastodon-settings.el")
(load "~/.config/emacs/settings/mpd-settings.el")
(load "~/.config/emacs/settings/osm-settings.el")
(load "~/.config/emacs/settings/tramp-settings.el")
(load "~/.config/emacs/settings/vterm-settings.el")
;; (load "~/.config/emacs/settings/weblorg-settings.el")
;; (load "~/.config/emacs/settings/ytdl-settings.el")

;;; IDE / LSP
(load "~/.config/emacs/settings/fly-settings.el")
(load "~/.config/emacs/settings/jedi-settings.el")
(load "~/.config/emacs/settings/js-settings.el")
(load "~/.config/emacs/settings/format-all-settings.el")
(load "~/.config/emacs/settings/latex-settings.el")
(load "~/.config/emacs/settings/yaml-settings.el")
(load "~/.config/emacs/settings/lsp-settings.el")
;;(load "~/.config/emacs/settings/eglot-settings.el")
(load "~/.config/emacs/settings/markdown-settings.el")
(load "~/.config/emacs/settings/polymode-settings.el")
(load "~/.config/emacs/settings/ess-settings.el")
(load "~/.config/emacs/settings/magit-settings.el")


;;; Themes
(load "~/.config/emacs/settings/powerline-settings.el")
(load "~/.config/emacs/settings/theme-settings.el")

;;; Keybindings (last so that all functions are defined)
(load "~/.config/emacs/settings/key-bindings.el")

;;; Split the window and start an R session
;; (split-window-horizontally)
;; (other-window 1)
;; (split-window-vertically)
;; (R)
;; (other-window 1)
;; (term "/bin/zsh")


(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; Start elfeed last
(load "~/.config/emacs/settings/elfeed-settings.el")

;; Set default major mode for *scratch*
;; (setq initial-major-mode 'python-mode)
