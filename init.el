;;; Commentary: --- Emacs configuration
;;;
;;; LOAD PACKAGES
;;; --------------------------------------
;; (setq debug-on-error f)
;;
(setq package-enable-at-startup nil)
(package-initialize)
(require 'package)
;; SETUP use-package, will install if not already present
;;   https://ianyepan.github.io/posts/setting-up-use-package/
(use-package use-package
  :config
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t))

;; On some systems we have problems communicating with ELPA (https://emacs.stackexchange.com/a/62210)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Adding repositories along with priority https://emacs.stackexchange.com/a/2989/10100
(setq package-archives
      '(("GNU ELPA"	. "https://elpa.gnu.org/packages/")
        ("NonGNU ELPA"  . "https://elpa.nongnu.org/nongnu/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"	. "https://melpa.org/packages/")
        ("jcs elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))
      package-archive-priorities
      '(("MELPA" . 10)
        ("GNU ELPA"	. 5)
        ("NonGNU ELPA"	. 5)
        ("MELPA Stable"	. 3)
        ("jcs elpa" . 0)
        ))

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; Add local lisp for miscellaneous things
;; (add-to-list 'load-path "~/.config/emacs/lisp/")

;; Load auto-compile early and ensure auto-compile-on-load-mode/auto-compile-on-save-mode are enabled
;; This will byte-compile any existing lisp that is _already_ byte-copmiled (i.e. there is .elc version)
;; of the file.
;; (setq load-prefer-newer t)
;; (setq native-comp-jit-compilation t)
;; (use-package auto-compile)
;; https://github.com/jamescherti/compile-angel.el
;; NB - This will fail on fresh installs as you have to first install use-package!
;; (use-package compile-angel
;;   :ensure t
;;   :demand t
;;   :custom
;;   (compile-angel-verbose nil)
;;   :config
;;   (compile-angel-on-load-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

;; no-littering (for when switched to 29.1) https://github.com/emacscollective/no-littering

;; Modify exec-path
(setq exec-path (append '("~/bin"
                          "~/.local/bin"
                          "~/.cargo/bin/"
                          "~/.node/bin/"
                          )
			exec-path))
;; https://emacs.stackexchange.com/questions/17866/
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (daemonp) (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AGENT_SOCK"))

;; Load and install mypackages
;; (load "~/.config/emacs/settings/mypackages.el")

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

;; Reinstall a package https://emacsredux.com/blog/2020/09/12/reinstalling-emacs-packages/
(defun ns/reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

;; https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
(use-package gnu-elpa-keyring-update
  :ensure t)

;; Load my custom functions
;; (load "~/.config/emacs/settings/custom-funcs.el")
;; Consider switching to elpaca
;; https://github.com/progfolio/elpaca
;; https://www.reddit.com/r/emacs/comments/11daqsz/elpaca_the_basics/

;; BASIC CUSTOMISATION
;; --------------------------------------
(use-package emacs
  :init
  (epa-file-enable)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (savehist-mode 1)
  (recentf-mode 1)
  (global-auto-revert-mode 1)
  :config
  ;; Add local lisp for miscellaneous things
  (add-to-list 'load-path "~/.config/emacs/elpa/") ; Local LISP
  (add-to-list 'load-path "~/.config/emacs/lisp/") ; Local LISP
  (setq custom-file "~/.config/emacs/custom.el")
  (setq package-install-upgrade-built-in t) ; Upgrade built-in packages
  (setq enable-recursive-minibuffers t) ; Vertico - open new minibuffers from inside a minibuffer
  (setq inhibit-startup-message t) ; Hide the startup message
  (setq global-visual-line-mode t) ; Visual line wrap
  (setq inhibit-startup-screen t) ; Disable startup screen
  (setq initial-scratch-message "") ; Make *scratch* buffer blank
  (setq confirm-kill-processes nil) ; Stop confirming the killing of processes
  (setq ring-bell-function 'ignore)  ; Disable bell sound
  (setq global-auto-revert-non-file-buffers t) ; Update non-file buffers (Dired) when disk changes
  (setq use-dialog-box nil) ; No dialog pop-ups
  (setq history-length 1000) ; Mini-buffer history
  (setq-default fill-column 120) ; Reset line-length
  (setq undo-limit 320000) ; Increase the undo history limits
  (setq vc-follow-symlinks t) ; open source of symlink maintain vc (https://stackoverflow.com/a/30900018/1444043)
  (setq winner-mode t) ; toggling window configuration
  (setq initial-scratch-message nil)
  (setq pixel-scroll-precision-mode t)
  (setq lisp-indent-offset 2)
  (setq undo-strong-limit 640000)
  (setq mode-line-compact t)
  (setq dired-dwim-target t) ; move file to other pane as default destination
  (setq global-goto-address-mode t)
  ;; (setq browse-url-browser-function 'eww-browse-url) ; Set eww as the default browser
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default sh-basic-offset 2)
  (setq-default sh-indentation 2)
  (setq-default cursor-type 'bar)     ; Line-style cursor similar to other text editors
  ;; (set-cursor-color "#62088A") ; Dark purple (not very visible)
  (set-cursor-color "#0AFF00") ; Bright Green (stands out better)
  (setq-default frame-title-format '("%f"))     ; Make window title the buffer name
  (setopt dictionary-server "dict.org")
  ;; Turn off package install warnings https://codeberg.org/jcastp/emacs.d/src/branch/main/emacs-config.org#headline-16
  ;; (when (and (fboundp 'native-comp-available-p)
  ;;         (native-comp-available-p))
  ;;   (setq native-comp-async-report-warnings-errors nil
  ;;     native-comp-deferred-compilation t))
  (set-frame-parameter nil 'alpha-background 85) ; Transparency
  (add-to-list 'default-frame-alist '(alpha-background . 85))
  ;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
		 (display-buffer-no-window)
		 (allow-no-window . t)))
  :bind (("C-c U" . revert-buffer)
	 ("C-c D" . toggle-debug-on-error)
	 ;; Org
	 ("\C-c l" . org-store-link)
	 ("\C-c c" . org-capture)
	 ("\C-c a" . org-agenda)
	 ("\C-c b" . org-iswitchb)
	 ("C-x p i" . org-org-cliplink) ;; From : https://github.com/rexim/org-cliplink
	 ;; Magit /code review
	 ("C-x g" . magit-status)
	 ("C-c R" . code-review-forge-pr-at-point)
	 ("s-SPC" . cycle-spacing))
  :hook
  ((latex-mode
    markdown-mode
    org-mode
    prog-mode
    text-mode) . auto-fill-mode)
  ((latex-mode
    prog-mode) . hs-minor-mode)
  (compilation-finish-functions . (lambda (buf strg) (kill-buffer buf))) ;; https://emacs.stackexchange.com/a/110
  (auto-fill-function . do-auto-fill)
  (before-save . delete-trailing-whitespace) ;; https://emacs.stackexchange.com/a/40773/10100
  (before-save . do-auto-fill)
  (dired-mode-hook . auto-revert-mode) ; auto refresh dired when files change
  ;; imenu http://yummymelon.com/devnull/til-imenu.html
  ((markdown-mode
    makefile-mode
    prog-mode) . imenu-add-menubar-index)
  ((markdown-mode
    makefile-mode
    prog-mode) . (lambda () (setq imenu-auto-rescan t))))

;; Set httpd-system-name for IP based on system-name, used by simple-httpd config (see settings/elfeed-settings.el)
;; https://emacs.stackexchange.com/a/41726/10100
(setq httpd-system-name
      '(("kimura" . "127.0.0.1")
        ("crow" . "127.0.0.1")
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

;; Insert rectangle and push lines down (https://emacs.stackexchange.com/a/46352/10100)
(defun my-insert-rectangle-push-lines ()
  "Yank a rectangle as if it was an ordinary kill."
  (interactive "*")
  (when (and (use-region-p) (delete-selection-mode))
    (delete-region (region-beginning) (region-end)))
  (save-restriction
    (narrow-to-region (point) (mark))
    (yank-rectangle)))
(global-set-key (kbd "C-x r C-y") #'my-insert-rectangle-push-lines)

;; Reload a buffer from disk
;; Source: http://www.emacswiki.org/emacs/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; Set the frames title to be that of the currently visited buffer
;; (setq frame-title-format "%b")

;; https://github.com/wbolster/emacs-direnv
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;; Add global keymaps (see https://emacs.stackexchange.com/a/54792/10100)
(define-key global-map (kbd "C-c p l") (make-sparse-keymap))

;; SSH agency
(load "~/.config/emacs/settings/ssh-agency.el")

;; PACKAGE SPECIFIC CONFIGURATION
;; --------------------------------------
;; Splitting settings into individual files as this has become monolithic and unnavigable
;; Path where settings files are kept
;; (add-to-list 'load-path "~/.config/emacs/settings")
(load "~/.config/emacs/settings/ssh-agency.el")

;;; Completion framework (vertico/marginalia/orderless)
(load "~/.config/emacs/settings/completion.el")

;;; Shell Interpreter (has to be called early so that commands are available for hooks)
(load "~/.config/emacs/settings/shell-interpreter.el")
(load "~/.config/emacs/settings/misc.el")
(load "~/.config/emacs/settings/modes.el")

;;; General editor settings
;;(load "~/.config/emacs/settings/activities.el")
(load "~/.config/emacs/settings/regex.el")
(load "~/.config/emacs/settings/auto-package-update.el")
(load "~/.config/emacs/settings/centaur-tabs.el")
(load "~/.config/emacs/settings/dired.el")
(load "~/.config/emacs/settings/encryption.el")
(load "~/.config/emacs/settings/emacs-everywhere.el")
;; (load "~/.config/emacs/settings/kitty.el")
(load "~/.config/emacs/settings/pass.el")
(load "~/.config/emacs/settings/projectile.el")
(load "~/.config/emacs/settings/tree-sitter.el")
(load "~/.config/emacs/settings/shackle.el")
;; (load "~/.config/emacs/settings/casual-suite.el")
;; (load "~/.config/emacs/settings/ready-player.el")

;;; Org / Bib
(load "~/.config/emacs/settings/ebib.el")
(load "~/.config/emacs/settings/org-babel.el")
(load "~/.config/emacs/settings/org-mode.el")
(load "~/.config/emacs/settings/org-ref.el")
(load "~/.config/emacs/settings/org-notifications.el")
(load "~/.config/emacs/settings/org-roam.el")
(load "~/.config/emacs/settings/org-gtd.el")
(load "~/.config/emacs/settings/org-capture.el")

;;; Chat
;; (load "~/.config/emacs/settings/messaging.el")

;;; Terminals
(load "~/.config/emacs/settings/eat.el")
(load "~/.config/emacs/settings/vterm.el")

;;; Misc
;; (load "~/.config/emacs/settings/exercism.el")
;; (load "~/.config/emacs/settings/literate-calc.el")
(load "~/.config/emacs/settings/mpd.el")
(load "~/.config/emacs/settings/osm.el")
(load "~/.config/emacs/settings/pdf-tools.el")
(load "~/.config/emacs/settings/scratch.el")
(load "~/.config/emacs/settings/tmr.el")
(load "~/.config/emacs/settings/tramp.el")
;; (load "~/.config/emacs/settings/weblorg.el")
;; (load "~/.config/emacs/settings/ytdl.el")

;;; IDE / LSP
(load "~/.config/emacs/settings/just-mode.el")
(load "~/.config/emacs/settings/polymode.el")
(load "~/.config/emacs/settings/markdown.el")
(load "~/.config/emacs/settings/python.el")
;; (load "~/.config/emacs/settings/conda.el")
;; (load "~/.config/emacs/settings/eglot.el")
(load "~/.config/emacs/settings/ess.el")
(load "~/.config/emacs/settings/fly.el")
;; (load "~/.config/emacs/settings/format-all.el")
(load "~/.config/emacs/settings/jedi.el")
(load "~/.config/emacs/settings/js.el")
(load "~/.config/emacs/settings/latex.el")
;; (load "~/.config/emacs/settings/lsp.el")
;; (load "~/.config/emacs/settings/lsp-start-plain.el")
(load "~/.config/emacs/settings/lsp-sample.el")
(load "~/.config/emacs/settings/magit.el")
(load "~/.config/emacs/settings/mermaid.el")
;; (load "~/.config/emacs/settings/nix.el")
;; (load "~/.config/emacs/settings/realgud.el")
;; (load "~/.config/emacs/settings/rust.el")
(load "~/.config/emacs/settings/sqlite.el")
(load "~/.config/emacs/settings/tempel.el")
;; (load "~/.config/emacs/settings/typst.el")
(load "~/.config/emacs/settings/yaml.el")
(load "~/.config/emacs/settings/yasnippet.el")
;; (load "~/.config/emacs/settings/devdocs.el")

;;; Themes, modeline and buffers
;; (load "~/.config/emacs/settings/modeline.el")
(load "~/.config/emacs/settings/popper.el")
(load "~/.config/emacs/settings/theme.el")

;;; Keybindings (last so that all functions are defined)
(load "~/.config/emacs/settings/key-bindings.el")
;; (load "~/.config/emacs/settings/pulsar.el")

;;; Split the window and start an R session
;; (split-window-horizontally)
;; (other-window 1)
;; (split-window-vertically)
;; (R)
;; (other-window 1)
;; (term "/bin/zsh")



;; (load custom-file)

;; Start elfeed last
;; (load "~/.config/emacs/settings/elfeed.el")


;; Set default major mode for *scratch*
;; (setq initial-major-mode 'python-mode)
(put 'scroll-left 'disabled nil)
