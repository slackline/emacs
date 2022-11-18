;; KEY BINDINGS
;; --------------------------------------
;; List keybindings for (see https://emacs.stackexchange.com/q/732)...
;;
;; Buffer C-h b
;; Mode   C-h m
;;
;; Other options are...
;;
;; helm-descbinds
;; which-key
;; guide-key
;;
;; Most things commented out here reside under init.el > (use-package emacs :bind (...))
(use-package which-key
	     :config (which-key-mode))

;; helpful settings
;; https://github.com/Wilfred/helpful
;;
(use-package helpful
	     :config
	     ;; Note that the built-in `describe-function' includes both functions
	     ;; and macros. `helpful-function' is functions only, so we provide
	     ;; `helpful-callable' as a drop-in replacement.
	     (global-set-key (kbd "C-h f") #'helpful-callable)
	     (global-set-key (kbd "C-h v") #'helpful-variable)
	     (global-set-key (kbd "C-h k") #'helpful-key)
	     ;; Lookup the current symbol at point. C-c C-d is a common keybinding
	     ;; for this in lisp modes.
	     (global-set-key (kbd "C-c C-d") #'helpful-at-point)

	     ;; Look up *F*unctions (excludes macros).
	     ;;
	     ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
	     ;; already links to the manual, if a function is referenced there.
	     (global-set-key (kbd "C-h F") #'helpful-function)

	     ;; Look up *C*ommands.
	     ;;
	     ;; By default, C-h C is bound to describe `describe-coding-system'. I
	     ;; don't find this very useful, but it's frequently useful to only
	     ;; look at interactive functions.
	     (global-set-key (kbd "C-h C") #'helpful-command)
	     )
;; Misc
;; (global-set-key (kbd "C-c U") 'revert-buffer)
;; (global-set-key (kbd "C-c e") 'eval-region)
;; (global-set-key (kbd "C-c E") 'eval-buffer)
;; (global-set-key (kbd "C-c D") 'toggle-debug-on-error)
;; Magit
;; (global-set-key (kbd "C-x g") 'magit-status)
;; (global-set-key (kbd "C-c P") 'magit-push-current-to-upstream)

;; Org-mode
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
;; Cliplink setup
;; From : https://github.com/rexim/org-cliplink
;; (global-set-key (kbd "C-x p i") 'org-cliplink)

;; Keychain
;; (global-set-key (kbd "C-c k") 'keychain-refresh-environment)

;; Custom commands
;; (global-set-key (kbd "C-c r") 'rsync-html)
;; (global-set-key (kbd "<f1>") )
;; (global-set-key (kbd "<f2>") )
;; (global-set-key (kbd "<f3>") )
;; (global-set-key (kbd "<f4>") )
;; (global-set-key (kbd "<f5>") )
;; (global-set-key (kbd "<f6>") )
;; (global-set-key (kbd "<f7>") )
;; (global-set-key (kbd "<f8>") )
;; (global-set-key (kbd "<f9>") )
;; (global-set-key (kbd "<f10>") )
;; (global-set-key (kbd "<f11>") )
;; (global-set-key (kbd "<f12>") )

;;; Some generally useful key-bindings (mostly ESS specific) from
;;; http://stats.blogoverflow.com/page/2/
;; (define-key global-map [f1] 'Control-X-prefix)
;; (define-key global-map [f2] 'save-buffer)
;; (define-key global-map [f3] 'find-file)
;; (define-key global-map [f5] 'switch-to-buffer)
;; (define-key global-map [f6] 'other-window)
;; (define-key global-map [f8] 'kill-buffer)
;; (define-key global-map [f9] 'ess-load-file)

;; EIN commands
(local-set-key [f12] 'ein:worksheet-delete-cell)
(local-set-key [f5] 'ein:notebook-reconnect-kernel)

;; Pass
(global-set-key (kbd "<f1>") 'password-store-copy)
(global-set-key (kbd "<f2>") 'eval-buffer)
(global-set-key (kbd "<f3>") 'eval-region)
(global-set-key (kbd "<f4>") 'package-list-packages)
(global-set-key (kbd "<f5>") 'keychain-refresh-environment)
