;; KEY BINDINGS
;; ------------
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
;; https://github.com/justbur/emacs-which-key
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
	     (global-set-key (kbd "C-h C") #'helpful-command))


;; Function Keys
(global-set-key (kbd "<f1>") 'password-store-copy)
(global-set-key (kbd "<f2>") 'eval-buffer)
(global-set-key (kbd "<f3>") 'eval-region)
(global-set-key (kbd "<f4>") 'package-list-packages)
(global-set-key (kbd "<f5>") 'keychain-refresh-environment)
(if (system-name) "kimura" (global-set-key (kbd "<XF86HomePage>") 'osm-home))

;; EIN commands
(global-set-key (kbd "C-c C-e r") 'ein:run)
(global-set-key (kbd "C-c C-e s") 'ein:stop)
(local-set-key (kbd "<f10>") 'ein:notebook-reconnect-kernel)
(local-set-key (kbd "<f11>") 'ein:worksheet-delete-cell)
(local-set-key (kbd "C-c C-e r") 'ein:worksheet-execute-all-cells)
(local-set-key (kbd "C-c C-e x") 'ein:notebook-reconnect-kernel)
(local-set-key (kbd "C-c C-e C-r u") 'ein:worksheet-execute-all-cells-above)
(local-set-key (kbd "C-c C-e C-r b") 'ein:worksheet-execute-all-cells-above)

;; Magit
(global-set-key (kbd "C-c m F") 'magit-pull)
(global-set-key (kbd "C-c m L") 'magit-log)
(global-set-key (kbd "C-c m P") 'magit-push-current-to-upstream)
(global-set-key (kbd "C-c m d") 'nds:magit-show-with-difftastic)

;; Python
(global-set-key (kbd "C-c p v") 'pyvenv-workon)
(global-set-key (kbd "C-c p p") 'run-python)

;; Miscellaneous
(global-set-key (kbd "C-c k") 'keychain-refresh-environment)
(global-set-key (kbd "C-c u") 'rsync-html)
(global-set-key (kbd "C-c C-r") 'revert-buffer-no-confirm)
;;; Some generally useful key-bindings (mostly ESS specific) from
;;; http://stats.blogoverflow.com/page/2/
;; (define-key global-map [f1] 'Control-X-prefix)
;; (define-key global-map [f2] 'save-buffer)
;; (define-key global-map [f3] 'find-file)
;; (define-key global-map [f5] 'switch-to-buffer)
;; (define-key global-map [f6] 'other-window)
;; (define-key global-map [f8] 'kill-buffer)
;; (define-key global-map [f9] 'ess-load-file)
