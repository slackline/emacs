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
;;
;; Simplified Emacs keymap in Emacs 29 (and earlier!) : https://systemcrafters.net/newsletter/sc-news-004.html
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :defer 0.5
  :config (which-key-mode)
  (global-set-key (kbd "<f8>") 'which-key-show-major-mode))

;; helpful settings
;; https://github.com/Wilfred/helpful
;;
(use-package helpful
  :defer 0.5
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

;; mwim settings
;; https://github.com/alezost/mwim.el
(use-package mwim
  :defer 0.5
  :conf
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
  (global-set-key (kbd "<home>") 'mwim-beginning-of-line-or-code)
  (global-set-key (kbd "<end>") 'mwim-end-of-line-or-code))

;; Function Keys
(global-set-key (kbd "<f1>") 'password-store-copy)
(global-set-key (kbd "<f2>") 'eval-buffer)
(global-set-key (kbd "<f3>") 'eval-region)
(global-set-key (kbd "<f4>") 'package-list-packages)
(global-set-key (kbd "<f5>") 'keychain-refresh-environment)
(global-set-key (kbd "<f6>") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "<f7>") 'flyspell-toggle)
(global-set-key (kbd "<f8>") 'which-key-show-major-mode)
(global-set-key (kbd "C-M-%") 'query-replace-regexp)
(if (system-name) "kimura" (global-set-key (kbd "<XF86HomePage>") 'osm-home))

;; Toggle comments https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
(global-set-key (kbd "C-c C") 'comment-or-uncomment-region)

;; ibuffer https://codeberg.org/anonimno/emacs/src/branch/master/config.org#headline-20
;; C-x C-b until you internalise it!
(global-set-key [remap list-buffers] 'ibuffer)

;; EIN global commands to start and stop Jupyter server
(global-set-key (kbd "C-c e R") 'ein:run)
(global-set-key (kbd "C-c e S") 'ein:stop)

;; Magit
(global-set-key (kbd "C-c m") (define-keymap :prefix 'my/org-key-map
                                "C" 'magit-clone
                                "d" 'nds:magit-show-with-difftastic
                                "F" 'magit-pull-from-upstream
                                "f" 'forge-pull
                                "L" 'magit-log
                                "P" 'magit-push-current-to-upstream
                                "R" 'magit-file-rename))
(global-set-key (kbd "C-c m F") 'magit-pull)
(global-set-key (kbd "C-c m L") 'magit-log)
(global-set-key (kbd "C-c m P") 'magit-push-current-to-upstream)
(global-set-key (kbd "C-c m d") 'nds:magit-show-with-difftastic)
(global-set-key (kbd "C-c m R") 'magit-file-rename)
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
