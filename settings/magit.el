;; MAGIT CONFIGURATION
;; --------------------------------------
;; https://magit.vc/
(use-package magit
  :ensure t
  :init
  (setq magit-repository-directories
	`(("~/dotfiles" . 1)
	  ("~/.config/emacs/" . 1)
	  ("~/org/" . 1)
	  ("~/work/git/" . 2)
	  ("~/work/python/tcx2gpx/" . 1)
	  ("~/work/python/wpweather/" . 1)
	  ))
  (setq auth-sources '("~/.authinfo.gpg"))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  ;; https://www.reddit.com/r/emacs/comments/17af1q5/opening_magit_fullframe_then_restoring_windows_on/
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq magit-pull-or-fetch t)
  (global-set-key (kbd "C-c m f") 'forge-pull))

;; FORGE CONFIGURATION
;; --------------------------------------
;; https://magit.vc/manual/ghub.html#Getting-Started
;; https://magit.vc/manual/forge.html#Getting-Started
;; Unnecessary as
;; (use-package forge
;;   :after magit)


;; Git modes
(use-package git-modes
  :ensure t)

(use-package gh-notify
  :ensure t)
(use-package magit-imerge
  :ensure t
  :after magit)

;; https://github.com/dandavison/magit-delta
(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode))

;; https://codeberg.org/akib/emacs-why-this
(use-package why-this
  :ensure t
  :init
  ;;  (setq global-why-this-mode)
  :config
  (set-face-background 'why-this-annotate-heat-map-cold "#203448")
  (set-face-background 'why-this-annotate-heat-map-warm "#382f27")
  (global-set-key (kbd "C-c m w") 'why-this))

;; https://github.com/sshaw/git-link
(use-package git-link
  :ensure t
  :config
  (global-set-key (kbd "C-c m g l") 'git-link)
  (global-set-key (kbd "C-c m g c") 'git-link-commit)
  (global-set-key (kbd "C-c m g h") 'git-link-homepage))

;; https://github.com/blahgeek/emacs-pr-review
;; (use-package pr-review
;;   :ensure t
;;   :after ghub)

;; https://github.com/wandersoncferreira/code-review/
(use-package code-review
  :ensure t
  :config
  (setq code-review-fill-column 120)
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer)
  :hook
  (code-review-mode . emojify-mode)
  )

;; https://github.com/LionyxML/magit-stats
(use-package magit-stats
  :ensure t)

;; Orgit / Orgit-forge
;;
;; References :
;;
;; https://www.reddit.com/r/emacs/comments/lsr161/wishlist_has_anyone_built_an_orgmode_git_log/
;;
;; Comments : Sets up keybindings 'C-c m [c|v]' for copying and pasting forge links from magit buffers to org-buffers
;; https://github.com/magit/orgit
(use-package orgit
  :after magit)

;; https://github.com/magit/orgit-forge
(use-package orgit-forge
  :after magit
  :bind (:map magit-mode-map
	      ("C-c m c" . orgit-store-link))
  (:map org-mode-map
	("C-c m v" . org-insert-last-stored-link)))

;; emacsql-sqlite-module is required by forge, but not always explicitly pulled in
(use-package emacsql-sqlite-module
  :ensure t
  :after magit)

;; https://codeberg.org/pidu/git-timemachine
(use-package git-timemachine
  :ensure t)


;; https://github.com/pkryger/difftastic.el
(use-package difftastic
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))
;; Replaces old configuration based on https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html


;; Transient prefix
(transient-define-prefix nds:magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("o" "Orgit Link" orgit-store-link)
   ])

;; Transient suffix https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
;; (transient-append-suffix 'magit-dispatch "!"
;;   '("#" "My Magit Cmds" nds:magit-aux-commands))

;; (define-key magit-status-mode-map (kbd "#") #'nds:magit-aux-commands)

;; Treemacs-magit
(use-package treemacs-magit
  :ensure t)

;; Gitlab packages

;; https://gitlab.com/joewreschnig/gitlab-ci-mode/
(use-package gitlab-ci-mode
  :ensure t)

;; https://gitlab.com/joewreschnig/gitlab-ci-mode-flycheck/
(use-package gitlab-ci-mode-flycheck
  :ensure t)


;; https://github.com/liuyinz/git-cliff.el
(use-package git-cliff
  :ensure t)
