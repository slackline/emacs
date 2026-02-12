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
  ;; https://huonw.github.io/blog/2025/12/magit-insert-worktrees/
  ;; Show all worktrees at the end of the status buffer (if more than one)
  (add-hook 'magit-status-sections-hook #'magit-insert-worktrees t)
  ;; https://www.reddit.com/r/emacs/comments/17af1q5/opening_magit_fullframe_then_restoring_windows_on/
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq magit-pull-or-fetch t)
  (setq magit-log-margin '(t "%F %R" magit-log-margin-width t 18))
  ;; Don't want auto-fill-mode enabled for the following modes, probably a smarter way of doing this under :hooks
  ;; perhaps?
  (remove-hook 'git-commit-mode #'turn-on-auto-fill)
  (remove-hook 'forge-post-mode #'turn-on-auto-fill)
  ;; https://mbork.pl/2025-05-12_Coloring_Git_output_in_Magit
  (setq magit-process-finish-apply-ansi-colors t)
  (global-set-key (kbd "C-c m C") 'magit-clone)
  (global-set-key (kbd "C-c m F") 'magit-pull-from-upstream)
  (global-set-key (kbd "C-c m P") 'magit-push-current-to-upstream)
  (global-set-key (kbd "C-c m R") 'magit-file-rename)
  (global-set-key (kbd "C-c m f") 'forge-pull)
  (global-set-key (kbd "C-c m d d") 'nds:magit-show-with-difftastic)
  (global-set-key (kbd "C-c m d r") 'magit-diff-range)
  (global-set-key (kbd "C-c m d s") 'magit-diff-staged)
  (global-set-key (kbd "C-c m l l") 'magit-log)
  (global-set-key (kbd "C-c m l f") 'magit-log-buffer-file)
  (global-set-key (kbd "C-c m l o") 'magit-log-other))

;; Emacs solo magit https://www.rahuljuliato.com/posts/vc-git-functions
;; FORGE CONFIGURATION
;; --------------------------------------
;; https://magit.vc/manual/ghub.html#Getting-Started
;; https://magit.vc/manual/forge.html#Getting-Started
;; https://github.com/mobid/gitea-forge
(use-package forge
  :after magit
  :config
  (push '("forgejo.nshephard.dev"
          "forgejo.nshephard.dev/api/v1"
          "forgejo.nshephard.dev"
          forge-forgejo-repository)
	forge-alist))

;; Git-commit TreeSitter mode
;; https://github.com/danilshvalov/git-commit-ts-mode
;; (use-package git-commit-ts-mode
;;   :mode "\\COMMIT_EDITMSG\\'"
;;   :config
;;   (setq git-commit-major-mode 'git-commit-ts-mode))

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
  ;; (global-why-this-mode)
  :config
  (set-face-background 'why-this-annotate-heat-map-cold "#203448")
  (set-face-background 'why-this-annotate-heat-map-warm "#382f27")
  (global-set-key (kbd "C-c m w") 'why-this))

;; https://github.com/sshaw/git-link
(use-package git-link
  :ensure t
  :config
  (require 'git-link-transient)
  (global-set-key (kbd "C-c m g g") 'git-link-dispatch)
  (global-set-key (kbd "C-c m g l") 'git-link)
  (global-set-key (kbd "C-c m g c") 'git-link-commit)
  (global-set-key (kbd "C-c m g h") 'git-link-homepage))

;; https://github.com/blahgeek/emacs-pr-review
(use-package pr-review
  :ensure t
  :after ghub)

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

;; https://codeberg.org/pidu/git-timemachine
(use-package git-timemachine
  :ensure t)

;; https://gitlab.com/arvidnl/magit-gitlab
(use-package magit-gitlab
  :ensure t)

;; https://github.com/pkryger/difftastic.el
(use-package difftastic
  :ensure t
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


;; https://github.com/armindarvish/consult-gh
;; (use-package consult-gh
;;   :ensure t
;;   :after consult
;;   :custom
;;   (consult-gh-default-clone-directory "~/projects")
;;   (consult-gh-show-preview t)
;;   (consult-gh-preview-key "C-o")
;;   (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
;;   (consult-gh-issue-action #'consult-gh--issue-view-action)
;;   (consult-gh-pr-action #'consult-gh--pr-view-action)
;;   (consult-gh-code-action #'consult-gh--code-view-action)
;;   (consult-gh-file-action #'consult-gh--files-view-action)
;;   (consult-gh-notifications-action #'consult-gh--notifications-action)
;;   (consult-gh-dashboard-action #'consult-gh--dashboard-action)
;;   (consult-gh-large-file-warning-threshold 2500000)
;;   (consult-gh-prioritize-local-folder 'suggest)
;;   :config
;;   ;; (setq consult-gh-default-orgs-list (consult-gh--get-current-orgs t))

;;   ;; Add a hook to change default organizations when the account is switched
;;   (add-hook 'consult-gh-auth-post-switch-hook (lambda (&rest args) (setq consult-gh-default-orgs-list (consult-gh--get-current-orgs t))))

;;   ;; Remember visited orgs and repos across sessions
;;   (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
;;   (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list))


;; ;; Install `consult-gh-embark' for embark actions
;; (use-package consult-gh-embark
;;   :ensure t
;;   :config
;;   (consult-gh-embark-mode +1))


;; ;; Install `consult-gh-forge' for forge actions
;; (use-package consult-gh-forge
;;   :ensure t
;;   :config
;;   (consult-gh-forge-mode +1)
;;   (setq consult-gh-forge-timeout-seconds 20))


;; Convenience wrappers around ghub
(use-package glab
  :ensure t)
(use-package gtea
  :ensure t)

;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :ensure t
  :defer 0.5
  :after magit
  :custom
  (global-git-gutter-mode +1))

;; https://github.com/DamianB-BitFlipper/magit-pre-commit.el
(use-package magit-pre-commit
  :ensure t
  :defer 0.5
  :after magit)
