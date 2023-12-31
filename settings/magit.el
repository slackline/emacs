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

;; difftastic configuration (https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html)
;;
;; Magit with Difftastic
(defun nds:magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

;; Magit Show
(defun nds:magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (nds:magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

;; Magit Diff
(defun nds:magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (nds:magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

;; Transient prefix
(transient-define-prefix nds:magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("d" "Difftastic Diff (dwim)" nds:magit-diff-with-difftastic)
   ("s" "Difftastic Show" nds:magit-show-with-difftastic)
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
