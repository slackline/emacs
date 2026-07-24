;;; init.el --- slackline's Emacs configuration -*- lexical-binding: t -*-
;; Author: N Shephard <nshephard@protonmail.com>
;; Keywords: configuration
;; URL: https://www.codeberg.org/slackline/emacs
;;; Commentary:
;; A literate and reproducible Emacs configuration

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :custom
  (when (daemonp) (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AGENT_SOCK")
  (exec-path (append '("~/bin"
                       "~/.local/bin"
                       "~/.cargo/bin/"
                       "~/.node/bin/")
                     exec-path)))

(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package use-package
  :config
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  ;; On some systems we have problems communicating with ELPA (https://emacs.stackexchange.com/a/62210)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  ;; Adding repositories along with priority https://emacs.stackexchange.com/a/2989/10100
  (setq package-archives
        '(("GNU ELPA"	. "https://elpa.gnu.org/packages/")
          ("NonGNU ELPA"  . "https://elpa.nongnu.org/nongnu/")
          ("MELPA Stable" . "https://releases.melpa.org/packages/")
          ("MELPA"	. "https://snapshots.melpa.org/packages/")
          ("jcs elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))
        package-archive-priorities
        '(("MELPA" . 10)
          ("GNU ELPA"	. 5)
          ("NonGNU ELPA"	. 5)
          ("MELPA Stable"	. 3)
          ("jcs elpa" . 0))))

(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

(use-package emacs
  :custom
  (add-to-list 'load-path "~/.config/emacs/elpa/") ; Local LISP
  (confirm-kill-processes nil) ; Stop confirming the killing of processes
  (custom-file "~/.config/emacs/custom.el")
  (describe-bindings-outline-rules ((match-regexp . "Key translations\|Minor Mode Bindings"))) ; https://ottawa.place/@plantarum/116891458751803204
  (dired-dwim-target t) ; move file to other pane as default destination
  (enable-recursive-minibuffers t) ; Vertico - open new minibuffers from inside a minibuffer
  (global-auto-revert-non-file-buffers t) ; Update non-file buffers (Dired) when disk changes
  (global-goto-address-mode t)
  (global-visual-line-mode t) ; Visual line wrap
  (history-length 1000) ; Mini-buffer history
  (inhibit-startup-message t) ; Hide the startup message
  (inhibit-startup-screen t) ; Disable startup screen
  (initial-scratch-message "") ; Make *scratch* buffer blank
  (initial-scratch-message nil)
  (lisp-indent-offset 2)
  (mode-line-compact t)
  (package-install-upgrade-built-in t) ; Upgrade built-in packages
  (pixel-scroll-precision-mode t)
  (ring-bell-function 'ignore)  ; Disable bell sound
  (undo-limit 320000) ; Increase the undo history limits
  (undo-strong-limit 640000)
  (use-dialog-box nil) ; No dialog pop-ups
  (vc-handled-backends '(Git))
  (vc-follow-symlinks t) ; open source of symlink maintain vc (https://stackoverflow.com/a/30900018/1444043)
  (winner-mode t) ; toggling window configuration
  (save-interprogram-paste-before-kill t) ;; Save the clipboard before killing
  (kill-do-not-save-duplicates t) ;; No duplicates in the kill ring
  (redisplay-skip-fontification-on-input t) ;; Disable fontification during input
  (reb-re-syntax 'string) ;; Sane syntax in rebuilder
  (window-combination-resize t) ;; Proportional window resizing
  (help-window-select t) ;; Auto-select help windows
  (set-cursor-color "#0AFF00") ; Bright Green (stands out better)
  ;; (set-cursor-color "#62088A") ; Dark purple (not very visible)
  ;; (browse-url-browser-function 'eww-browse-url) ; Set eww as the default browser
  ;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
                 (display-buffer-no-window) (allow-no-window . t)))
  ;; Turn off package install warnings https://codeberg.org/jcastp/emacs.d/src/branch/main/emacs-config.org#headline-16
  ;; (when (and (fboundp 'native-comp-available-p)
  ;;         (native-comp-available-p))
  ;;   (setq native-comp-async-report-warnings-errors nil
  ;;     native-comp-deferred-compilation t))
  :config
  (add-to-list 'default-frame-alist '(alpha-background . 85))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . right))
  (setq-default fill-column 120) ; Reset line-length
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default sh-basic-offset 2)
  (setq-default sh-indentation 2)
  (setq-default cursor-type 'bar)     ; Line-style cursor similar to other text editors
  (setq-default frame-title-format '("%f"))     ; Make window title the buffer name
  ;; Disable birectional text scanning
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  ;; Don't render cursors in non-focused windows
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  (setopt dictionary-server "dict.org")
  :bind (("C-c U" . revert-buffer)
         ("C-c D" . toggle-debug-on-error)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-x p i" . org-org-cliplink)
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
  ;; ((git-commit-mode
  ;;   forge-post-mode) . gfm-mode)
  (compilation-finish-functions . (lambda (buf strg) (kill-buffer buf)))
  (auto-fill-function . do-auto-fill)
  (before-save . delete-trailing-whitespace)
  (before-save . do-auto-fill)
  (dired-mode . auto-revert-mode) ; auto refresh dired when files change
  (after-save . executable-make-buffer-file-executable-if-script-p) ;; Make scripts executable on save
  ;; imenu
  ((markdown-mode
    makefile-mode
    prog-mode) . imenu-add-menubar-index)
  ((markdown-mode
    makefile-mode
    prog-mode) . (lambda () (setq imenu-auto-rescan t)))
  :init
  (epa-file-enable)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode t)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode t)
  (savehist-mode t)
  (recentf-mode t)
  (global-auto-revert-mode t))

(use-package ssh-agency
  :ensure t
  :custom (ssh-agency-keys '("~/.ssh/id_ed25519" "~/.ssh/haldane_ed25519")))
;; :bind ("C-c C-g" . (ssh-agency-keys '("~/.ssh/id_ed25519" "~/.ssh/haldane_ed25519")))

(use-package keychain-environment
  :ensure t
  :init (keychain-refresh-environment)
  :bind (("C-c C-k" . 'keychain-refresh-environment)))

(use-package emojify
  :hook
  (after-init . global-emojify-mode))

(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)

(defun ns/reinstall-package (pkg)
  "Reinstall a package taken from prompt."
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(defun ns/ssh-agency-load ()
  "Set SSH keys for ssh-agency.

This command sets the SSH keys for ssh-agency."
  (ssh-agency-keys '("~/.ssh/id_ed25519" "~/.ssh/haldane_ed25519")))

(defun ns/auto-create-missing-dirs ()
  "Automatically create missing directory paths when saving a file.

URL `https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/'"
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))
(add-to-list 'find-file-not-found-functions #'ns/auto-create-missing-dirs)

(defun ns/wikipedia-org-linkify ()
  "Make the current word or text selection into a org-mode Wikipedia link.
For Example:
 Emacs
becomes
 [[http://en.wikipedia.org/wiki/Emacs\"][Emacs]]
URL `http://ergoemacs.org/emacs/elisp_html_word_to_wikipedia_linkify.html'
Version 2015-07-27"
  (interactive)
  (let ($p0 $p1 $p2 $linkText)
    (if (region-active-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (progn
        (setq $p0 (point))
        (skip-chars-backward "^ \t\n")
        (setq $p1 (point))
        (goto-char $p0)
        (skip-chars-forward "^ \t\n")
        (setq $p2 (point))))
    (setq $linkText
          (replace-regexp-in-string "_" " " (buffer-substring-no-properties $p1 $p2)))
    (delete-region $p1 $p2)
    (insert (concat "[[http://en.wikipedia.org/wiki/"
                    (replace-regexp-in-string " " "_" $linkText)
                    "][" $linkText "]]"))))

(use-package beacon
  :ensure t
  :defer 2
  :custom (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))

(use-package company
  :ensure t
  :defer 0.5
  :hook
  (text-mode . company-mode)
  (prog-mode . company-mode)
  (org-src-mode . company-mode)
  ;; https://themagitian.github.io/posts/emacsconfig/
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.3)
  (company-selection-wrap-around t)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  (custom-set-faces
   '(company-tooltip ((t (:background "#3e4452"))))
   '(company-tooltip-selection ((t (:background "#454c59"))))
   '(company-tooltip-common ((t (:background "#3e4452"))))
   '(company-scrollbar-bg ((t (:background "#282c34"))))))

(use-package completion-preview
  :ensure t
  :defer 2
  :after (comint)
  :hook
  ((prog-mode text-mode comint-mode) . completion-preview-mode)
  :config
  (setq completion-preview-minimum-symbol-length 2)
  :bind
  (:map completion-preview-active-mode-map
        ("M-n" . completion-preview-next-candidate)
        ("M-p" . completion-preview-prev-candidate)))

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :custom
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref))

(use-package embark
  :ensure t
  :defer 0.5
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  ;; (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (embark-indicators
   '(embark-minimal-indicator  ; default is embark-mixed-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; Consider

(use-package embark-consult
  :ensure t
  :defer 0.5
  :after (consult embark))

(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  (:map completion-list-mode-map ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package move-text
  :ensure t
  :defer 0.5
  :config
  (move-text-default-bindings))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)
                     completion-category-defaults nil
                     completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)

  ;; TAB completion - completes path selection rather than selecting current point
  ;; (keymap-set vertico-map "TAB" #'minibuffer-complete)
  )

(use-package vundo
  :ensure t
  :defer 0.5
  :bind (("C-c v" . vundo))
  :hook
  (prog-mode . vundo-popup-mode)
  (text-mode . vundo-popup-mode))

(use-package which-key
  :ensure t
  :defer 0.5
  :config (which-key-mode)
  (global-set-key (kbd "<f8>") 'which-key-show-major-mode)
  ;; +prefix isn't helpful, lets sort that out via...
  (which-key-add-key-based-replacements
   "C-h 4"         "help-other-win"
   "C-x 4"         "other-window"
   "C-x 5"         "other-frame"
   "C-x 6"         "2-column"
   "C-x 8"         "insert-special"
   "C-x C-k C-q"   "kmacro-counters"
   "C-x C-k C-r a" "kmacro-add"
   "C-x C-k C-r"   "kmacro-register"
   "C-x C-k"       "keyboard-macros"
   "C-x RET"       "encoding/input"
   "C-x a i"       "abbrevs-inverse-add"
   "C-x a"         "abbrevs"
   "C-x n"         "narrowing"
   "C-x p"         "projects"
   "C-x r"         "reg/rect/bkmks"
   "C-x t ^"       "tab-bar-detach"
   "C-x t"         "tab-bar"
   "C-x v M"       "vc-mergebase"
   "C-x v b"       "vc-branch"
   "C-x v"         "version-control"
   "C-x w ^"       "window-detach"
   "C-x w"         "window-extras"
   "C-x x"         "buffer-extras"
   "C-x X"         "edebug"
   "C-c m"         "Magit"
   "C-c m d"       "Diff"
   "C-c m g"       "Link"
   "C-c m l"       "Log")
  "M-s h"         "search-highlight"
  "s-l F"         "Workspace"
  "s-l G"         "Peek"
  "s-l T"         "Treemacs/UI"
  "s-l a"         "Execute/highlight"
  "s-l g"         "Find"
  "s-l h"         "Doc/Describe"
  "s-l r"         "Refactor"
  "s-l w"         "Session")
(with-eval-after-load 'lsp
  (which-key-add-key-based-replacements
   "s-l"           "LSP"
   "s-l ="         "LSP Format"
   "s-l F"         "LSP Workspace"
   "s-l G"         "LSP Peek"
   "s-l T"         "LSP Treemacs/UI"
   "s-l a"         "LSP Execute/highlight"
   "s-l g"         "LSP Find"
   "s-l h"         "LSP Doc/Describe"
   "s-l r"         "LSP Refactor"
   "s-l w"         "LSP Session"))
(with-eval-after-load 'gud (which-key-add-key-based-replacements gud-key-prefix "gud")
                      (with-eval-after-load 'page-ext
                        (which-key-add-key-based-replacements "C-x C-p" "page-extras"))
                      ;; Org-mode provides some additional prefix-keys in `org-mode-map'.
                      (with-eval-after-load 'org
                        (which-key-add-keymap-based-replacements org-mode-map
                          "C-c \""  "org-plot"
                          "C-c C-v" "org-babel"
                          "C-c C-x" "org-extra-commands")))

(use-package helpful
  :ensure t
  :defer 0.5
  :bind (("C-h C" . helpful-command)
         ("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))

(use-package goto-addr
  :hook
  ((compilation-mode eshell-mode shell-mode) . goto-address-mode)
  (prog-mode . goto-address-prog-mode)
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package magit
  :ensure t
  :custom
  (magit-repository-directories
   `(("~/dotfiles" . 1)
     ("~/.config/emacs/" . 1)
     ("~/.password-store/" . 1)
     ("~/org/" . 1)
     ("~/org-roam/" . 1)
     ("~/work/git/" . 3)))
  ;; Don't want auto-fill-mode enabled for the following modes, probably a smarter way of doing this under :hooks
  ;; perhaps?
  (remove-hook 'git-commit-mode #'turn-on-auto-fill)
  (remove-hook 'forge-post-mode #'turn-on-auto-fill)
  :config
  (add-to-list 'after-save-hook 'magit-after-save-refresh-status t)
  (add-to-list 'magit-status-sections-hook 'magit-insert-worktrees t)
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq magit-pull-or-fetch t)
  (setq magit-log-margin '(t "%F %R" magit-log-margin-width t 18))
  (setq magit-process-finish-apply-ansi-colors t)
  ;; :hook
  :bind (:map magit-mode-map (("C-c m C" . magit-clone)
                              ("C-c m B" . magit-clone-bare)
                              ("C-c m F" . magit-pull-from-upstream)
                              ("C-c m P" . magit-push-current-to-upstream)
                              ("C-c m R" . magit-file-rename)
                              ("C-c m f" . forge-pull)
                              ("C-c m d r" . magit-diff-range)
                              ("C-c m d s" . magit-diff-staged)
                              ("C-c m l l" . magit-log)
                              ("C-c m l f" . magit-log-buffer-file)
                              ("C-c m l o" . magit-log-other))))

(use-package code-review
  :ensure t
  :custom
  (code-review-fill-column 120)
  (code-review-new-buffer-window-strategy #'switch-to-buffer)
  :hook
  (code-review-mode . emojify-mode))

(use-package difftastic
  :ensure t
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :custom
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package magit-difftastic
  :straight (:host github :repo "rschmukler/magit-difftastic")
  :after magit
  :config
  (magit-difftastic-mode +1))

(use-package forge
  :ensure t
  :defer 0.5
  :after magit
  :config
  (push '("forgejo.nshephard.dev"
          "forgejo.nshephard.dev/api/v1"
          "forgejo.nshephard.dev"
          forge-forgejo-repository)
        forge-alist))

(use-package ghub
  :ensure t
  :defer 0.5
  :after (magit))

(use-package glab
  :ensure t
  :defer 0.5
  :after (magit))

(use-package git-gutter
  :ensure t
  :defer 0.5
  :after magit
  :custom
  (global-git-gutter-mode +1))

(use-package git-cliff
  :ensure t  :defer 2
  :after (magit))

(use-package conflict-buttons
  :ensure t
  :defer 2
  :after (magit)
  :custom
  (conflict-buttons-global-mode 1))

(use-package fj
  :ensure t
  :defer 0.6
  :after (magit))

(use-package gitlab-ci-mode
  :ensure t
  :defer 2
  :after (glab))

(use-package git-link
  :ensure t
  :after (magit)
  :bind (:map magit-mode-map (("C-c m g c" . git-link-commit)
                              ("C-c m g g" . git-link-dispatch)
                              ("C-c m g h" . git-link-homepage)
                              ("C-c m g l" . git-link)))
  :config
  (require 'git-link-transient))

(use-package git-modes
  :ensure t
  :defer 3
  :after (magit))

(use-package gh-notify
  :ensure t
  :after (magit))

(use-package git-timemachine
  :ensure t
  :defer 3
  :after (magit))

(use-package magit-browse-commit
  :ensure t
  :defer 3
  :after (magit))

(use-package magit-gitlab
  :ensure t
  :defer 3
  :after (magit))

(use-package magit-imerge
  :ensure t
  :after (magit))

(use-package magit-pre-commit
  :ensure t
  :after magit
  :bind (:map magit-mode-map ("@" . magit-pre-commit)))

(use-package magit-stats
  :ensure t
  :defer 0.5
  :after (magit))

(use-package orgit
  :ensure t
  :defer 0.5
  :after (magit))

(use-package orgit-forge
  :ensure t
  :defer 0.5
  :after magit
  :bind (:map magit-mode-map
              ("C-c m c" . orgit-store-link))
  (:map org-mode-map
        ("C-c m v" . org-insert-last-stored-link)))

(use-package treemacs-magit
  :ensure t
  :defer 0.5
  :after (magit))

(use-package why-this
  :ensure t
  :defer 1
  :after magit
  :custom
  (set-face-background 'why-this-annotate-heat-map-cold "#203448")
  (set-face-background 'why-this-annotate-heat-map-warm "#382f27")
  :bind (:map magit-mode-map (("C-c m w" . 'why-this)
                              ("C-c m W" . 'why-this-annotate))))

(use-package casual-suite
  :ensure t
  :bind (:map reb-mode-map ("s-c" . casual-re-builder-tmenu)
              :map reb-lisp-mode-map ("s-c" . casual-re-builder-tmenu)
              :map ibuffer-mode-map))

(use-package org
  :ensure t
  :bind
  (("C-x p i" . 'org-cliplink))
  :custom
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-font-lock-mode 1)
  (org-directory "~/org/")
  (org-agenda-include-diary t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-files '("~/org/agenda.org"
                      "~/org/gtd/admin.org"
                      "~/org/gtd/afmslicer.org"
                      ;; "~/org/gtd/carpentries.org"
                      "~/org/gtd/computing.org"
                      "~/org/gtd/exercise.org"
                      "~/org/gtd/emacs.org"
                      "~/org/gtd/joss.org"
                      "~/org/gtd/layopt.org"
                      ;; "~/org/gtd/openfest2024.org"
                      ;; "~/org/gtd/osc.org"
                      ;; "~/org/gtd/pgfinder.org"
                      "~/org/gtd/reproducibilitea.org"
                      "~/org/gtd/rse.org"
                      ;; "~/org/gtd/rse-competencies.org"
                      "~/org/grd/sheffieldr.org"
                      "~/org/gtd/tcx2gpx.org"
                      ;; "~/org/gtd/thyroid.org"
                      "~/org/gtd/topostats.org"))
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (jq .t)
                                                           (latex .t)
                                                           (org . t)
                                                           (python . t)
                                                           (shell . t)
                                                           (R . t)))
  (org-babel-python-command "~/.virtualenvs/default/bin/python")
  (org-format-latex-options '(:foreground default
                                          :background "rgb 1 1 1"
                                          :scale 1.5
                                          :html-foreground "Black"
                                          :html-background "Transparent"
                                          :html-scale 1.0
                                          :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-startup-indented 0)
  (org-startup-with-inline-images t)
  (org-clock-persist 'history)
  (org-log-done 'time)
  (org-image-actual-width nil)
  (org-export-backends '(beamer html latex md odt))
  (org-confirm-babel-evaluate nil)
  (org-startup-within-inline-images t)
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "BLOCKED(b@)"  "|" "DONE(d!)" "WONT-DO(w@/!)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "GoldenRod" :weight bold))
     ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
     ("BLOCKED" . (:foreground "Red" :weight bold))
     ("DONE" . (:foreground "LimeGreen" :weight bold))
     ("WONT-DO" . (:foreground "DarkViolet" :weight bold))))
  ;; Disable electric-indent-mode in org buffers
  :hook
  (org-mode . (lambda () (electric-indent-local-mode 0)))
  (org-mode . (lambda () (org-rainbow-tags-mode 1)))
  (org-mode . (lambda () (add-hook 'after-save-hook #'ns/org-babel-tangle-on-save))))

(use-package org-analyzer
  :ensure t
  :defer 2)

(use-package org-links
  :ensure t
  :defer 0.5
  :after org-mode)

(use-package org-modern
  :ensure t
  :defer 0.5
  :after org-mode
  :config (setq
           ;; Edit settings
           org-auto-align-tags nil
           org-tags-column 0
           org-catch-invisible-edits 'show-and-error
           org-special-ctrl-a/e t
           org-insert-heading-respect-content t
           ;; Org styling, hide markup etc.
           org-hide-emphasis-markers t
           org-pretty-entities t
           org-ellipsis "…"
           ;; Agenda styling
           org-agenda-tags-column 0
           org-agenda-block-separator ?─
           org-agenda-time-grid
           '((daily today require-timed)
             (800 1000 1200 1400 1600 1800 2000)
             " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
           org-agenda-current-time-string
           "◀── now ─────────────────────────────────────────────────"))



(use-package org-rainbow-tags
  :ensure t
  :defer 0.5)

(use-package org-ref
  :ensure t
  :after (org-roam))

(use-package org-download
  :ensure t
  :defer 0.5
  :after org-mode
  :hook
  (dired-mode . org-download-enable))

(defun ns/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'ns/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defvar ns/org-current-effort "1.00"
  "Current effort for agenda item.")

(defun ns/org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (setq ns/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
                          (with-current-buffer buffer
                            (widen)
                            (goto-char pos)
                            (org-show-context 'agenda)
                            (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
                            (end-of-line 1)
                            (setq newhead (org-get-heading)))
                          (org-agenda-change-all-lines newhead hdmarker))))



(use-package org-roam
  :ensure t
  :defer 10
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam/main"/)
  (org-roam-dailies-directory "daily/")
  (org-roam-db-location "~/org-roam/main/org-roam.db")
  (org-roam-db-autosync-mode)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: ${tags}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?"
      :target (file+head "%<%y-%m-%d>.org" "#+title: %{title}\n#+date: %u\n"))))
  ;; to add : https://www.reddit.com/r/orgroam/comments/tfcwki/org_roam_capture_create_nametitle_of_a_note/
  :hook
  (after-init . org-roam-mode)
  :bind (("C-c n a" . org-roam-alias-add)
         ("C-c n c" . org-roam-capture)
         ("C-c n d" . org-roam-dailies-capture-today)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-graph-show)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n o" . org-id-get-create)
         ("C-c n s" . org-roam-db-sync)
         ("C-c n t" . org-tag-add)
         ("C-c n u s" . org-roam-ui-open)))

(use-package org-roam-timestamps
  :ensure t
  :after org-roam
  :config (org-roam-timestamps-mode))

(use-package org-roam-bibtex
  :ensure t
  :defer 5
  :after (org-roam)
  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links

(use-package org-roam-ui
  :ensure t
  :defer 5
  :after (org-roam-bibtex-mode)
  :init
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package citar-org-roam
  :ensure t
  :defer 5
  :after (citar org-roam)
  :config (citar-org-roam-mode))

(defun ns/org-babel-tangle-on-save ()
  "Tangle config.org on saving."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(defun ns/org-babel-export-on-save ()
  "Export config.org on saving"
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-html-export-to-html))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ns/org-babel-tangle-on-save)))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ns/org-babel-export-on-save)))

;; Almost works, but org-lint needs to know what buffer to check???
(defun ns/org-lint-on-save ()
  "Run org-lint on saving .org files."
  (when(string-equal (file-name-extension (buffer-file-name)) "org")
    (org-lint)))
(add-hook 'after-save-hook 'ns/org-lint-on-save)

(defun ns/yank-markdown-as-org ()
  "Yank Markdown text as Org.

This command will convert Markdown text in the top of the `kill-ring'
and convert it to Org using the pandoc utility."
  (interactive)
  (save-excursion
    (with-temp-buffer
      (yank)
      (shell-command-on-region
       (point-min) (point-max)
       "pandoc -f markdown -t org --wrap=preserve" t t)
      (kill-region (point-min) (point-max)))
    (yank)))

(defun ns/org-copy-region-as-markdown ()
  "Copy the region (in Org) to the system clipboard as Markdown."
  (interactive)
  (if (use-region-p)
      (let* ((region
              (buffer-substring-no-properties (region-beginning) (region-end)))
             (markdown (org-export-string-as region 'md t '(:with-toc nil))))
        (gui-set-selection 'CLIPBOARD markdown))))

(defun ns/retrieve-url-at-point ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (string-match org-bracket-link-regexp text)
      (kill-new (substring text (match-beginning 1) (match-end 1))))))
(define-key org-mode-map (kbd "s-c") #'ns/link-fast-copy)

(use-package org-grimoire
  :defer 4
  :ensure t)

(org-grimoire-setup "nshephard.dev"
                    :base-dir    "/home/neil/work/git/codeberg/slackline/nshephard.dev"
                    :base-url    "https://nshephard.dev"
                    :site-title  "nshephard.dev"
                    :description ""
                    :theme "dev-theme")

(use-package org-capture
  :ensure nil
  :after org-gtd
  :config
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        '(
          ;; Bib references (https://emacs.stackexchange.com/questions/80734/org-capture-to-bibtex-file)
          ("B" "Bibtex template" plain (file "~/org/references.bib")
           "" :prepend t)
          ;; Email
          ("E" "Email"
           entry(file+headline ,(concat org-gtd-directory "emails.org") "Emails")
           "* TODO [#A] Reply: %a" :immediate-finish t)
          ;; Agenda
          ("a" "Agenda")
          ("au" "Things to get Done" entry (file+olp "~/org/agenda.org" "Things to get Done")
           "** TODO %U %?\n" :prepend t)
          ("an" "Not so Urgent" entry (file+olp "~/org/agenda.org" "Not so Urgent")
           "** TODO %U %?\n" :prepend t)
          ("ac" "Computing" entry (file+olp "~/org/agenda.org" "Computing")
           "** TODO %U %?\n" :prepend t)
          ("aw" "Weekly" entry (file+olp "~/org/agenda.org" "Weekly")
           "** TODO %U %?\n" :prepend t)
          ("af" "Fortnightly" entry (file+olp "~/org/agenda.org" "Fortnightly")
           "** TODO %U %?\n" :prepend t)
          ("am" "Monthly" entry (file+olp "~/org/agenda.org" "Monthly")
           "** TODO %U %?\n" :prepend t)
          ("ac" "Annually" entry (file+olp "~/org/agenda.org" "Annually")
           "** TODO %U %?\n" :prepend t)
          ;; Bibliography
          ;; ("b" "Bibliography" item (file+olp "~/org/references.bib")
          ;;  "%?\n" :prepend t)
          ;; Computing
          ("c" "Computing")
          ("ce" "Emacs" entry (file+olp "~/org/computing.org" "Emacs")
           "* TODO %t %?\n" :prepend t)
          ("cl" "Laptop" entry (file+olp "~/org/computing.org" "Laptop")
           "* TODO %t %?\n" :prepend t)
          ("cd" "Desktop" entry (file+olp "~/org/computing.org" "Desktop")
           "* TODO %t %?\n" :prepend t)
          ("cn" "Networking/Routers" entry (file+olp "~/org/computing.org" "Networking/Routers")
           "* TODO %t %?\n" :prepend t)
          ("cq" "Printers" entry (file+olp "~/org/computing.org" "Printers")
           "* TODO %t %?\n" :prepend t)
          ("cr" "RaspberryPi" entry (file+olp "~/org/computing.org" "Raspberry Pi")
           "* TODO %t %?\n" :prepend t)
          ("cm" "AirQuality Monitor" entry (file+olp "~/org/computing.org" "AirQuality Monitor")
           "* TODO %t %?\n" :prepend t)
          ("ca" "Android" entry (file+olp "~/org/computing.org" "Android")
           "* TODO %t %?\n" :prepend t)
          ("cp" "Programming") ;; Programming
          ("cpb" "" entry (file+olp "~/org/computing.org" "Programming" "Bash")
           "* TODO %t %?\n" :prepend t)
          ("cpe" "MongoDB" entry (file+olp "~/org/computing.org" "Programming" "MongoDB")
           "* TODO %t %?\n" :prepend t)
          ("cpp" "Python" entry (file+olp "~/org/computing.org" "Programming" "Python")
           "* TODO %t %?\n" :prepend t)
          ("cpg" "Git" entry (file+olp "~/org/computing.org" "Programming" "Git")
           "* TODO %t %?\n" :prepend t)
          ("cpl" "LaTeX" entry (file+olp "~/org/computing.org" "Programming" "LaTeX")
           "* TODO %t %?\n" :prepend t)
          ("cpr" "R" entry (file+olp "~/org/computing.org" "Programming" "R")
           "* TODO %t %?\n" :prepend t)
          ("cv" "VPS" entry (file+olp "~/org/computing.org" "VPS")
           "* TODO %t %?\n" :prepend t)
          ;; Jobs
          ("j" "Jobs")
          ("jp" "Domestication" entry (file+olp "~/org/jobs.org" "Places to Look")
           "+ %t %?\n" :prepend t)
          ("jps" "Sheffield" entry (file+olp "~/org/jobs.org" "Places to Look" "Sheffield")
           "+ %t %?\n" :prepend t)
          ("js" "Specifics" entry (file+olp "~/org/jobs.org" "Specifics")
           "* %t %?\n" :prepend t)
          ;; Cats
          ("z" "Cats")
          ("zd" "Domestication" entry (file+olp "~/org/cats.org" "Domestication")
           "+ %t %?\n" :prepend t)
          ("zg" "Genetics" entry (file+olp "~/org/cats.org" "Genetics")
           "+ %t %?\n" :prepend t)
          ;; Drugs
          ("d" "Drugs")
          ("dr" "Research")
          ("drc" "Cannabis" item (file+olp "~/org/drugs.org" "Research" "Cannabis")
           "+ %?\n" :prepend t)
          ("drd" "DMT" item (file+olp "~/org/drugs.org" "Research" "DMT")
           "+ %?\n" :prepend t)
          ("drl" "LSD" item (file+olp "~/org/drugs.org" "Research" "LSD")
           "+ %?\n" :prepend t)
          ("drm" "MDMA" item (file+olp "~/org/drugs.org" "Research" "MDMA")
           "+ %?\n" :prepend t)
          ("drp" "Psilocybin" item (file+olp "~/org/drugs.org" "Research" "Psilocybin")
           "+ %?\n" :prepend t)
          ("db" "Books" item (file+olp "~/org/drugs.org" "Books")
           "+ %?\n" :prepend t)
          ("da" "Articles")
          ("dam" "Mushrooms" item (file+olp "~/org/drugs.org" "Articles" "Mushrooms")
           "+ %?\n" :prepend t)
          ("dac" "Culture" item (file+olp "~/org/drugs.org" "Articles" "Culture")
           "+ %?\n" :prepend t)
          ("dag" "General" item (file+olp "~/org/drugs.org" "Articles" "General")
           "+ %?\n" :prepend t)
          ("di" "Interviews" item (file+olp "~/org/drugs.org" "Interviews")
           "+ %?\n" :prepend t)
          ("dg" "Blogs / Podcasts /Videos")
          ("dgd" "DMT" item (file+olp "~/org/drugs.org" "Blogs / Podcasts / Videos" "DMT")
           "+ %?\n" :prepend t)
          ("dgm" "Mushrooms" item (file+olp "~/org/drugs.org" "Blogs / Podcasts / Videos" "Mushrooms")
           "+ %?\n" :prepend t)
          ("dgp" "Mushrooms" item (file+olp "~/org/drugs.org" "Blogs / Podcasts / Videos" "Psychedelics")
           "+ %?\n" :prepend t)
          ("dgz" "Misc" item (file+olp "~/org/drugs.org" "Blogs / Podcasts / Videos" "Misc")
           "+ %?\n" :prepend t)
          ("dt" "Terrence McKenna" item (file+olp "~/org/drugs.org" "Terrence McKenna")
           "+ %?\n" :prepend t)
          ;; Exercise
          ("e" "Exercise")
          ("er" "Logging a run" table-line (file "~/org-roam/main/log/running_2026.org")
           "| %U | %? | km | min + s | | |" :prepend t)
          ("ec" "Logging a cycle" table-line (file "~/org-roam/main/log/cycling.org")
           "| %U | %? | km | min + s | | |" :prepend t)
          ("eh" "Logging a hike" table-line (file "~/org-roam/main/log/hiking.org")
           "| %U | %? | km | m | min + s| |" :prepend t)
          ("ep" "Logging Pilates" table-line (file "~/org-roam/main/log/pilates_2026.org")
           "| %U | %? | |" :prepend t)
          ("em" "Weight & Waist/Hip" table-line (file "~/org-roam/main/log/metrics_weight.org")
           "| %U | %? | | | |" :prepend t)
          ("es" "Steps" table-line (file "~/org-roam/main/log/metrics_steps.org")
           "| %t | %? |" :prepend t)
          ("eb" "Blood" table-line (file "~/org-roam/main/log/metrics_blood.org")
           "| %U | %? | | | | | |" :prepend t)
          ("et" "Training Resources") ;; Training Resources
          ("etc" "Training Resources (Climbing)")
          ("etcf" "Fingerboarding" item (file+olp "~/org-roam/main/training.org" "Resources" "Climbing" "Fingerboarding")
           "+ %?\n" :prepend t)
          ("etcl" "Lattice" item (file+olp "~/org-roam/main/training.org" "Resources" "Climbing" "Lattice")
           "+ %?\n" :prepend t)
          ("etcd" "Dave MacLeod" item (file+olp "~/org-roam/main/training.org" "Resources" "Climbing" "Dave Macleod")
           "+ %?\n" :prepend t)
          ("etch" "Hoopers Beta" item (file+olp "~/org-roam/main/training.org" "Resources" "Climbing" "Hoopers Beta")
           "+ %?\n" :prepend t)
          ("etcm" "Miscellaneous" item (file+olp "~/org-roam/main/training.org" "Resources" "Climbing" "Miscellaneous")
           "+ %?\n" :prepend t)
          ("etcs" "Shauna Coxy" item (file+olp "~/org-roam/main/training.org" "Resources" "Climbing" "Shauna Coxy")
           "+ %?\n" :prepend t)
          ("etr" "Running" entry (file+olp "~/org-roam/main/training.org" "Resources" "Running")
           "+ %t %?\n" :prepend t)
          ("etl" "Calisthenics")
          ("etlg" "General" entry (file+olp "~/org-roam/main/training.org" "Resources" "Calisthenics" "General")
           "+ %t %?\n" :prepend t)
          ("etlr" "Recommended Routine" entry (file+olp "~/org-roam/main/training.org" "Resources" "Calisthenics" "Recommended
Routine")
           "+ %t %?\n" :prepend t)
          ("etls" "Stretching" entry (file+olp "~/org-roam/main/training.org" "Resources" "Calisthenics" "Stretching")
           "+ %t %?\n" :prepend t)
          ("ets" "Swimming" entry (file+olp "~/org-roam/main/training.org" "Resources" "Swimming")
           "+ %t %?\n" :prepend t)
          ("etx" "Research" entry (file+olp "~/org-roam/main/training.org" "Resources" "Research")
           "+ %t %?\n" :prepend t)
          ("eta" "Apps" entry (file+olp "~/org-roam/main/training.org" "Resources" "Apps")
           "+ %t %?\n" :prepend t)
          ("etd" "Apps" entry (file+olp "~/org-roam/main/training.org" "Resources" "Summarising Data")
           "+ %t %?\n" :prepend t)
          ;; Cooking
          ("f" "Food & Drink")
          ("fc" "Chinese")
          ("fcs" "Starters" item (file+olp "~/org-roam/food_chinese.org" "Starters")
           "+ %?\n" :prepend t)
          ("fcr" "Rice" item (file+olp "~/org-roam/food_chinese.org" "Rice")
           "+ %?\n" :prepend t)
          ("fcm" "Main" item (file+olp "~/org-roam/food_chinese.org" "Main")
           "+ %?\n" :prepend t)
          ("fcn" "Noodles" item (file+olp "~/org-roam/food_chinese.org" "Noodles")
           "+ %?\n" :prepend t)
          ("fu" "Indian")
          ("fus" "Starters" item (file+olp "~/org-roam/food_indian.org" "Starters")
           "+ %?\n" :prepend t)
          ("fuc" "Curries" item (file+olp "~/org-roam/food_indian.org" "Curries")
           "+ %?\n" :prepend t)
          ("fub" "Breads" item (file+olp "~/org-roam/food_indian.org" "Breads")
           "+ %?\n" :prepend t)
          ("fi" "Italian")
          ("fip" "Pasta" item (file+olp "~/org-roam/food_italian.org" "Pasta Dishes")
           "+ %?\n" :prepend t)
          ("fiz" "Pizza" item (file+olp "~/org-roam/food_italian.org" "Pizza")
           "+ %?\n" :prepend t)
          ("fs" "Spanish" item (file+olp "~/org-roam/food_italian.org" "Pasta")
           "+ %?\n" :prepend t)
          ("fv" "Vegetarian" item (file+olp "~/org-roam/food_vegetarian.org" "Vegetarian")
           "+ %?\n" :prepend t)
          ("fw" "Web sites" item (file+olp "~/org-roam/food_drink.org" "Links")
           "+ %?\n" :prepend t)
          ("fb" "Books" item (file+olp "~/org-roam/food_drink.org" "Books")
           "+ %?\n" :prepend t)
          ;; Isla
          ("I" "Activities for Isla")
          ("Ie" "Educational" item (file+olp "~/org/isla.org" "Educational")
           "+ %t %?\n" :prepend t)
          ("Ix" "Exercise" item (file+olp "~/org/isla.org" "Exercise")
           "+ %t %?\n" :prepend t)
          ("If" "Fun" item (file+olp "~/org/isla.org" "Fun")
           "+ %t %?\n" :prepend t)
          ("Ig" "Gardening" item (file+olp "~/org/isla.org" "Gardening")
           "+ %t %?\n" :prepend t)
          ;; Paula
          ("p" "Paula")
          ("pa" "Anger" item (file+olp "~/org/paula.org" "Anger")
           "+ %U %?\n" :prepend t)
          ("pf" "Food" item (file+olp "~/org/paula.org" "Food")
           "+ %U %?\n" :prepend t)
          ("ph" "Help" item (file+olp "~/org/paula.org" "Help")
           "+ %U %?\n" :prepend t)
          ("pl" "Listening" item (file+olp "~/org/paula.org" "Not Listening")
           "+ %U %?\n" :prepend t)
          ("pt" "Tidying" item (file+olp "~/org/paula.org" "Tidying")
           "+ %U %?\n" :prepend t)
          ;; Reading
          ("r" "Reading")
          ("rb" "Books" entry (file+olp "~/org/reading.org" "Books")
           "* TODO %?\n" :prepend t)
          ("re" "Economics" entry (file+olp "~/org/reading.org" "Economics")
           "* TODO %?\n" :prepend t)
          ("rg" "Genetics" entry (file+olp "~/org/reading.org" "Genetics")
           "* TODO %?\n" :prepend t)
          ("rs" "Statistics")
          ("rsd" "Data/Machine Learning/AI" entry (file+olp "~/org/reading.org" "Statistics" "Data/Machine Learning/AI")
           "* TODO %?\n" :prepend t)
          ("ri" "Internet")
          ("rid" "Data/Machine Learning/AI" entry (file+olp "~/org/reading.org" "Internet" "Social Media")
           "* TODO %?\n" :prepend t)
          ("rm" "Miscellany" entry (file+olp "~/org/reading.org" "Miscellany")
           "* TODO %?\n" :prepend t)
          ;; ("s" "Org-Roam Notes" entry (file "~/work/org-roam/20220206201656-notes.org")
          ;;  "* %U %?\n" :prepend t)
          ;; TODO
          ("t" "Stuff ToDo in my Life")
          ("th" "House Tasks" entry (file+olp "~/org/todo.org" "House")
           "* TODO %t %?\n" :prepend t)
          ("tg" "Gardening" entry (file+olp "~/org/todo.org" "Garden")
           "* TODO %t %?\n" :prepend t)
          ("tv" "Campervan" entry (file+olp "~/org/todo.org" "Campervan")
           "* TODO %t %?\n" :prepend t)
          ("tc" "Car" entry (file+olp "~/org/todo.org" "Car")
           "* TODO %t %?\n" :prepend t)
          ("tg" "Bike" entry (file+olp "~/org/todo.org" "Bike")
           "* TODO %t %?\n" :prepend t)
          ("ts" "Stuff to Sell" entry (file+olp "~/org/todo.org" "Stuff To Sell")
           "* TODO %t %?\n" :prepend t)
          ;; Coronavirus
          ("v" "Coronavirus")
          ("va" "Air Quality" item (file+olp "~/org/coronavirus.org" "Air Quality")
           "+ %t %?\n" :prepend t)
          ("vc" "Collections/Streams")
          ("vca" "The Atlantic" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "The Atlantic")
           "+ %t %?\n" :prepend t)
          ("vcb" "BMJ" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "BMJ")
           "+ %t %?\n" :prepend t)
          ("vcd" "David Spiegelhalter" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "David Spiegelhalter")
           "+ %t %?\n" :prepend t)
          ("vcg" "The Guardian" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "The Guardian")
           "+ %t %?\n" :prepend t)
          ("vcl" "The Lancet" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "The Lancet")
           "+ %t %?\n" :prepend t)
          ("vcn" "New Scientist" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "New Scientist")
           "+ %t %?\n" :prepend t)
          ("vco" "ONS" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "ONS")
           "+ %t %?\n" :prepend t)
          ("vcs" "Significance" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "Significance magazine")
           "+ %t %?\n" :prepend t)
          ("vcw" "Wired" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "Wired")
           "+ %t %?\n" :prepend t)
          ("vcx" "BioRxiv/MedRxiv" item (file+olp "~/org/coronavirus.org" "Collections/Streams" "BioRxiv/MedRxiv")
           "+ %t %?\n" :prepend t)
          ("vd" "Data" item (file+olp "~/org/coronavirus.org" "Data")
           "+ %t %?\n")
          ("ve" "Evolutionary Genetics" item (file+olp "~/org/coronavirus.org" "Evolutionary Genetics"))
          ("vg" "Genetics" item (file+olp "~/org/coronavirus.org" "Genetics"))
          ("vh" "Herd Immunity" item (file+olp "~/org/coronavirus.org" "Herd Immunity")
           "+ %t %?\n")
          ("vi" "Immunity / Testing / Vaccine / Treatment")
          ("vie" "ENSEMBLE2" item (file+olp "~/org/coronavirus.org" "Immunity / Testing / Vaccine" "ENSEMBLE 2")
           "+ %t %?\n" :prepend t)
          ("vii" "Immunity" item (file+olp "~/org/coronavirus.org" "Immunity / Testing / Vaccine" "Immunity")
           "+ %t %?\n" :prepend t)
          ("vis" "Testing" item (file+olp "~/org/coronavirus.org" "Immunity / Testing / Vaccine" "Testing")
           "+ %t %?\n" :prepend t)
          ("viv" "Vaccine" item (file+olp "~/org/coronavirus.org" "Immunity / Testing / Vaccine" "Vaccine")
           "+ %t %?\n" :prepend t)
          ("vit" "Treatment" item (file+olp "~/org/coronavirus.org" "Immunity / Testing / Treatment" "Treatment")
           "+ %t %?\n" :prepend t)
          ("ve" "Independent SAGE" item (file+olp "~/org/coronavirus.org" "Independent SAGE")
           "+ %t %?\n" :prepend t)
          ("vl" "Long Term Outcomes" item (file+olp "~/org/coronavirus.org" "Long Term Outcomes")
           "+ %t %?\n" :prepend t)
          ("vh" "Mechanism" item (file+olp "~/org/coronavirus.org" "Mechanism")
           "+ %t %?\n" :prepend t)
          ("vhs" "Susceptibility" item (file+olp "~/org/coronavirus.org" "Mechanism" "Susceptibility")
           "+ %t %?\n" :prepend t)
          ("vm" "Miscellaneous" item (file+olp "~/org/coronavirus.org" "Miscellaneous")
           "+ %t %?\n" :prepend t)
          ("vn" "Non-Pharmaceutical Interventions")
          ("vo" "Mortality Rates" item (file+olp "~/org/coronavirus.org" "Mortality Rates")
           "+ %t %?\n" :prepend t)
          ("vp" "Molecular Biology" item (file+olp "~/org/coronavirus.org" "Molecular Biology")
           "+ %t %?\n" :prepend t)
          ("vr" "Risk" item (file+olp "~/org/coronavirus.org" "Risk Assessment")
           "+ %t %?\n" :prepend t)
          ("vs" "SAGE" item (file+olp "~/org/coronavirus.org" "SAGE")
           "+ %t %?\n" :prepend t)
          ("vt" "Track and Trace" item (file+olp "~/org/coronavirus.org" "Track and Trace")
           "+ %t %?\n" :prepend t)
          ("vu" "Transmission" item (file+olp "~/org/coronavirus.org" "Transmission")
           "+ %t %?\n" :prepend t)
          ("vnd" "Physical Distancing" item (file+olp "~/org/coronavirus.org" "Transmission" "Physical Distancing")
           "+ %t %?\n" :prepend t)
          ("vnm" "Face Masks" item (file+olp "~/org/coronavirus.org" "Transmission" "Face Masks")
           "+ %t %?\n" :prepend t)
          ("vup" "Prevalence" item (file+olp "~/org/coronavirus.org" "Transmission" "Prevalence")
           "+ %t %?\n" :prepend t)
          ("vx" "Traffic")
          ("vxc" "Cycling" item (file+olp "~/org/coronavirus.org" "Traffic" "Cycling")
           "+ %t %?\n")
          ("vxr" "Roads" item (file+olp "~/org/coronavirus.org" "Traffic" "Roads")
           "+ %t %?\n")
          ("vw" "Work" item (file+olp "~/org/coronavirus.org" "Work")
           "+ %t %?\n")
          ("vy" "Humour") ;; Humour
          ("vyg" "Graphics" item (file+olp "~/org/coronavirus.org" "Humour" "Graphics")
           "+ %t %?\n" :prepend t)
          ("vyx" "XKCD" item (file+olp "~/org/coronavirus.org" "Humour" "XKCD")
           "+ %t %?\n" :prepend t)
          ("vz" "Non-Science")
          ("vzs" "Sheffield" item (file+olp "~/org/coronavirus.org" "Non-Science" "Sheffield")
           "+ %t %?\n" :prepend t)
          ;; Work
          ("w" "Work")
          ("wj" "Reproducibilitea" entry (file+olp "~/org/gtd/reproducibilitea.org" "Reproducibilitea")
           "** TODO %U %?\n %a" :prepend t)
          ("wo" "OSC" entry (file+olp "~/org/gtd/osc.org" "Open Scholarship Sheffield")
           "** TODO %U %?\n %a" :prepend t)
          ("wR" "Sheffield R" entry (file+olp "~/org/gtd/sheffieldr.org" "SheffieldR")
           "** TODO %U %?\n %a" :prepend t)
          ("wr" "RSE" entry (file+olp "~/org/gtd/rse.org" "RSE")
           "** TODO %U %?\n %a" :prepend t)
          ("wt" "TopoStats" entry (file+olp "~/org/gtd/topostats.org" "TopoStats")
           "** TODO %U %?\n %a" :prepend t)
          ("wc" "Clarity" entry (file+olp "~/org/gtd/clarity.org" "Clarity")
           "** TODO %U %?\n %a" :prepend t)
          ("wC" "Carpentries Courses" entry (file+olp "~/org/gtd/carpentries.org" "Courses")
           "** TODO %U %?\n %a" :prepend t)
          ("wp" "PGFinder" entry (file+olp "~/org/gtd/pgfinder.org" "PGFinder")
           "** TODO %U %?\n %a" :prepend t))))

(setq org-gtd-update-ack "4.0.0")
(use-package org-gtd
  :ensure t
  :after org
  :demand t
  :config
  (setq org-gtd-directory '"~/org/gtd")
  (setq org-edna-use-inheritance t)
  (org-edna-mode)
  ;; All your GTD keywords must be in the same sequence
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL")))

  ;; Then map GTD semantic states to your keywords
  (setopt org-gtd-keyword-mapping
          '((todo . "TODO")      ;; tasks not ready to be acted upon
            (next . "NEXT")      ;; tasks ready to act on immediately
            (wait . "WAIT")      ;; tasks blocked or delegated
            (done . "DONE")      ;; tasks successfully completed
            (canceled . "CNCL"))) ;; tasks that won't be completed
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-clarify-map
   ("C-c c" . org-gtd-organize)))

(use-package csv-mode
  :ensure t
  :defer 6
  :mode (("\\.csv" . csv-mode))
  :hook
  (csv-mode . csv-guess-set-separator)
  (csv-mode . csv-align-mode))

(use-package ess
  :ensure t
  ;;     :requires ess-r-mode
  ;;     ess-r-package
  :init
  :mode (("/R/.*\\.q\\'"       . R-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rmd\\'"          . Rd-mode)
         ("\\.Rd\\'"           . Rd-mode))
  :interpreter (("R" . R-mode)
                ("R" . R-transcript-mode)
                ("R" . Rd-mode))
  :config
  (require 'ess-r-mode)
  (require 'ess-r-package)
  (setq ess-r-backend 'lsp)
  (setq comint-input-ring-size 1000)
  (setq ess-indent-offset 2)
  (setq ess-eval-visibly-p nil)
  (setq ess-startup-directory nil)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-togggle-underscore nil)
  (setq ess-eval-visibly 'nowait)
  (setq ess-use-tracebug nil)
  :hook
  (ess-mode . company-mode)
  (inferior-ess-mode . company-mode)
  :bind
  (:map ess-r-mode-map
        ("_" . 'ess-insert-assign)  ;;
        ("C-q" . 'ess-eval-region-or-line-and-step)
        ("C-|" . " |>\n"))
  (:map inferior-ess-r-mode-map
        ("_" . 'ess-insert-assign)
        ("C-|" . " |>\n")))

(use-package ess-smart-underscore
  :ensure t
  :after (ess))

(use-package essgd
  :ensure t)

(use-package jq-mode
  :ensure t
  :defer 5
  :mode ("\\.json\\'" . jq-mode)
  :custom
  (add-to-list 'auto-mode-alist '("\\.json$" . jq-mode))
  (add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))
  )

(add-to-list 'auto-mode-alist '("\\.js" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json" . json-mode))
(add-to-list 'auto-mode-alist '("\\.svelte" . js-ts-mode))

(use-package just-mode
  :ensure t
  :defer 0.5)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)
;;(latex-preview-pane-enable)
(setq latex-run-command "pdflatex")

(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.qmd\\'" . markdown-mode)
   ("\\.Rmd\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :hook
  ((markdown-mode . auto-fill-mode)
   (markdown-mode . outline-minor-mode))
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-m" . markdown-do)))

(use-package mermaid-mode
  :ensure t
  :defer 10)

(use-package ob-mermaid
  :ensure t
  :after (mermaid-mode))

(use-package mermaid-ts-mode
  :ensure t
  :after (mermaid-mode))

(use-package python
  :after (pyvenv)
  :ensure t
  :defer 1
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"
        python-environment-directory venv-location)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
  ;; Define a skeleton for printing
  (python-skeleton-define print
                          "Insert a print statement that will show the value of the argument."
                          "Enter the variable/object name: "
                          "print(f'\\n{" str "=}\\n')")
  :bind (:map python-mode-map
              ("C-c p t" . python-pytest-dispatch)
              ("C-c p l" . pylint)
              ("C-c p y" . pylint-insert-ignore-comment)
              ("C-c p n" . numpydoc-generate)
              ("C-c p b" . blacken-buffer)
              ("C-c p r" . ruff-format-buffer)
              ("C-c p v" . pyvenv-workon)
              ("C-c p T c" . python-skeleton-class)
              ("C-c p T d" . python-skeleton-def)
              ("C-c p T f" . python-skeleton-for)
              ("C-c p T i" . python-skeleton-if)
              ("C-c p T m" . python-skeleton-import)
              ("C-c P" . python-skeleton-print)
              ("C-c p T t" . python-skeleton-try)
              ("C-c p T T" . python-skeleton-parameterized-test)
              ("C-c p T w" . python-skeleton-while)))

(use-package pyvenv
  :ensure t
  :defer 1
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/.virtualenvs/"))
  (pyvenv-mode 1)
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t
        venv-byhost
        '(("kimura" . "~/.virtualenvs/")
          ("fisher" . "~/.virtualenvs/")
          ("haldane" . "~/.virtualenvs/")
          ("ovh" . "~/.virtualenvs/")
          ("alarmpi" . "~/.virtualenvs/")
          ("alarmpi-4b" . "~/.virtualenvs/"))
        venv-location (cdr
                       (assoc system-name venv-byhost))
        default-venv-byhost
        '(("kimura" . "~/.virtualenvs/default")
          ("fisher" . "~/.virtualenvs/python3_9")
          ("haldane" . "~/.virtualenvs/default")
          ("ovh" . "~/.virtualenvs/default")
          ("alarmpi" . "~/.virtualenvs/default")
          ("alarmpi-4b" . "~/.virtualenvs/default"))
        default-venv (cdr
                      (assoc system-name default-venv-byhost))
        python-environment-directory venv-location)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (pyvenv-restart-python)))
  :hook
  (python-mode . pyvenv-mode)
  (after-init-hook . (pyvenv-workon default-env))
  )

(use-package uv-mode
  :ensure t
  :defer 0.5
  :after (python-mode)
  :hook (python-mode . uv-mode-auto-activate-hook))

(use-package blacken
  :ensure t
  :defer 0.5
  :after (python-mode)
  :bind
  (:map python-mode-map
        ("C-c p b" . blacken-buffer))
  :custom
  (blacken-line-length 120)
  :hook
  (python-mode . blacken-mode))

(use-package numpydoc
  :ensure t
  :defer 0.5
  :after (python-mode)
  :bind
  (:map python-mode-map
        ("C-c p n" . numpydoc-generate))
  :custom
  (numpydoc-prompt-for-input t)
  (numpydoc-insert-examples-block 0)
  ;; Awaiting merge
  ;; (numpydoc-newline-after-opening-quotes t)
  (numpydoc-insert-raises-block 0))

(use-package python-pytest
  :ensure t
  :after (pyvenv)
  :bind (:map python-mode-map
              ("C-c p t" . python-pytest-dispatch))
  :custom
  (transient-append-suffix
    'python-pytest-dispatch
    '(0)
    ["Regression Tests"
     ;; ("-r" "Reset regression tests" "--regtest-reset")
     ("--su" "Reset Syrupy snapshot" "--snapshot-update")
     ("--tee" "Print results" "--regtest-tee")
     ("--nodiff" "Suppress output" "--regtest-nodiff")
     ("--endings" "Do not strip whitespaces at end of recorded lines" "--regtest-consider-line-endings")
     ])
  (transient-append-suffix
    'python-pytest-dispatch
    '(0)
    ["Profiling"
     ("-P" "Generate profiling information" "--profile")
     ("--profile-svg" "Generate profiling graph" "--profile-svg")
     ("--pstats-dir" "Configure the dump directory of profile data files" "--pstats-dir")
     ("--element-number" "Defines how many elements will display in a result" "--element-number")
     ("--strip-dirs" "Configure to show/hide the leading path information from file names" "--strip-dirs")
     ])
  (transient-append-suffix
    'python-pytest-dispatch
    '(0)
    ["Matplotlib Plugin"
     ("--mpl" "Enable comparison of matplotlib figures to reference files" "--mpl")
     ("--generate-path" "Path to generate baseline images" "--mpl-generate-path")
     ("--html" "Generate summary report in HTML" "--mpl-generate-summary html")
     ("--json" "Generate summary report in JSON" "--mpl-generate-summary json")
     ]))

(use-package ruff-format
  :ensure t
  :defer 0.5
  :after (python-mode)
  :bind (:map python-mode-map
              ("C-c p r" . ruff-format-buffer))
  :after (python-mode)
  :hook (python-mode . ruff-format-on-save-mode))

(use-package lazy-ruff
  :ensure t
  :defer 0.5
  :after (python-mode)
  :bind (("C-c p l" . lazy-ruff-lint-format-dwim)) ;; keybinding
  :config
  (lazy-ruff-mode-global-toggle t)) ;; Enable the lazy-ruff minor mode globally

(use-package flymake-ruff
  :ensure t
  :defer 0.5
  :after (python-mode)
  ;; :hook (python-mode . flymake-ruff-load))
  :hook (lsp-managed-mode . flymake-ruff-load))

(use-package sqlite-mode
  :custom
  (defun ns/sqlite-view-file-magically ()
    "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it."
    (require 'sqlite-mode)
    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name)))
  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . ns/sqlite-view-file-magically)))

(use-package sqlite-mode-extras
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("b" . sqlite-mode-extras-backtab-dwim)
         ("f" . sqlite-mode-extras-tab-dwim)
         ("+" . sqlite-mode-extras-add-row)
         ("D" . sqlite-mode-extras-delete-row-dwim)
         ("C" . sqlite-mode-extras-compose-and-execute)
         ("E" . sqlite-mode-extras-execute)
         ("S" . sqlite-mode-extras-execute-and-display-select-query)
         ("DEL" . sqlite-mode-extras-delete-row-dwim)
         ("g" . sqlite-mode-extras-refresh)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.cff\\'" . yaml-mode)))

(use-package polymode
  :ensure t
  :custom
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))

(use-package poly-markdown
  :ensure t
  :defer 0.5
  :after (polymode))

(use-package poly-noweb
  :ensure t
  :defer 0.5
  :after (polymode))

(use-package poly-org
  :ensure t
  :defer 0.5
  :after (polymode))

(use-package poly-R
  :ensure t
  :defer 0.5
  :after (polymode))

(use-package poly-rst
  :ensure t
  :defer 0.5
  :after (polymode))

(use-package mason
  :ensure t
  :hook
  (after-init-hook . mason-ensure))
(mason-setup)
;; (mason-install "black")
;; (mason-install "jedi-language-server")
;; (mason-install "jq-lsp")
;; (mason-install "ltex-ls-plus")
;; (mason-install "markdownlint-cli2")
;; (mason-install "mypy")
;; (mason-install "prettierd")
;; (mason-install "pydocstyle")
;; (mason-install "pylint")
;; (mason-install "remark-language-server")
;; (mason-install "ruff")
;; (mason-install "r-languageserver")
;; (mason-install "rumdl")
;; (mason-install "rust-analyzer")
;; (mason-install "shellcheck")
;; (mason-install "ty")
;; (mason-install "uv")
;; (mason-install "yaml-language-server")
;; (mason-install "yamllint")

(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :custom (lsp-keymap-prefix "s-l")
  (lsp-disabled-clients '(pylsp pyls))
  ;; :init (setq lsp-keymap-prefix "s-l")
  :hook ((R-mode . lsp-deferred)
         (bash-mode . lsp-deferred)
         ;; (dockerfile-mode . lsp-deferred)
         (ess-r-mode . lsp-deferred)
         (gfm-mode . lsp-deferred)
         (git-commit-mode . lsp-deferred)
         (forge-post-mode . lsp-deferred)
         ;; (groovy-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         ;; (julia-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (latex-mode . lsp-deferred)
         (markdown-mode . lsp-deferred)
         (org-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (sh-mode . lsp-deferred)))
;; (terraform-mode . lsp-deferred)
;; (typescript-mode . lsp-deferred)))

(use-package lsp-jedi
  :ensure t
  :after lsp-mode
  :defer 0.5
  :config
  (with-eval-after-load "python-mode"
    (add-to-list 'lsp-enabled-clients 'jedi)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :defer t
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-delay 2)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-c i" . lsp-ui-imenu)))

(use-package lsp-ltex-plus
  :ensure t
  :defer t
  :init
  (lsp-ltex-plus-enable-for-modes)
  :custom
  (lsp-ltex-plus-check-programming-languages t))
;; (setq lsp-ltex-plus-version "18.7.0"))

(use-package realgud
  :ensure t
  :defer 1
  :after (lsp-mode))

(use-package realgud-ipdb
  :ensure t
  :defer 0.5
  :after (realgud))

(use-package projectile
  :ensure t
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map))
  :custom
  (projectile-project-search-path '("~/dotfiles/" "~/org/" "~/org-roam" "~/.config/emacs" ("~/work/git/" . 3)))
  (projectile-auto-discover t)
  (projectile-enable-caching t)
  (projectile-switch-project-action #'projectile-dispatch)
  :init
  (projectile-mode +1))

;; Initially set to 0
(setq active-projects-list nil)

(setq project-org-id-alist
      '(("~/org/" . "5be58bbc-beeb-451d-bcc5-b3987257e581")
        ("~/org-roam/" . "46553056-ef21-48a0-a58d-c2c64ae8b56b")
        ("~/work/git/hub/ICAIR-Sheffield/LayOpt/" . "4406be03-5c5a-4cbd-b1d8-d7c0874acbcc")
        ("~/work/git/hub/Morinay-Lab/lotties/" . "844af655-fd54-42a4-a3fa-45064fd79663")
        ("~/.config/emacs" . "281f2ec1-c60b-4ace-83d6-49bdeced89e1")))

(defun log-active-project ()
  "Logs the active project.

Add this to the `buffer-list-update-hook'.

The logging assumes that you're always working on some project.
For instance, for the purposes of this function, switching to an
``unaffiliated'' buffer (as reported by projectile) has no effect
— you will be ``clocked'' as still working on the last active
project, at least until you switch to a different project."
  (let* ((project (ignore-errors (projectile-project-root)))
         (last-logged-project (car active-projects-list))
         (org-id (cdr (assoc project project-org-id-alist)))
         (last-org-id (cdr (assoc last-logged-project project-org-id-alist))))
    (unless (or (equal last-logged-project project)
                (not org-id))
      (setq active-projects-list (cons project active-projects-list))
      (if last-org-id (org-with-point-at (org-id-find last-org-id 'marker)
                                         (if (org-clock-is-active)
                                             (org-clock-out))))
      (org-with-point-at (org-id-find org-id 'marker)
                         (org-clock-in)))))


(add-hook 'buffer-list-update-hook 'log-active-project)

(use-package consult-projectile
  :ensure t
  :after projectile)

(use-package treesit-fold
  :ensure t
  :after (treesit)
  :custom
  (treesit-fold-line-comment-mode t)
  (global-treesit-fold-mode t)
  :hook
  ((css-ts-mode
    json-ts-mode
    just-ts-mode
    python-ts-mode
    rust-ts-mode
    toml-ts-mode) . treesit-fold-mode))

(use-package treesit-ispell
  :ensure t
  :defer 0.5
  :after (treesit))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived
from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.
If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
        (if (derived-mode-p 'prog-mode) (progn (message "Flyspell on (code)") (flyspell-prog-mode))
          ;; else
          (progn (message "Flyspell on (text)") (flyspell-mode 1))))))
;; I tried putting (flyspell-buffer) here but it didn't seem to work

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so
code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
    ;; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

(add-hook 'find-file-hook 'flyspell-on-for-buffer-type)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'ess-r-mode-hook #'yas-minor-mode))
(yas-global-mode)

(use-package yasnippet-snippets
  :ensure t)

(use-package kirigami
  :ensure t
  :defer 2
  :after treesit-fold
  :commands (kirigami-open-fold
             kirigami-open-fold-rec
             kirigami-close-fold
             kirigami-toggle-fold
             kirigami-open-folds
             kirigami-close-folds-except-current
             kirigami-close-folds)
  :bind
  ;; Vanilla Emacs keybindings
  (("C-c z o" . kirigami-open-fold)          ; Open fold at point
   ("C-c z O" . kirigami-open-fold-rec)      ; Open fold recursively
   ("C-c z r" . kirigami-open-folds)         ; Open all folds
   ("C-c z c" . kirigami-close-fold)         ; Close fold at point
   ("C-c z m" . kirigami-close-folds)        ; Close all folds
   ("C-c z a" . kirigami-toggle-fold)))      ; Toggle fold at point

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(add-hook 'conf-mode-hook #'outline-minor-mode)

;; Systems and General Purpose
(add-hook 'c-mode-hook #'hs-minor-mode)
(add-hook 'c++-mode-hook #'hs-minor-mode)
(add-hook 'java-mode-hook #'hs-minor-mode)
(add-hook 'rust-mode-hook #'hs-minor-mode)
(add-hook 'go-mode-hook #'hs-minor-mode)
(add-hook 'ruby-mode-hook #'hs-minor-mode)
;; Web and Frontend
(add-hook 'js-mode-hook #'hs-minor-mode)
(add-hook 'typescript-mode-hook #'hs-minor-mode)
(add-hook 'css-mode-hook #'hs-minor-mode)
;; Scripting, Data, and Infrastructure
(add-hook 'sh-mode-hook #'hs-minor-mode) ; for bash/shell scripts
(add-hook 'json-mode-hook #'hs-minor-mode)
(add-hook 'lua-mode-hook #'hs-minor-mode)

(use-package outline-indent
  :commands outline-indent-minor-mode
  :custom
  (outline-indent-ellipsis " ▼")
  :hook (haskell-mode . outline-indent-minor-mode)
  (python-mode . outline-indent-minor-mode)
  (python-ts-mode . outline-indent-minor-mode)
  (yaml-mode . outline-indent-minor-mode)
  (yaml-ts-mode . outline-indent-minor-mode))

;; Python
;; (add-hook 'python-mode-hook #'outline-indent-minor-mode)
;; (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

;; ;; Yaml
;; (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
;; (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)

;; ;; Haskell
;; (add-hook 'haskell-mode-hook #'outline-indent-minor-mode)

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package envrc
  :ensure t
  :defer 0.5
  :hook (after-init . envrc-global-mode))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes
        '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
  ;; Placement
  ;; (setq dirvish-use-header-line nil)     ; hide header line (show the classic dired header)
  ;; (setq dirvish-use-mode-line nil)       ; hide mode line
  (setq dirvish-use-header-line 'global)    ; make header line span all panes

  ;; Hide the parent directory
  (setq dirvish-default-layout '(0 0.4 0.6))
  ;; Height
  ;;; '(25 . 35) means
  ;;;   - height in single window sessions is 25
  ;;;   - height in full-frame sessions is 35
  (setq dirvish-header-line-height '(25 . 35))
  (setq dirvish-mode-line-height 25) ; shorthand for '(25 . 25)

  ;; Segments
  ;;; 1. the order of segments *matters* here
  ;;; 2. it's ok to place raw string inside
  (setq dirvish-header-line-format
        '(:left (path) :right (free-space))
        dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index))))

(use-package all-the-icons
  :ensure t
  :after dirvish)

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons)

(use-package all-the-icons-ibuffer
  :ensure t
  :after all-the-icons)

(use-package dired-duplicates
  :ensure t
  :defer 0.5
  :after (dirvish))

(use-package dired-quick-sort
  :ensure t
  :defer 0.5
  :after dirvish
  :config
  (dired-quick-sort-setup))

(use-package dired-ranger
  :ensure t
  :defer 0.5
  :after (dirvish)
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package dired-subtree
  :ensure t
  :defer 0.5
  :after (dirvish)
  :custom
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package dired-rsync-transient
  :ensure t
  :defer 0.5
  :after (dirvish))

(use-package tramp
  :ensure t
  :custom
  (tramp-default-method "ssh")
  (add-to-list 'tramp-default-method-alist '("" "neil" "ssh"))
  ;; Set prompt so it doesn't hang
  (shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *")
  (tramp-auto-save-directory "~/.config/emacs/tmp/")
  (remote-file-name-inhibit-locks t)
  (tramp-use-scp-direct-remote-copying t)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-copy-size-limit (* 1024 1024)) ;; 1Mb
  ;; Set default usernames for different hosts and a global default.
  (add-to-list 'tramp-default-user-alist
               '("ssh" ".*ovh'" "arch") t)
  (add-to-list 'tramp-default-user-alist
               '("ssh" ".*openwrt" "admin") t)
  (add-to-list 'tramp-default-user-alist
               '(nil nil "neil") t)
  (add-to-list 'tramp-default-user-alist
               '("ssh" ".*alarmpi-4b" "neil") t)
  (add-to-list 'tramp-default-user-alist
               '("ssh" ".*crow'" "neil") t))

(use-package ghostel
  :ensure t)

(use-package citar
  :ensure t
  :defer 1
  :custom
  (citar-bibliography '("~/org/references.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package mpdel
  :ensure t
  :defer 1
  :custom
  (libmpdel-hostname "192.168.1.28")
  (libmpdel-port 6600)
  ;; :bind (("<XF86AudioPlay>" . libmpdel-playback-play-pause)
  ;;        ("<XF86AudioNext>" . libmpdel-playback-next)
  ;;        ("<XF86AudioPrev>" . libmpdel-pl;; ayback-previous))
  ;;        ;; ("<XF86AudioPlay>" ("MPDel Play" . libmpdel-playback-play-pause))
  ;;        ;; ("<XF86AudioNext>" ("MPDel Next" . libmpdel-playback-next))
  ;;        ;; ("<XF86AudioPrev>" ("MPDel Prev" . libmpdel-playback-previous))
  :bind-keymap (("C-x Z" . mpdel-core-map)))
;; (mpdel-mode)

(use-package osm
  :ensure t
  :defer 3
  :custom
  (osm-server 'default)
  (osm-home '(53.356116 -1.463397 15))
  :bind
  (("C-c o" . osm-prefix-map)))

(use-package password-store-menu
  :ensure t
  :config (password-store-menu-enable)
  :custom (password-store-menu-key "C-c C-p"))

(use-package password-store-otp
  :ensure t
  :after (password-store-menu))

(use-package scratch
  :ensure t
  :bind ("C-c s" . scratch))

(use-package scratch-plus
  :ensure t
  :defer t
  ;; :hook
  ;; (after-init-hook . scratch-plus-mode)
  :custom
  (add-hook 'after-init-hook #'scratch-plus-mode)
  (scratch-plus-save-directory "~/.config/emacs/scratch")
  (scratch-plus-project-subdir ".scratch")
  (scratch-plus-idle-save 3))

(use-package rg
  :ensure t)

(use-package sicp
  :ensure t)

(use-package tmr
  :ensure t
  :defer 0.5
  :bind-keymap (("C-c T" . tmr-prefix-map)))

(use-package display-wttr
  :ensure t
  :config
  (display-wttr-mode))

(use-package wttrin
  :ensure t
  :defer 0.5
  :bind ("C-c w" . wttrin)
  :custom
  (wttrin-default-locations '("Sheffield", "Nant Peris")))

(use-package devdocs
  :ensure t
  :defer 4
  :bind ("C-h D" . devdocs-lookup))

(use-package elfeed
  :ensure t
  :defer 2
  :custom
  (setq elfeed-feeds '("https://freshrss.nshephard.dev/api/query.php?user=nshephard&t=84c876bf38ba62861111455deaad9adf&f=rss"))
  )

(global-set-key (kbd "C-x w") 'elfeed)

;; use an org file to organise feeds
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

(use-package comet-trail
  :ensure t
  :defer 2
  :custom
  (add-hook 'prog-mode-hook #'comet-trail-mode)
  (add-hook 'text-mode-hook #'comet-trail-mode))

(use-package modus-themes
  :ensure t
  :defer 0.5
  :config
  ;; Add all your customisation's prior to loading the themes
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-org-blocks '(tinted-background))
  (setq modus-themes-include-derivatives-mode 1)
  :bind
  ("C-c C-t m" . modus-themes-toggle))

(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-take-over-modus-themes-mode t)
  (setq ef-themes-disable-other-themes 'ef-themes-light-themes)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random)
   ("C-c C-t d" . ef-themes-select-dark)
   ("C-c C-t e" . ef-themes-toggle)))
(load-theme 'ef-dark :no-confirm)

(use-package golden-ratio
  :ensure t
  :defer 0.5
  :custom
  (setq golden-ratio-auto-scale t))

(set-face-attribute 'default t :font "Hack")

(use-package hide-mode-line
  :ensure t
  :defer 3
  :hook
  (completion-list-mode-hook . hide-mode-line-mode))

(use-package mood-line
  :ensure t
  :custom
  (mood-line-glyph-alist mood-line-glyphs-unicode))
(mood-line-mode)

(use-package ibuffer
  :ensure nil
  :defer 4)

(use-package ibuffer-vc
  :ensure t
  :defer 3.0)

(use-package smartparens
  :ensure t
  :defer 1
  :custom
  (smartparens-global-mode t)
  :hook
  (prog-mode . smartparens-mode)
  (text-mode . smartparens-mode)
  (markdown-mode . smartparens-mode)
  (latex-mode . smartparens-mode)
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :defer 1
  :hook
  (prog-mode . rainbow-mode))

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (defun centaur-tabs-buffer-groups ()
    "Groups tabs based on which project root they are in if possible"
    (let ((get-closest-projectile-project
           (lambda (path)
             (let ((expanded-path (f-long path)))
               (-first (lambda (proj)
                         (s-starts-with? proj
                                         expanded-path))
                       (-map (lambda (proj)
                               (f-long proj))
                             projectile-known-projects))))))
      (list (cond
             ;; Group as part of projectile project if directly part of it
             ((condition-case _err (projectile-project-root) (error nil))
              (f-expand (projectile-project-root)))
             ;; Try to group as part of projectile project if indirectly part of it (started from the same directory,
             ;; not yet tracked, or maybe temporary buffer)
             (get-closest-projectile-project default-directory)
             ((string-equal "*" (substring (buffer-name) 0 1)) "proc-buffers")
             ;; ... other groupings ...
             (t "Other"))))))
(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))
     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p "*temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))
:custom
(setq centaur-tabs-enable-key-bindings t)
(setq centaur-tabs-style "wave")
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-bar 'under)
(setq x-underline-at-descent-line t)
(setq centaur-tabs-cycle-scope 'default)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "⏺")
;; :bind(
;;        ;; ("C-c t C-<right>" ("Move tab right" . centaur-tabs-move-current-tab-to-right))
;;        ;; ("C-c t C-<left>" ("Move tab left" . centaur-tabs-move-current-tab-to-left))
;;        ("C-<prior>" . centaur-tabs-backward)
;;        ("C-<next>"  . centaur-tabs-forward))

;; Function Keys
(global-set-key (kbd "<f1>") 'password-store-copy)
(global-set-key (kbd "<f2>") 'eval-buffer)
(global-set-key (kbd "<f3>") 'eval-region)
(global-set-key (kbd "<f4>") 'package-list-packages)
(global-set-key (kbd "<f5>") 'keychain-refresh-environment)
(global-set-key (kbd "<f6>") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "<f7>") 'ibuffer)
(global-set-key (kbd "<f8>") 'which-key-show-major-mode)
(global-set-key (kbd "<f9>") 'emacs-index-search)
(global-set-key (kbd "C-M-%") 'query-replace-regexp)
(if (system-name) "kimura" (global-set-key (kbd "<XF86HomePage>") 'osm-home))

;; Python
(global-set-key (kbd "C-c p v") 'pyvenv-workon)
(global-set-key (kbd "C-c p p") 'run-python)

;; Miscellaneous
(global-set-key (kbd "C-c C-k") 'keychain-refresh-environment)
(global-set-key (kbd "C-c u") 'rsync-html)
(global-set-key (kbd "C-c C-r") 'revert-buffer-no-confirm)

;; vundo
(global-set-key (kbd "C-c v") 'vundo)

;; Unset overwrite-mode
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "<insertchar>"))

(provide 'init.el)
;;; init.el ends here
