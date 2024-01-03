;;; LSP CONFIGURATION
;;; --------------------------------------
;;; https://gitlab.com/nathanfurnal/dotemacs/-/snippets/2060535
;;; https://ianyepan.github.io/posts/emacs-ide/
;; Provides workspaces with file browsing (tree file viewer)
;; and project management when coupled with `projectile`.
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-no-png-images t
	treemacs-width 24)
  :bind ("C-c t" . treemacs))


;; Provide LSP-mode for python, it requires a language server.
;; I used to use jedi-language-server loaded by lsp-jedi below but its stopped working on home systems
;; and so I've switched to pyright
;; Know that you have to `M-x lsp-restart-workspace`
;; if you change the virtual environment in an open python buffer.
;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
;; https://ianyepan.github.io/posts/emacs-ide/
(use-package lsp-mode
  :ensure t
  :defer 1
  :init
  (setq lsp-keymap-prefix "C-c L")
  :config
  (setq lsp-idle-delay 0.5
	lsp-enable-symbol-highlighting t
	lsp-pylsp-plugins-pylint-args ["--rcfile=/home/neil/dotfiles/python/.pylintrc"])
  lsp-warn-no-matched-clients nil
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (R-mode . lsp)
	 (bash-mode . lsp)
	 (dockerfile-mode . lsp)
	 (ess-r-mode . lsp)
	 (gfm-mode . lsp)
	 (groovy-mode . lsp)
	 (html-mode . lsp)
	 (julia-mode . lsp)
	 (latex-mode . lsp)
	 (markdown-mode . lsp)
	 (org-mode . lsp)
	 (python-mode . lsp)
	 (sh-mode . lsp)
	 (terraform-mode . lsp)))
;;   :commands (lsp))

;; Provides visual help in the buffer
;; For example definitions on hover.
;; The `imenu` lets me browse definitions quickly.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq
   lsp-ui-doc-border (face-foreground 'default)
   lsp-ui-doc-delay 1
   lsp-ui-doc-enable t
   lsp-ui-doc-include-signature t
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature t
   lsp-ui-doc-position 'top
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-show-with-mouse t
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-use-childframe t
   lsp-ui-flycheck-enable t
   lsp-ui-flycheck-list-position 'right
   lsp-ui-flycheck-live-reporting t
   lsp-ui-imenu-enable t
   lsp-ui-peek-enable t
   lsp-ui-peek-list-width 60
   lsp-ui-peek-peek-height 25
   lsp-ui-sideline-delay 3
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-enable t
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-sideline-show-code-actions t
   lsp-ui-sideline-show-hover t)
  :bind (:map lsp-ui-mode-map
	      ("C-c i" . lsp-ui-imenu)))

;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  :ensure t
  :config
  (setq lsp-treemacs-sync-mode 1))

;; https://github.com/emacs-lsp/dap-mode
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))


;; Required to hide the modeline
(use-package hide-mode-line
  :ensure t)



;; Language servers
;; Read the docs for the different variables set in the config.
;; Python - pyright
;; (use-package lsp-pyright
;;   :ensure t
;;   ;; :config
;;   ;; (setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
;;   ;; (setq lsp-pyright-disable-language-service nil
;;   ;;   lsp-pyright-disable-organize-imports nil
;;   ;;   lsp-pyright-auto-import-completions t
;;   ;;   lsp-pyright-use-library-code-for-types t
;;   ;;   lsp-pyright-typeshed-paths
;;   ;;   lsp-pyright-diagnostic-mode
;;   ;;   lsp-pyright-typechecking-mode "basic"
;;   ;;   lsp-pyright-log-level 1
;;   ;;   lsp-pyright-auto-search-paths
;;   ;;   lsp-pyright-extra-paths
;;   ;;   lsp-pyright-venv-path "~/miniconda3/envs")
;;   :hook ((python-mode . (lambda ()
;;                           (require 'lsp-pyright) (lsp-deferred)))))

;; Python - Jedi
;; LSP Module : https://github.com/fredcamps/lsp-jedi
;; Server     : https://github.com/pappasam/jedi-language-server
(use-package lsp-jedi
  :ensure t
  :after lsp-mode
  :defer 0.5
  :config
  (with-eval-after-load "python-mode"
    ;; (add-to-list 'lsp-disabled-clients '(pyls pylsp))
    (add-to-list 'lsp-enabled-clients 'jedi)))
;; (add-to-list 'lsp-disabled-clients '(python-mode . pyright))
;; (add-to-list 'lsp-enabled-clients '(python-mode . jedi-language-server)))

;; :init
;; (setq lsp-jedi-workspace-extra-paths
;;       (vconcat lsp-jedi-workspace-extra-paths ["/home/neil/.virtualenvs/python3_10/lib/site-packages"])))

;; Python pyls
;; https://www.mattduck.com/lsp-python-getting-started.html

;; Python - Sourcery
;; https://github.com/sourcery-ai/sourcery/wiki/Emacs
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("sourcery" "lsp"))
;;                   :initialization-options '((token . "user_ajagq3NtzYEZHCChHXS1bXvaFFZpOb3f8AC666z0J_cCknj8OLZsDR31tK0")
;;                                             (extension_version . "emacs-lsp")
;;                                             (editor_version . "emacs"))
;;                   :activation-fn (lsp-activate-on "python")
;;                   :server-id 'sourcery
;;                   :add-on? t
;;                   :priority 2))
;; LTex
;; LSP Module : https://github.com/emacs-languagetool/lsp-ltex
;; Server     : https://valentjn.github.io/ltex/
(use-package lsp-ltex
  :ensure t
  :hook (text-mode . (lambda ()
		       (require 'lsp-ltex)
		       (lsp)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-version "16.0.0"))
(use-package lsp-latex
  :ensure t
  :hook (text-mode . (lambda ()
		       (require 'lsp-latex)
		       (lsp))))
;; Julia
;; LSP Module : https://github.com/non-Jedi/lsp-julia
;; Server     :
;; (use-package lsp-julia
;;   :ensure t
;;   :config
;;   (setq lsp-julia-default-environment "~/.julia/environments/v1.5"))

;; Markdown
;; LSP Module : https://emacs-lsp.github.io/lsp-mode/page/lsp-markdown/
;; Server     : https://github.com/remarkjs/remark-language-server

;; Bash
;; https://github.com/bash-lsp/bash-language-server

;; R
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-r/
;; (use-package lsp-r
;;   :ensure t)

;; Cleanup LSP sessions https://arjenwiersma.nl/posts/2022-11-07-cleaning-up-after-lsp/index.html
(defun nds/cleanup-lsp ()
  "Remove all the workspace folders from LSP"
  (interactive)
  (let ((folders (lsp-session-folders (lsp-session))))
    (while folders
      (lsp-workspace-folders-remove (car folders))
      (setq folders (cdr folders)))))
