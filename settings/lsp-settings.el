;;; LSP CONFIGURATION
;;; --------------------------------------
;;; https://gitlab.com/nathanfurnal/dotemacs/-/snippets/2060535
;;; https://ianyepan.github.io/posts/emacs-ide/
;; Provides workspaces with file browsing (tree file viewer)
;; and project management when coupled with `projectile`.
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-no-png-images t
	   treemacs-width 24)
  :bind ("C-c t" . treemacs))


;; Provide LSP-mode for python, it requires a language server.
;; I use `lsp-pyright`. Know that you have to `M-x lsp-restart-workspace`
;; if you change the virtual environment in an open python buffer.
(use-package lsp-mode
  :ensure t
  :defer t
  :config
  (setq lsp-idle-delay 0.5
	    lsp-enable-symbol-highlighting t)
  ;; (setq lsp-markdown-server-command remark-language-server)
  (lsp-register-custom-settings
   ;; pyls enabled/disabled https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md
   '(;; Enabled
     ("pyls.plugins.jedi_completion.enabled" t t)
     ("pyls.plugins.jedi_completion.cache_for" '(pandas, numpy, matplotlib))
     ("pyls.plugins.jedi_hover.enabled" t t)
     ("pyls.plugins.jedi_references.enabled" t t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pylsp.plugins.pycodestyle.maxLineLength" nil 120)
     ;; ("pylsp.plugins.pydocstyle.enabled" t t)
     ;; ("pylsp.plugins.pydocstyle.convention" nil 'numpy)
     ("pylsp.plugins.pylint.enabled" t t)
     ;; Disabled (duplicated by flake8)
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l"
	          lsp-bash-highlight-parsing-errors t)
  :hook ((python-mode . lsp)
	     (bash-mode . lsp)
	     (dockerfile-mode . lsp)
	     (groovy-mode . lsp)
	     (html-mode . lsp)
	     (latex-mode . lsp)
	     (markdown-mode . lsp)
	     (gfm-mode . lsp)
	     (org-mode . lsp)
	     (R-mode . lsp)
	     (ess-r-mode . lsp)
	     (sh-mode . lsp)
         (terraform-mode . lsp)))

;; Provides completion, with the proper backend
;; it will provide Python completion.
(use-package company
  :ensure t
  :defer t
  :diminish
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t)
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode)))

;; Provides visual help in the buffer
;; For example definitions on hover.
;; The `imenu` lets me browse definitions quickly.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-doc-enable t
	    lsp-ui-doc-delay 1
	    lsp-ui-doc-header nil
        sp-ui-doc-include-signature t
        lsp-ui-doc-border (face-foreground 'default)
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 3)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	          ("C-c i" . lsp-ui-imenu)))

;; LSP Treemacs
(use-package lsp-treemacs
  :ensure t
  :defer t
  :config
  (setq lsp-treemacs-sync-mode 1))

;; Integration with the debug server
(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))


;; Required to hide the modeline
(use-package hide-mode-line
  :ensure t
  :defer t)


;; Language servers
;; Read the docs for the different variables set in the config.
;; Python - pyright
;; (use-package lsp-pyright
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
;;   (setq lsp-pyright-disable-language-service nil
;; 	lsp-pyright-disable-organize-imports nil
;; 	lsp-pyright-auto-import-completions t
;; 	lsp-pyright-use-library-code-for-types t
;; 	;; lsp-pyright-typeshed-paths
;; 	;; lsp-pyright-diagnostic-mode
;; 	lsp-pyright-typechecking-mode "basic"
;; 	lsp-pyright-log-level 1
;; 	;; lsp-pyright-auto-search-paths
;; 	;; lsp-pyright-extra-paths
;; 	lsp-pyright-venv-path "~/miniconda3/envs")
;;   :hook ((python-mode . (lambda ()
;;                           (require 'lsp-pyright) (lsp-deferred)))))

;; Python - Jedi
;; LSP Module : https://github.com/fredcamps/lsp-jedi
;; Server     : https://github.com/pappasam/jedi-language-server
;; (use-package lsp-jedi
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))
  ;; :init
  ;; (setq lsp-jedi-workspace-extra-paths
  ;;       (vconcat lsp-jedi-workspace-extra-paths ["/home/neil/.virtualenvs/default/lib/site-packages"])))

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
                       (lsp))))  ; or lsp-deferred
(use-package lsp-latex
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-latex)
                       (lsp))))
;; Julia
;; LSP Module : https://github.com/non-Jedi/lsp-julia
;; Server     :
(use-package lsp-julia
  :ensure t
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.5"))

;; Markdown
;; LSP Module : https://emacs-lsp.github.io/lsp-mode/page/lsp-markdown/
;; Server     : https://github.com/remarkjs/remark-language-server

;; Bash
;; https://github.com/bash-lsp/bash-language-server

;; R
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-r/
;; (use-package lsp-r
;;   :ensure t)
