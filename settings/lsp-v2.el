(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.5
	lsp-enable-symbol-highlighting t
	lsp-pylsp-plugins-pylint-args ["--rcfile=/home/neil/dotfiles/python/.pylintrc"])
  ;; pylsp https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md
  ;; '(;; Enabled
  ;;   ("pylsp.plugins.jedi_completion.enabled" t t)
  ;;   ("pylsp.plugins.jedi_completion.cache_for" '(pandas, numpy, matplotlib))
  ;;   ("pylsp.plugins.jedi_hover.enabled" t t)
  ;;   ("pylsp.plugins.jedi_references.enabled" t t)
  ;;   ("pylsp.plugins.jedi_use_pyenv_environment.enabled" t t)
  ;;   ("pylsp.plugins.pyls_black.enabled" t t)
  ;;   ("pylsp.plugins.pycodestyle.maxLineLength" nil 120)
  ;;   ("pylsp.plugins.pydocstyle.enabled" t t)
  ;;   ("pylsp.plugins.pydocstyle.convention" nil 'numpy)
  ;;   ("pylsp.plugins.pylint.enabled" t t)
  ;;   ;; Disabled (duplicated by flake8)
  ;;   ("pylsp.plugins.pycodestyle.enabled" nil t)
  ;;   ("pylsp.plugins.mccabe.enabled" nil t)
  ;;   ("pylsp.plugins.pyflakes.enabled" nil t))
  :hook (;(lsp-mode . lsp-enable-which-key-integration)
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
	 (terraform-mode . lsp))
  :commands (lsp lsp-deferred))

;; Provides visual help in the buffer
;; For example definitions on hover.
;; The `imenu` lets me browse definitions quickly.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
	lsp-ui-doc-delay 1
	lsp-ui-doc-header nil
	lsp-ui-doc-include-signature t
	lsp-ui-doc-border (face-foreground 'default)
	lsp-ui-doc-use-childframe t
	lsp-ui-doc-position 'top
	lsp-ui-doc-include-signature t
	lsp-ui-doc-use-childframe t
	lsp-ui-doc-show-with-cursor t
	lsp-ui-doc-show-with-mouse t
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
  :hook (lsp-mode . lsp-ui-mode))

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

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred
