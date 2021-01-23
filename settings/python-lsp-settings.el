;;; PYTHON LSP CONFIGURATION
;;; --------------------------------------
;;; https://gitlab.com/nathanfurnal/dotemacs/-/snippets/2060535

;; Provides workspaces with file browsing (tree file viewer)
;; and project management when coupled with `projectile`.
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-no-png-images t
	  treemacs-width 24)
  :bind ("C-c t" . treemacs))

;;; Virtual Environment Setuip
;; Define location of virtual environments by host and set venv-location
(setq venv-byhost
      '(("kimura" . "~/.virtualenvs/")
	("fisher" . "~/.virtualenvs/")
	("ovh" . "~/.virtualenvs/")
	("alarmpi" . "~/.virtualenvs/")
	("alarmpi-4b" . "~/.virtualenvs/")
	("583-datascience.samba.sheffield.thefloow.com" . "~/.miniconda3/"))
      venv-location (cdr
		     (assoc system-name venv-byhost))
      default-venv-byhost
      '(("kimura" . "~/.virtualenvs/python3_9")
	("fisher" . "~/.virtualenvs/default")
	("ovh" . "~/.virtualenvs/default")
	("alarmpi" . "~/.virtualenvs/default")
	("alarmpi-4b" . "~/.virtualenvs/default")
	("583-datascience.samba.sheffield.thefloow.com" . "~/.miniconda3/"))
      default-venv (cdr
		     (assoc system-name default-venv-byhost))
      elpy-rpc-python-command "python3"
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt"
      python-environment-directory venv-location)

;; Activate virtual environment based on location (set above)
;; (pyvenv-activate venv-location)
(use-package pyvenv
  :ensure t
  :defer t
  :config
  (pyvenv-mode t)
  ;; (pyvenv-activate default-venv) ;; Causes : env-diff (("SHLVL" . "1")) ???
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
               (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
		(setq python-shell-interpreter "python3")))))

;; Provide LSP-mode for python, it requires a language server.
;; I use `lsp-pyright`. Know that you have to `M-x lsp-restart-workspace`
;; if you change the virtual environment in an open python buffer.
(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (python-mode . lsp-deferred))

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
(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-doc-delay 2
	lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25
	lsp-ui-sideline-enable nil)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ("C-c i" . lsp-ui-imenu)))

;; Integration with the debug server
(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; Built-in Python utilities
(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))

;; Hide the modeline for inferior python processes
(use-package inferior-python-mode
  :ensure nil
  :hook (inferior-python-mode . hide-mode-line-mode))

;; Required to hide the modeline
(use-package hide-mode-line
  :ensure t
  :defer t)

;; Required to easily switch virtual envs
;; via the menu bar or with `pyvenv-workon`
;; Setting the `WORKON_HOME` environment variable points
;; at where the envs are located. I use miniconda.
(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs/"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
					  (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

;; Language server for Python
;; Read the docs for the different variables set in the config.
(use-package lsp-pyright
  :ensure t
  :defer t
  :config
  (setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
  (setq lsp-pyright-disable-language-service nil
	lsp-pyright-disable-organize-imports nil
	lsp-pyright-auto-import-completions t
	lsp-pyright-use-library-code-for-types t
	lsp-pyright-venv-path "~/miniconda3/envs")
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright) (lsp-deferred)))))

;; Format the python buffer following YAPF rules
;; There's also blacken if you like it better.
(use-package yapfify
  :ensure t
  :defer t
  :hook (python-mode . yapf-mode))
