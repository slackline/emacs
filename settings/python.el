;;; PYTHON CONFIGURATION
;;; --------------------------------------

;; Hide the modeline for inferior python processes
(use-package inferior-python-mode
  :ensure nil
  :hook (inferior-python-mode . hide-mode-line-mode))

;; Required to easily switch virtual envs
;; via the menu bar or with `pyvenv-workon`
;; Setting the `WORKON_HOME` environment variable points
;; at where the envs are located.
;;
;; https://github.com/jorgenschaefer/pyvenv
(use-package pyvenv
  :ensure t
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
  :hook (python-mode . pyvenv-mode)
  (after-init-hook . (pyvenv-workon default-env))
  )

;; Built-in Python utilities
(use-package python
  :after (pyvenv)
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil
	python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"
	python-environment-directory venv-location)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
  :bind (:map python-mode-map
	      ("C-c p t" . python-pytest-dispatch)
	      ("C-c p l" . pylint)
	      ("C-c p y" . pylint-insert-ignore-comment)
	      ("C-c p n" . numpydoc-generate)
	      ("C-c p b" . blacken-buffer)
	      ("C-c p v" . pyvenv-workon)
	      ("C-c p T c" . python-skeleton-class)
	      ("C-c p T d" . python-skeleton-def)
	      ("C-c p T f" . python-skeleton-for)
	      ("C-c p T i" . python-skeleton-if)
	      ("C-c p T m" . python-skeleton-import)
	      ("C-c p T t" . python-skeleton-try)
	      ("C-c p T w" . python-skeleton-while)))

;; https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest
  :after (pyvenv)
  :ensure t
  :defer 2)

;; https://github.com/emacsorphanage/pylint
(use-package pylint
  :hook (python-mode . pylint-add-menu)
  (python-mode . add-key-bindings))

;;; Linting - Lots of options, currently going with blacken
;; https://github.com/pythonic-emacs/blacken
(use-package blacken
  :ensure t
  :defer 3
  :custom
  (blacken-line-length 120)
  :hook (python-mode . blacken-mode))

;; https://github.com/erickgnavar/flymake-ruff
(use-package flymake-ruff
  :ensure t
  :defer 3
  :hook (python-mode . flymake-ruff-load))

;; https://github.com/douglasdavis/numpydoc.el
(use-package numpydoc
  :ensure t
  :defer t
  :after lsp
  :custom
  (numpydoc-prompt-for-input t)
  (numpydoc-insert-examples-block nil))


;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein
  :ensure t
  :defer t
  :config
  (setq ein:output-area-inlined-images t
	ein:polymode t
	ein:query-timeout 100000)
  :bind (:map ein:notebook-mode
	      ("C-c e r" . ein:notebook-rename-command)
	      ("C-c e x" . ein:notebook-restart-session-command)
	      ("C-c e a" . ein:worksheet-execute-all-cells)
	      ("C-c e <down>" . ein:worksheet-move-cell-down-km)
	      ("C-c e <up>" . ein:worksheet-move-cell-up-km)
	      ("C-c e d" . ein:worksheet-delete-cell)
	      ("C-c e u" . ein:worksheet-execute-all-cells-above)
	      ("C-c e b" . ein:worksheet-execute-all-cells-below)))

;; https://github.com/paetzke/py-autopep8.el
;; (use-package py-autopep8
;;   :defer 3
;;   :hook (python-mode . py-autopep8-enable-on-save))

;; (use-package yapfify
;;   :ensure t
;;   :defer t
;;   :hook (python-mode . yapf-mode))

;; https://github.com/cybniv/poetry.el
;; (use-package poetry
;; 	     :ensure t
;; 	     :defer t
;; 	     :after lsp)
