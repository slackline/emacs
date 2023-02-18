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
		   '(("kimura" . "~/.virtualenvs/python3_10")
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
	     :bind (:map python-mode-map
			 ("C-c p t" . python-pytest-dispatch)
			 ("C-c p l" . pylint)
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
	     :defer 3)

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
	     :defer t)

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
