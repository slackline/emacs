;;; PYTHON CONFIGURATION
;;; --------------------------------------

;; Hide the modeline for inferior python processes
(use-package inferior-python-mode
	     :ensure nil
	     :hook (inferior-python-mode . hide-mode-line-mode))

;; Required to easily switch virtual envs
;; via the menu bar or with `pyvenv-workon`
;; Setting the `WORKON_HOME` environment variable points
;; at where the envs are located. I use miniconda.
(use-package pyvenv
	     :ensure t
	     :config
	     ;; Setting work on to easily switch between environments
	     (setenv "WORKON_HOME" (expand-file-name "~/.virtualenvs/"))
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
	     :hook (python-mode . pyvenv-mode))

;; Built-in Python utilities
(use-package python
	     :after (pyvenv)
	     :ensure t
	     :config
	     ;; Remove guess indent python message
	     (setq python-indent-guess-indent-offset-verbose nil
		   python-shell-interpreter "ipython"
		   python-shell-interpreter-args "-i --simple-prompt"
		   ;; python-environment-directory venv-location)
		   python-environment-directory venv-location)
	     ;; Use IPython when available or fall back to regular Python
	     ;; (cond
	     ;;  ((executable-find "ipython")
	     ;;   (progn
	     ;;     (setq python-shell-buffer-name "IPython")
	     ;;     (setq python-shell-interpreter "ipython")
	     ;;     (setq python-shell-interpreter-args "-i --simple-prompt")))
	     ;;  ((executable-find "python3")
	     ;;   (setq python-shell-interpreter "python3"))
	     ;;  ((executable-find "python2")
	     ;;   (setq python-shell-interpreter "python2"))
	     ;;  (t
	     ;;   (setq python-shell-interpreter "python")))
	     )

;;; Python pytest (https://github.com/wbolster/emacs-python-pytest)
(use-package python-pytest
	     :after (pyvenv)
	     :ensure t
	     :defer 2
	     :config
	     :bind (:map python-mode-map
			         ("C-c p t" . python-pytest-dispatch)
                     ("C-c p l" . pylint)))

;; Linting - Lots of options, currently going with blacken
;; (use-package py-autopep8
;;   :defer 3
;;   :hook (python-mode . py-autopep8-enable-on-save))
;; (use-package py-yapf
;;   :defer 3
;;   :hook (python-mode . py-yapf-enable-on-save))
(use-package blacken
	     :ensure t
	     :defer 3
	     :custom
	     (blacken-line-length 120)
	     :hook (python-mode . blacken-mode)
	     :bind (:map python-mode-map
			 ("C-c p b" . blacken-buffer)))
;; (use-package yapfify
;;   :ensure t
;;   :defer t
;;   :hook (python-mode . yapf-mode))

(use-package numpydoc
	     :ensure t
	     :defer t
	     :after lsp
	     :custom
	     (numpydoc-prompt-for-input t)
	     (numpydoc-insert-examples-block nil)
	     :bind (:map python-mode-map
			 ("C-c p n" . numpydoc-generate)))
