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
  (setq pyvenv-menu t
	venv-byhost
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
	  ("fisher" . "~/.virtualenvs/python3_9")
	  ("ovh" . "~/.virtualenvs/default")
	  ("alarmpi" . "~/.virtualenvs/default")
	  ("alarmpi-4b" . "~/.virtualenvs/default")
	  ("583-datascience.samba.sheffield.thefloow.com" . "~/.miniconda3/"))
	default-venv (cdr
		      (assoc system-name default-venv-byhost))
	python-environment-directory venv-location)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
					  (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

;; Format the python buffer following YAPF rules
;; There's also blacken if you like it better.
(use-package yapfify
  :ensure t
  :defer t
  :hook (python-mode . yapf-mode))
