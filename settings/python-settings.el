;; PYTHON CONFIGURATION
;; --------------------------------------
;; elpy configuration
(elpy-enable)
(setq elpy-rpc-backend "jedi")


;;; Virtual Environment Setuip
;; Define location of virtual environments by host and set venv-location
(setq virtualenv-byhost
      '(("kimura" . "~/.virtualenvs/python3_9")
	("fisher" . "~/.virtualenvs/default")
	("ovh" . "~/.virtualenvs/default")
	("alarmpi" . "~/.virtualenvs/default")
	("alarmpi-4b" . "~/.virtualenvs/default")
	("583-datascience.samba.sheffield.thefloow.com" . "~/.miniconda3/"))
      venv-location (cdr
		     (assoc system-name virtualenv-byhost))
      elpy-rpc-python-command "python3"
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt"
      python-environment-directory venv-location)

;; Activate virtual environment based on location (set above)
(pyvenv-activate venv-location)

;;; pyvenv and Jedi setup/hooks for Python mode
(add-hook 'python-mode-hook 'jedi:setup)

;; use flycheck not flymake with elpy
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))
;; Add flycheck-mypy to ensure static typing
(use-package flycheck-mypy
  :defer 3
  :hook (python-mode-hook 'flycheck-mode)
  :init
    (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    (add-to-list 'flycheck-disabled-checkers 'python-pylint))
;; Keymaps to navigate to the errors (under flymake)
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))

;; Try to add pylint rules https://emacs.stackexchange.com/a/41048/10100
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq flycheck-python-pylint-executable "~/.virtualenvs/default/bin/pylint")
	    (setq flycheck-pylintrc "~/.config/emacs/settings/.pylintrc")))
;; enable autopep8 formatting on save
(use-package py-autopep8
  :defer 3
  :hook (elpy-mode-hook 'py-autopep8-enable-on-save))
;; (use-package py-yapf
;;   :defer 3)
;; (add-hook 'python-mode-hook 'py-yapf-enable-on-save)
;; (use-package blacken
;;   :defer 3)
;; (add-hook 'python-mode-hook 'blacken-mode)
;; (add-hook 'python-mode-hook 'yapf-mode)

;; pytest
(use-package pytest:
  :defer 3)
