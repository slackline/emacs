;; PYTHON CONFIGURATION
;; --------------------------------------

;; elpy configuration
(elpy-enable)
(setq elpy-rpc-backend "jedi")  
(pyvenv-activate "~/.virtualenvs/default")

;; Set ipython as the default interpreter
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; pyvenv and Jedi setup/hooks for Python mode
(setq venv-location (expand-file-name "~/.virtualenvs"))
(setq python-environment-directory venv-location)
;;(add-hook 'python-mode-hook 'jedi:setup)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; Keymaps to navigate to the errors (under flymake)
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))
;; Try to add pylint rules https://emacs.stackexchange.com/a/41048/10100
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq flycheck-python-pylint-executable "~/.virtualenvs/dskit/bin/pylint")
	    (setq flycheck-pylintrc "~/.emacs.d/settings/.pylintrc")))
;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;;(require 'py-yapf)
;;(add-hook 'python-mode-hook 'py-yapf-enable-on-save)
;; (require 'blacken)
;; (add-hook 'python-mode-hook 'blacken-mode)

;; pytest
(require 'pytest)
