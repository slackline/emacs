;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
;; Set ipython as the default interpreter
(setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt")

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; Try to add pylint rules https://emacs.stackexchange.com/a/41048/10100
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq flycheck-python-pylint-executable "~/.local/bin/pylint")
	    (setq flycheck-pylintrc "~/.emacs.d/settings/.pylintrc")))
;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
