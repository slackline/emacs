;; JEDI CONFIGURATION
;; --------------------------------------

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

(custom-set-variables
 '(jedi:import-python-el-settings nil))
