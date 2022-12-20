;;; Enable flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;;; Define custom pylintrc file for checking Python code
;;(defcustom flycheck-pylintrc)
