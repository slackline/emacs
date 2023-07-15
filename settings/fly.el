;;; Settings for flycheck, flymake and flyspell
;;; https://www.flycheck.org/en/latest/
(use-package flycheck
	     :ensure t
	     :init (global-flycheck-mode))
;;; Define custom pylintrc file for checking Python code
;;(defcustom flycheck-pylintrc)

;;; Flyspell for automatic spell checking
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook
                R-mode-hook))
  (add-hook mode
            #'(lambda ()
		(flyspell-prog-mode))))
