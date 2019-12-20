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
            '(lambda ()
               (flyspell-prog-mode))))
