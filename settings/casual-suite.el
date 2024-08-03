;;; CASUAL SUITE
;;; http://yummymelon.com/devnull/announcing-casual-suite.html
;;; https://github.com/kickingvegas/casual-suite
(use-package casual-suite
  :ensure t)

;; https://github.com/kickingvegas/casual-isearch
(use-package casual-isearch
  :ensure t)

;; https://github.com/kickingvegas/casual-dired
(use-package casual-dired
  :ensure t)

;; https://github.com/kickingvegas/casual-info
(use-package casual-dired
  :ensure t)

;; https://github.com/kickingvegas/casual-calc
(use-package casual-calc
  :ensure t)

;; https://github.com/kickingvegas/casual-avy
(use-package casual-avy
  :ensure t)

;; https://github.com/kickingvegas/casual-ibuffer
(use-package casual-ibuffer
  :ensure t)

;; https://github.com/kickingvegas/casual-re-builder
(use-package re-builder
  :defer t)
(use-package casual-re-builder
  :ensure t
  :bind (:map
         reb-mode-map ("C-o" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu))
  :after (re-builder))
