;;; CASUAL SUITE
;;; http://yummymelon.com/devnull/announcing-casual-suite.html
;;; https://github.com/kickingvegas/casual-suite
(use-package casual-suite
  :ensure t
  :bind (:map
         reb-mode-map ("C-o" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu)))
;; :after (re-builder)
