;;; CASUAL SUITE
;;; http://yummymelon.com/devnull/announcing-casual-suite.html
;;; https://github.com/kickingvegas/casual-suite
(use-package casual-suite
  :ensure t
  :bind (:map
         reb-mode-map ("s-c" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("s-c" . casual-re-builder-tmenu)))
;; :after (re-builder)
