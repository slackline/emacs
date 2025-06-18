;;; CASUAL SUITE
;;; http://yummymelon.com/devnull/announcing-casual-suite.html
;;; https://github.com/kickingvegas/casual-suite
(use-package casual-suite
  :ensure t
  :bind (:map
         reb-mode-map ("s-c" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("s-c" . casual-re-builder-tmenu)
         :map
         ibuffer-mode-map))
(use-package casual-dired
  :ensure nil
  :bind (:map dired-mode-map ("C-o" . causal-dired-tmenu)))
(use-package casual-ibuffer
  :ensure nil
  :after (ibuffer)
  :bind (:map ibuffer-mode-map ("C-o" . causal-ibuffer-tmenu)))
;; :after (re-builder)
