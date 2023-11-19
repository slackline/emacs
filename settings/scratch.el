;;; https://codeberg.org/emacs-weirdware/scratch
;;;
;;; C-u M-x scratch prompts for which buffer mode to invoke
(use-package scratch
  :ensure t
  :defer 3
  :bind ("C-c s" . scratch))
