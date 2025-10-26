;; https://github.com/astoff/devdocs.el
(use-package devdocs
  :ensure t
  :defer 0.5
  :bind-keymap (("C-h D") . devocs-lookup))
