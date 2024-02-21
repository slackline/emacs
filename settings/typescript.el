;;; http://github.com/ananthakumaran/typescript.el
(use-package typescript-mode
  :ensure t
  :defer 0.5
  :config
  (add-to-list 'auto-mode-alist '("\\.svelte" . typescript-mode)))
