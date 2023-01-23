;; HIGHLIGHT INDENT GUIDES CONFIGURATION
;; --------------------------------------
(use-package highlight-indent-guides
  :defer t)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(setq highlight-indent-guides-method 'character)
