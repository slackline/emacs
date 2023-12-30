;; HIGHLIGHT INDENT GUIDES CONFIGURATION
;; --------------------------------------
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode)
  )
