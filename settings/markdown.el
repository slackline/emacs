;; MARKDOWN CONFIGURATION
;; --------------------------------------
;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :hook (markdown-mode . auto-fill-mode)
  :init (setq markdown-command "pandoc")
  :config
  :bind (:map markdown-mode-map
              ("C-c C-m" . markdown-do)))
