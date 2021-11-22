;; MARKDOWN CONFIGURATION
;; --------------------------------------
;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;; Enable auto-fill-mode for Markdown files (https://emacs.stackexchange.com/a/46980/10100)
(add-to-list 'auto-mode-alist
     '("\\.md\\'" . auto-fill-mode))
