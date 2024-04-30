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

;; https://github.com/LionyxML/markdown-ts-mode
;; (use-package markdown-ts-mode
;;   :mode ("\\.md\\'" . markdown-ts-mode)
;;   :defer 't
;;   :config
;;   (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/ikatyang/tree-sitter-markdown" "master"
;; "src")))
