;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure t
  :defer 0.5
  :init
  (setq rust-mode-treesitter-derive t)
  ;; :hook
  ;; (rust-mode . (lambda () (prettify-symbols-mode)))
  )

;; https://github.com/ayrat555/cargo-mode
(use-package cargo-mode
  :ensure t
  :defer 0.5
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (setq compulation-scroll-output t))
