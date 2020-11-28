;; YAML CONFIGURATION
;; --------------------------------------
(use-package yaml-mode
  :defer 4
  :init
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
