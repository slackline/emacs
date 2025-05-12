;; MMM CONFIGURATION
;; --------------------------------------
;; Load Polymode https://github.com/purcell/mmm-mode
(use-package mmm-auto
  :ensure t
  :init
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'org-mode "\\.py\\'" 'org-py)
  (mmm-add-mode-ext-class 'org-mode "\\.R\\'" 'org-R)  )
