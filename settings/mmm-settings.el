;; MMM CONFIGURATION
;; --------------------------------------
;; Load Polymode https://github.com/purcell/mmm-mode
(require 'mmm-auto)

(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'org-mode "\\.py\\'" 'org-py)
(mmm-add-mode-ext-class 'org-mode "\\.R\\'" 'org-R)
