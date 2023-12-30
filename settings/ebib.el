;; EBIB CONFIGURATION
;; --------------------------------------
;;
;; Manual : https://joostkremers.github.io/ebib/ebib-manual.html

;; (define-key 'LaTeX-mode-map "\C-cb" 'ebib-insert-citation)
(use-package ebib
  :config
  (setq ebib-preload-bib-files '("~/org/references.bib"))
  (global-set-key "\C-c x" 'ebib))
