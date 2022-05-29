;; EBIB CONFIGURATION
;; --------------------------------------
;;
;; Manual : https://joostkremers.github.io/ebib/ebib-manual.html

(global-set-key "\C-ce" 'ebib)
;; (define-key 'LaTeX-mode-map "\C-cb" 'ebib-insert-citation)
(setq ebib-preload-bib-files '("~/work/org/references.bib"))
