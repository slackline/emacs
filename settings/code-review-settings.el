;; CODE REVIEW CONFIGURATION
;; --------------------------------------
;; https://github.com/wandersoncferreira/code-review
(use-package code-review
	     :defer 0.5
	     :init
	     )

;; Code review at point
(define-key forge-topic-mode-map (kbd "C-c R") 'code-review-forge-pr-at-point)
