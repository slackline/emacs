;; PYTHON CONFIGURATION
;; --------------------------------------

;; Enable auto-fill-mode for Markdown files (https://emacs.stackexchange.com/a/46980/10100)

(add-to-list 'auto-mode-alist
	     '("\\.md\\'" . auto-fill-mode))
(setq-default fill-column 120)
