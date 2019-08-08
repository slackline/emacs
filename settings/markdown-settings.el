;; PYTHON CONFIGURATION
;; --------------------------------------

;; Enable auto-fill-mode for Markdown files (https://emacs.stackexchange.com/a/46980/10100)

; (add-to-list 'auto-mode-alist
;	     '("\\.md\\'" . auto-fill-mode))
(autoload 'markdown-mode "markdown-mode.el" 
	"Major mode for editing Markdown files" t) 
	(setq auto-mode-alist 
		(cons '("\\.md" . markdown-mode) auto-mode-alist)
	)
(setq-default fill-column 120)
