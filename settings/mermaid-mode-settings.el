;;; MERMAID MODE
;;; https://github.com/abrochard/mermaid-mode
;;; https://github.com/arnm/ob-mermaid

(use-package mermaid-mode
	     :ensure t
	     :config
	     (setq mermaid-mmdc-location '("~/.local/bin/mmdc")))

(use-package ob-mermaid)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (scheme . t)
   (your-other-langs . t)))
