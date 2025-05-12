;;; ORG-REF CONFIGURATION
;;; --------------------------------------
;;; And other citation related settings

;; org-ref https://github.com/jkitchin/org-ref
(use-package org-ref
  :ensure t
  :requires
  org-ref-helm
  :config
  (setq bibtex-completion-bibliography '("~/org/references.bib"
					 "~/org/rse/references.bib")
	bibtex-completion-library-path '("~/work/ref/")
	bibtex-completion-notes-path "~/org/helm-bibtex-notes"
	bibtex-completion-display-formats  '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
					     (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
					     (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
					     (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
					     (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	org-ref-bibliography-notes "~/org/ref-notes.org"
	org-ref-default-bibliography '("~/org/references.bib")
	org-ref-pdf-directory "~/work/ref/"
	org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

;; RefTex setup
;; From : https://blog.karssen.org/2013/08/22/using-bibtex-from-org-mode/
;;        http://www.mfasold.net/blog/2009/02/using-emacs-org-mode-to-draft-papers/
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  )
(add-hook 'org-mode-hook 'org-mode-reftex-setup)
