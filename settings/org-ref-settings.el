;;; ORG-REF CONFIGURATION
;;; --------------------------------------
;;; And other citation related settings

;; org-ref https://github.com/jkitchin/org-ref
(use-package org-ref
  :requires
  org-ref-helm
  :config
  (setq bibtex-completion-bibliography '("~/org/references.bib"
                                         "~/org/rse/references.bib")
        bibtex-completion-library-path '("~/work/ref/")))

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
