;;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-font-lock-mode 1)
(setq org-directory        "~/org/"
      org-agenda-files '("~/org/daily_tasks.org")
      org-roam-directory "~/org"
      org-startup-indented 1
      org-agenda-include-diary t
      org-log-done 'time
      org-image-actual-width nil                                   ;; https://stackoverflow.com/a/38477233/1444043
      org-export-backends '(beamer html latex md odt)
      org-startup-with-inline-images t                             ;; https://emacs.stackexchange.com/a/21267/10100
      org-confirm-babel-evaluate nil                               ;; https://emacs.stackexchange.com/a/3570/10100
      org-babel-python-command "~/.virtualenvs/default/bin/python"
      org-format-latex-options ;; https://github.com/erikriverson/org-mode-R-tutorial/blob/master/org-mode-R-tutorial.org#inserting-r-graphical-output
          '(:foreground default
	    :background "rgb 1 1 1"
            :scale 1.5
            :html-foreground "Black"
	    :html-background "Transparent"
            :html-scale 1.0
            :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

;; Define conversion
(defmath uconvert (v u)
  "Convert value V to compatible unit U."
  (math-convert-units v u))

;; org-cliplink (why doesn't this work with binding in init.el?)
(global-set-key (kbd "C-x p i") 'org-cliplink)

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

;; Hook - Insert created date when adding a header
;; From - https://stackoverflow.com/a/37478674
;; (require 'org-expiry)
;; (add-hook 'org-after-todo-state-change-hook
;;           (lambda ()
;;             (when (string= org-state "TODO")
;;               (save-excursion
;;                 (org-back-to-heading)
;;                 (org-expiry-insert-created)))))

;; org-present
(autoload 'org-present "org-present" nil t)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

;; Org-reveal
(use-package ox-reveal
  :ensure t
;;  :defer t
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/"
	org-reveal-mathjax t))

;; org-ref (https://github.com/jkitchin/org-ref)
(use-package org-ref
  :init
  (setq org-ref-bibliography-notes "~/org/ref-notes.org"
	org-ref-default-bibliography '("~/org/references.bib")
	org-ref-pdf-directory "~/work/ref/"
	bibtex-completion-bibliography "~/org/references.bib"
	bibtex-completion-library-path "~/work/ref/"
	bibtex-completion-notes-path "~/org/helm-bibtex-notes"
	org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

;; org-babel
;;
;; Set up evaluation languages
(org-babel-do-load-languages
 'org-babel-load-languages '((R . t)
			     (python . t)))
;; Hooks for in-line images (https://emacs.stackexchange.com/a/21267/10100)
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;;; Hook to rsync html output to OVH on export
(add-hook 'org-html-export-to-html 'rsync-html)
