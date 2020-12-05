;;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-font-lock-mode 1)
(setq org-directory        "~/org/"
      org-agenda-files '("~/org/daily_tasks.org")
      org-roam-directory "~/org"
      org-startup-indented 1
      org-agenda-include-diary t
      org-log-done 'time
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
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)


;; Skeleton
(define-skeleton org-skeleton
  "Header info for a emacs-org file."
  "Title: "
  "#+TITLE:" str " \n"
  "#+AUTHOR: Neil Shephard\n"
  "#+EMAIL: nshephard@gmail.com\n"
  "#+PROPERTY: header-args:R  :session *org-R*\n"
  "#+PROPERTY: header-args:R  :cache yes\n"
  "#+PROPERTY: header-args:R  :results graphics\n"
  "#+PROPERTY: header-args:R  :width 1024\n"
  "#+PROPERTY: header-args:R  :height 768\n"
  "#+PROPERTY: header-args:R  :tangle yes\n"
  "#+INFOJS_OPT: \n"
  "#+BABEL:  :session *org-R*  :cache yes  :exports both  :results output graphics  :tangle yes  :width 1024  :height 768 \n"
  "-----"
 )
(global-set-key [C-S-f4] 'org-skeleton)

;; Default header arguments
(add-to-list 'org-babel-default-header-args
             '(:AUTHOR . "Neil Shephard")
	     '(:EMAIL . "nshephard@gmail.com")
	     )
(add-to-list 'org-babel-default-header-args:R
             '(:session . "*org-R*")
	     )
;; 	       (:width . 1024) (:height . 768)
;; 	       (:cache . "yes")
;; 	       (:results . "output graphics")
;; 	       (:exports . "both")
;; 	       (:tangle . "yes")
;; 	      ))
(add-to-list 'org-babel-default-inline-header-args
             '(:colnames . "nil"))

;; Embed CSS (https://stackoverflow.com/a/37132338)
(defun org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/org/solarized-dark.css" path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))
(add-hook 'org-export-before-processing-hook 'org-inline-css-hook)

;; org-roam
;; (use-package org-roam
;;       :ensure t
;;       :hook
;;       (after-init . org-roam-mode)
;;       :custom
;;       (org-roam-directory "~/org")
;;       :bind (:map org-roam-mode-map
;;               (("C-c n l" . org-roam)
;;                ("C-c n f" . org-roam-find-file)
;;                ("C-c n g" . org-roam-graph-show))
;;               :map org-mode-map
;;               (("C-c n i" . org-roam-insert))
;;               (("C-c n I" . org-roam-insert-immediate))))
