;;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-font-lock-mode 1)
(setq org-directory        "~/org/")
(setq org-startup-indented 1)
;; Enable org-mode agenda/emacs diary integration
(setq org-agenda-include-diary t)

;; Set time on changing TODO stats
(setq org-log-done 'time)

;; Define conversion
(defmath uconvert (v u)
  "Convert value V to compatible unit U."
  (math-convert-units v u))

;; org-trello for thefloow.org
(require 'org-trello)
(custom-set-variables '(org-trello-files '("~/org/thefloow.org")))

;; Cliplink setup
;; From : https://github.com/rexim/org-cliplink
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

;; Hook - Reftex (i.e. runs the above!)
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
(require 'org-ref)
;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/org/ref-notes.org"
      org-ref-default-bibliography '("~/org/references.bib")
      org-ref-pdf-directory "~/work/ref/")

;; org-babel
;;
;; Set up evaluation languages
(org-babel-do-load-languages
 'org-babel-load-languages '((R . t)
			     (python . t)))
;; Set defaults...
;; In-line images by default (https://emacs.stackexchange.com/a/21267/10100)
(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)
;; Turn off code evaluation confirmation (https://emacs.stackexchange.com/a/3570/10100)
(setq org-confirm-babel-evaluate nil)
;; Use the default virtualenvironment
(setq org-babel-python-command "~/.virtualenvs/default/bin/python")

;; Default LaTeX formatting (https://github.com/erikriverson/org-mode-R-tutorial/blob/master/org-mode-R-tutorial.org#inserting-r-graphical-output)
(setq org-format-latex-options
      '(:foreground default
	:background "rgb 1 1 1"
        :scale 1.5
        :html-foreground "Black"
	:html-background "Transparent"
        :html-scale 1.0
        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))


;; Header skeleton
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
