;;; ORG-MODE CONFIGURATION
;;; --------------------------------------
(use-package org
  :ensure t
  :init
  :bind
  (("C-x p i" . 'org-cliplink))
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-font-lock-mode 1)
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
							   (python . t)
							   (shell . t)
							   (R . t)
							   (latex .t)))
  (setq org-directory        "~/org/"
	;; org-agenda-files '("~/org/agenda.org" "~/org/work/rse/todo.org" "~/org/gtd/org-gtd-tasks.org")
	org-agenda-files '("~/org/agenda.org"
 			   "~/org/gtd/carpentries.org"
 			   "~/org/gtd/clarity.org"
 			   "~/org/gtd/computing.org"
 			   "~/org/gtd/cured.org"
 			   "~/org/gtd/joss.org"
 			   "~/org/gtd/osc.org"
 			   "~/org/gtd/pgfinder.org"
                           "~/org/gtd/reproducibilitea.org"
			   "~/org/gtd/rse.org"
			   "~/org/gtd/rse-competencies.org"
			   "~/org/grd/sheffieldr.org"
 			   "~/org/gtd/tcx2gpx.org"
			   "~/org/gtd/thyroid.org"
			   "~/org/gtd/topostats.org")
	;; org-tags-alist '()
	org-startup-indented 1
	org-agenda-include-diary t
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
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
  ;; Disable electric-indent-mode in org buffers
  :hook (org-mode . (lambda () (electric-indent-local-mode 0)))
  (org-mode . (lambda () (org-rainbow-tags-mode 1)))
  )


;; Set additional keywords (and colours) https://github.com/james-stoup/emacs-org-mode-tutorial#orga87f491=
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "BLOCKED(b@)"  "|" "DONE(d!)" "WONT-DO(w@/!)" )
        ))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "GoldenRod" :weight bold))
        ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
        ("BLOCKED" . (:foreground "Red" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("WONT-DO" . (:foreground "DarkViolet" :weight bold))
        ))

;; https://github.com/rksm/clj-org-analyzer/
(use-package org-analyzer
  :ensure t
  :defer 3
  )
;; Swap backtick & tilde https://twitter.com/iLemming/status/1516930099148472321
;; (define-key org-mode-map (kbd "`")
;;     (lambda ()
;;         (interactive)
;;         (self-insert-command 1 126)))

;; Define conversion
(defmath uconvert (v u)
  "Convert value V to compatible unit U."
  (math-convert-units v u))

;; org-cliplink (why doesn't this work with binding in init.el?)
(global-set-key (kbd "C-x p i") 'org-cliplink)


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

;; Org rainbow tags
;; https://github.com/KaratasFurkan/org-rainbow-tags
(use-package org-rainbow-tags
  :ensure t
  :defer t)

;; https://github.com/jxq0/org-tidy
(use-package org-tidy
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-tidy-mode))

;; ox packages
(use-package ox-reveal
  :ensure t
  ;;  :defer t
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/"
	org-reveal-mathjax t))
(use-package ox-spectacle
  :ensure t
  :defer t)
(use-package ox-pandoc
  :ensure t
  :defer t)

;; Skeletons
(define-skeleton org-R-skeleton
  "Header info for a org file with R."
  "#+TITLE:" str " \n"
  "#+AUTHOR: Neil Shephard\n"
  "#+EMAIL: nshephard@protonmail.com\n"
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
(global-set-key [C-f4] 'org-R-skeleton)

(define-skeleton org-python-skeleton
  "Header info for a org file with Python."
  "#+TITLE:" str " \n"
  "#+AUTHOR: Neil Shephard\n"
  "#+EMAIL: nshephard@gmail.com\n"
  "#+PROPERTY: header-args:python  :session *org-python*\n"
  "#+PROPERTY: header-args:python  :cache yes\n"
  "#+PROPERTY: header-args:python  :results graphics\n"
  "#+PROPERTY: header-args:python  :width 1024\n"
  "#+PROPERTY: header-args:python  :height 768\n"
  "#+PROPERTY: header-args:python  :tangle yes\n"
  "#+INFOJS_OPT: \n"
  "#+BABEL:  :session *org-R*  :cache yes  :exports both  :results output graphics  :tangle yes  :width 1024  :height 768 \n"
  "-----"
  )
(global-set-key [C-f5] 'org-python-skeleton)

;; Default header arguments
(add-to-list 'org-babel-default-header-args
             '(:AUTHOR . "Neil Shephard")
	     '(:EMAIL . "nshephard@gmail.com")
	     )
;; (add-to-list 'org-babel-default-header-args:R
;;              '(:session . "*org-R*")
;; 	     )
;; 	       (:width . 1024) (:height . 768)
;; 	       (:cache . "yes")
;; 	       (:results . "output graphics")
;; 	       (:exports . "both")
;; 	       (:tangle . "yes")
;; 	      ))
(add-to-list 'org-babel-default-inline-header-args
             '(:colnames . "nil"))
;; Insert code blocks (https://emacs.stackexchange.com/a/12847)
;; OBSOLETE as of Org 9.2 see https://emacs.stackexchange.com/a/46992/10100
;; (add-to-list 'org-structure-template-alist
;;              '("r" "#+NAME: ?\n#+BEGIN_SRC R :session ** :eval yes :exports none :results output silent\n\n#+END_SRC"))
;; (add-to-list 'org-structure-template-alist
;;              '("p" "#+NAME: ?\n#+BEGIN_SRC Python :session ** :eval yes :exports none :results output silent\n\n#+END_SRC"))


;; Embed YouTube video iframe when exporting to HTML
;; http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))
