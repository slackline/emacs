;;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-font-lock-mode 1)
(setq org-directory        "~/work/org/")
(setq org-startup-indented 1)
;; Enable org-mode agenda/emacs diary integration
(setq org-agenda-include-diary t)

;; Set time on changing TODO stats
(setq org-log-done 'time)

;; Define conversion
(defmath uconvert (v u)
  "Convert value V to compatible unit U."
  (math-convert-units v u))

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
(setq reftex-default-bibliography '("~/dotfiles/ref/citeulike_export_20180712.bib"))
;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/work/org/ref-notes.org"
      org-ref-default-bibliography '("~/dotfiles/ref/citeulike_export_20180712.bib")
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

;; org-roam
;; (use-package org-roam
;;       :ensure t
;;       :hook
;;       (after-init . org-roam-mode)
;;       :custom
;;       (org-roam-directory "~/work/org")
;;       :bind (:map org-roam-mode-map
;;               (("C-c n l" . org-roam)
;;                ("C-c n f" . org-roam-find-file)
;;                ("C-c n g" . org-roam-graph-show))
;;               :map org-mode-map
;;               (("C-c n i" . org-roam-insert))
;;               (("C-c n I" . org-roam-insert-immediate))))

;; org-capture (https://orgmode.org/manual/Capture.html#Capture)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Stuff ToDo in my Life")
	("th" "House Tasks" entry (file+olp "~/work/org/todo.org" "House")
         "* TODO %?\n")
	("tg" "Gardening" entry (file+olp "~/work/org/todo.org" "Garden")
         "* TODO %?\n")
	("tv" "Campervan" entry (file+olp "~/work/org/todo.org" "Campervan")
         "* TODO %?\n")
	("tc" "Car" entry (file+olp "~/work/org/todo.org" "Car")
         "* TODO %?\n")
	("tg" "Bike" entry (file+olp "~/work/org/todo.org" "Bike")
         "* TODO %?\n")
	("ts" "Stuff to Sell" entry (file+olp "~/work/org/todo.org" "Stuff To Sell")
         "* TODO %?\n")
	("c" "Computing")
	("ce" "Emacs" entry (file+olp "~/work/org/computing.org" "Emacs")
         "* TODO %?\n")
	("cl" "Laptop" entry (file+olp "~/work/org/computing.org" "Laptop")
         "* TODO %?\n")
	("cd" "Desktop" entry (file+olp "~/work/org/computing.org" "Desktop")
         "* TODO %?\n")
	("cn" "Networking/Routers" entry (file+olp "~/work/org/computing.org" "Networking/Routers")
         "* TODO %?\n")
	("cq" "Printers" entry (file+olp "~/work/org/computing.org" "Printers")
         "* TODO %?\n")
	("cr" "RaspberryPi" entry (file+olp "~/work/org/computing.org" "Raspberry Pi")
         "* TODO %?\n")
	("cm" "AirQuality Monitor" entry (file+olp "~/work/org/computing.org" "AirQuality Monitor")
         "* TODO %?\n")
	("ca" "Android" entry (file+olp "~/work/org/computing.org" "Android")
         "* TODO %?\n")
	("cp" "Programming")
	("cpb" "" entry (file+olp "~/work/org/computing.org" "Programming" "Bash")
         "* TODO %?\n")
	("cpe" "MongoDB" entry (file+olp "~/work/org/computing.org" "Programming" "MongoDB")
         "* TODO %?\n")
	("cpp" "Python" entry (file+olp "~/work/org/computing.org" "Programming" "Python")
         "* TODO %?\n")
	("cpg" "Git" entry (file+olp "~/work/org/computing.org" "Programming" "Git")
         "* TODO %?\n")
	("cpl" "LaTeX" entry (file+olp "~/work/org/computing.org" "Programming" "LaTeX")
         "* TODO %?\n")
	("cpr" "LaTeX" entry (file+olp "~/work/org/computing.org" "Programming" "R")
         "* TODO %?\n")
	("cv" "VPS" entry (file+olp "~/work/org/computing.org" "VPS")
         "* TODO %?\n")
	("e" "Exercise Workouts")
        ("er" "Logging a run" table-line (file+olp "~/work/org/training.org" "Workouts" "Running")
	 "| %t | %? | | | | |" :prepend t)
	("ec" "Logging a cycle" table-line (file+olp "~/work/org/training.org" "Workouts" "Cycling")
	 "| %t | %? | | | | |" :prepend t)
	("eh" "Logging a hike" table-line (file+olp "~/work/org/training.org" "Workouts" "Hiking")
	 "| %t | %? | | | | |" :prepend t)
	("em" "Logging metrics" table-line (file+olp "~/work/org/training.org" "Workouts" "Metrics")
	 "| %t | %? | | | |" :prepend t)
	("es" "Training Resources")
	("esr" "Running" entry (file+olp "~/work/org/training.org" "Resources" "Running")
         "* TODO %?\n")
	("ess" "Calisthenics" entry (file+olp "~/work/org/training.org" "Resources" "Calisthenics")
         "* TODO %?\n")
	("esc" "Training Resources (Climbing)")
	("escm" "Miscellaneous" item (file+olp "~/work/org/training.org" "Resources" "Climbing" "Miscellaneous")
          "+  %?\n")
	("escf" "Fingerboarding" item (file+olp "~/work/org/training.org" "Resources" "Climbing" "Fingerboarding")
         "+  %?\n")
	("escs" "Shauna Coxy" item (file+olp "~/work/org/training.org" "Resources" "Climbing" "Shauna Coxy")
         "+  %?\n")
	("w" "Work")
	("wp" "Work Projects" table-line (file+olp "~/work/org/thefloow.org" "Project")
	 "** TODO %t %?\n")
	("ww" "ToDo Tasks" table-line (file+olp "~/work/org/thefloow.org" "ToDo Tasks")
	 "** TODO %t %?\n")
	("o" "Cooking")
	("ov" "Vegetarian" item (file+olp "~/work/org/cooking.org" "Vegetarian")
         "+ %?\n")
	("ow" "Web sites" item (file+olp "~/work/org/cooking.org" "Web Sites")
         "+ %?\n")
	("ou" "Indian")
	("ous" "Starters" item (file+olp "~/work/org/cooking.org" "Indian" "Starters")
         "+ %?\n")
	("ouc" "Curries" item (file+olp "~/work/org/cooking.org" "Indian" "Curries")
         "+ %?\n")
	("oub" "Breads" item (file+olp "~/work/org/cooking.org" "Indian" "Breads")
         "+ %?\n")
	("oh" "Chinese")
	("ohs" "Starters" item (file+olp "~/work/org/cooking.org" "Chinese" "Starters")
         "+ %?\n")
	("ohr" "Rice" item (file+olp "~/work/org/cooking.org" "Chinese" "Rice")
         "+ %?\n")
	("ohn" "Noodles" item (file+olp "~/work/org/cooking.org" "Chinese" "Noodles")
         "+ %?\n")
	("oi" "Italian")
	("oip" "Pasta" item (file+olp "~/work/org/cooking.org" "Italian" "Pasta")
         "+ %?\n")
	("oiz" "Pizza" item (file+olp "~/work/org/cooking.org" "Italian" "Pizza")
         "+ %?\n")
	("os" "Spanish" item (file+olp "~/work/org/cooking.org" "Italian" "Pasta")
         "+ %?\n")
	("ob" "Books" item (file+olp "~/work/org/cooking.org" "Books")
         "+ %?\n")))
