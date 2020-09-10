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

;; org-trello for thefloow.org
(require 'org-trello)
(custom-set-variables '(org-trello-files '("~/work/org/thefloow.org")))

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
      '(("t" "Stuff ToDo in my Life") ;; TODO
	("th" "House Tasks" entry (file+olp "~/work/org/todo.org" "House")
         "* TODO %?\n" :prepend t)
	("tg" "Gardening" entry (file+olp "~/work/org/todo.org" "Garden")
         "* TODO %?\n" :prepend t)
	("tv" "Campervan" entry (file+olp "~/work/org/todo.org" "Campervan")
         "* TODO %?\n" :prepend t)
	("tc" "Car" entry (file+olp "~/work/org/todo.org" "Car")
         "* TODO %?\n" :prepend t)
	("tg" "Bike" entry (file+olp "~/work/org/todo.org" "Bike")
         "* TODO %?\n" :prepend t)
	("ts" "Stuff to Sell" entry (file+olp "~/work/org/todo.org" "Stuff To Sell")
         "* TODO %?\n" :prepend t)
	("c" "Computing") ;; Computing
	("ce" "Emacs" entry (file+olp "~/work/org/computing.org" "Emacs")
         "* TODO %?\n" :prepend t)
	("cl" "Laptop" entry (file+olp "~/work/org/computing.org" "Laptop")
         "* TODO %?\n" :prepend t)
	("cd" "Desktop" entry (file+olp "~/work/org/computing.org" "Desktop")
         "* TODO %?\n" :prepend t)
	("cn" "Networking/Routers" entry (file+olp "~/work/org/computing.org" "Networking/Routers")
         "* TODO %?\n" :prepend t)
	("cq" "Printers" entry (file+olp "~/work/org/computing.org" "Printers")
         "* TODO %?\n" :prepend t)
	("cr" "RaspberryPi" entry (file+olp "~/work/org/computing.org" "Raspberry Pi")
         "* TODO %?\n" :prepend t)
	("cm" "AirQuality Monitor" entry (file+olp "~/work/org/computing.org" "AirQuality Monitor")
         "* TODO %?\n" :prepend t)
	("ca" "Android" entry (file+olp "~/work/org/computing.org" "Android")
         "* TODO %?\n" :prepend t)
	("cp" "Programming") ;; Programming
	("cpb" "" entry (file+olp "~/work/org/computing.org" "Programming" "Bash")
         "* TODO %?\n" :prepend t)
	("cpe" "MongoDB" entry (file+olp "~/work/org/computing.org" "Programming" "MongoDB")
         "* TODO %?\n" :prepend t)
	("cpp" "Python" entry (file+olp "~/work/org/computing.org" "Programming" "Python")
         "* TODO %?\n" :prepend t)
	("cpg" "Git" entry (file+olp "~/work/org/computing.org" "Programming" "Git")
         "* TODO %?\n" :prepend t)
	("cpl" "LaTeX" entry (file+olp "~/work/org/computing.org" "Programming" "LaTeX")
         "* TODO %?\n" :prepend t)
	("cpr" "R" entry (file+olp "~/work/org/computing.org" "Programming" "R")
         "* TODO %?\n" :prepend t)
	("cv" "VPS" entry (file+olp "~/work/org/computing.org" "VPS")
         "* TODO %?\n" :prepend t)
	("e" "Exercise Workouts") ;; Workouts
        ("er" "Logging a run" table-line (file+olp "~/work/org/training.org" "Workouts" "Running")
	 "| %t | %? | | | | |" :prepend t)
	("ec" "Logging a cycle" table-line (file+olp "~/work/org/training.org" "Workouts" "Cycling")
	 "| %t | %? | | | | |" :prepend t)
	("eh" "Logging a hike" table-line (file+olp "~/work/org/training.org" "Workouts" "Hiking")
	 "| %t | %? | | | | |" :prepend t)
	("em" "Logging metrics" table-line (file+olp "~/work/org/training.org" "Workouts" "Metrics")
	 "| %t | %? | | | |" :prepend t)
	("es" "Training Resources") ;; Training Resources
	("esr" "Running" entry (file+olp "~/work/org/training.org" "Resources" "Running")
         "* TODO %?\n" :prepend t)
	("ess" "Calisthenics" entry (file+olp "~/work/org/training.org" "Resources" "Calisthenics")
         "* TODO %?\n" :prepend t)
	("esc" "Training Resources (Climbing)")
	("escm" "Miscellaneous" item (file+olp "~/work/org/training.org" "Resources" "Climbing" "Miscellaneous")
          "+  %?\n" :prepend t)
	("escf" "Fingerboarding" item (file+olp "~/work/org/training.org" "Resources" "Climbing" "Fingerboarding")
         "+  %?\n" :prepend t)
	("escs" "Shauna Coxy" item (file+olp "~/work/org/training.org" "Resources" "Climbing" "Shauna Coxy")
         "+  %?\n" :prepend t)
	("w" "Work") ;; Work
	("wp" "Work Projects" entry (file+olp "~/work/org/thefloow.org" "Project")
	 "** TODO %t %?\n" :prepend t)
	("ww" "ToDo Tasks" entry (file+olp "~/work/org/thefloow.org" "ToDo Tasks")
	 "** TODO %t %?\n" :prepend t)
	("o" "Cooking") ;; Cooking
	("ov" "Vegetarian" item (file+olp "~/work/org/cooking.org" "Vegetarian")
         "+ %?\n" :prepend t)
	("ow" "Web sites" item (file+olp "~/work/org/cooking.org" "Web Sites")
         "+ %?\n" :prepend t)
	("ou" "Indian")
	("ous" "Starters" item (file+olp "~/work/org/cooking.org" "Indian" "Starters")
         "+ %?\n" :prepend t)
	("ouc" "Curries" item (file+olp "~/work/org/cooking.org" "Indian" "Curries")
         "+ %?\n" :prepend t)
	("oub" "Breads" item (file+olp "~/work/org/cooking.org" "Indian" "Breads")
         "+ %?\n" :prepend t)
	("oh" "Chinese")
	("ohs" "Starters" item (file+olp "~/work/org/cooking.org" "Chinese" "Starters")
         "+ %?\n" :prepend t)
	("ohr" "Rice" item (file+olp "~/work/org/cooking.org" "Chinese" "Rice")
         "+ %?\n" :prepend t)
	("ohn" "Noodles" item (file+olp "~/work/org/cooking.org" "Chinese" "Noodles")
         "+ %?\n" :prepend t)
	("oi" "Italian")
	("oip" "Pasta" item (file+olp "~/work/org/cooking.org" "Italian" "Pasta")
         "+ %?\n" :prepend t)
	("oiz" "Pizza" item (file+olp "~/work/org/cooking.org" "Italian" "Pizza")
         "+ %?\n" :prepend t)
	("os" "Spanish" item (file+olp "~/work/org/cooking.org" "Italian" "Pasta")
         "+ %?\n" :prepend t)
	("ob" "Books" item (file+olp "~/work/org/cooking.org" "Books")
         "+ %?\n" :prepend t)
	("v" "Coronavirus") ;; Coronavirus
	("vi" "Information")
	("viu" "Useful Resources" item (file+olp "~/work/org/coronavirus.org" "Information" "Useful Resources")
         "+ %t %?\n" :prepend t)
	("viuc" "Collections/Streams")
	("viucn" "New Scientist" item (file+olp "~/work/org/coronavirus.org" "Information" "Useful Resources" "Collections/Streams" "New Scientist")
         "+ %t %?\n" :prepend t)
	("viuca" "The Atlantic" item (file+olp "~/work/org/coronavirus.org" "Information" "Useful Resources" "Collections/Streams" "The Atlantic")
         "+ %t %?\n" :prepend t)
	("viuco" "ONS" item (file+olp "~/work/org/coronavirus.org" "Information" "Useful Resources" "Collections/Streams" "ONS")
         "+ %t %?\n" :prepend t)
	("viucg" "The Guardian" item (file+olp "~/work/org/coronavirus.org" "Information" "Useful Resources" "Collections/Streams" "The Guardian")
         "+ %t %?\n" :prepend t)
	("viucd" "David Spiegelhalter" item (file+olp "~/work/org/coronavirus.org" "Information" "Useful Resources" "Collections/Streams" "David Spiegelhalter")
         "+ %t %?\n" :prepend t)
	("vis" "Science") ;; Science
	("vism" "Mechanism" item (file+olp "~/work/org/coronavirus.org" "Information" "Science" "Mechanism")
         "+ %t %?\n" :prepend t)
	("visp" "Physical Distancing / Face Masks" item (file+olp "~/work/org/coronavirus.org" "Information" "Science" "Physical Distancing / Face Masks")
         "+ %t %?\n" :prepend t)
	("vist" "Track and Trace" item (file+olp "~/work/org/coronavirus.org" "Information" "Science" "Track and Trace")
         "+ %t %?\n" :prepend t)
	("visi" "Immunity / Testing / Vaccine" item (file+olp "~/work/org/coronavirus.org" "Information" "Science" "Immunit / Testing / Vaccine")
         "+ %t %?\n" :prepend t)
	("visr" "Transmission" item (file+olp "~/work/org/coronavirus.org" "Information" "Science" "Transmission")
         "+ %t %?\n" :prepend t)
	("visc" "Miscellaneous" item (file+olp "~/work/org/coronavirus.org" "Information" "Science" "Miscellaneous")
         "+ %t %?\n" :prepend t)
	("visg" "Independant SAGE" item (file+olp "~/work/org/coronavirus.org" "Information" "Science" "Independant SAGE")
         "+ %t %?\n" :prepend t)
	("visv" "Evolutionary Genetics" item (file+olp "~/work/org/coronavirus.org" "Information" "Science" "Evolutionary Genetics")
	("vid" "Data" item (file+olp "~/work/org/coronavirus.org" "Information" "Data")
         "+ %t %?\n")
	("vitc" "Cycling" item (file+olp "~/work/org/coronavirus.org" "Information" "Traffic" "Cycling")
         "+ %t %?\n")
	("vitr" "Roads" item (file+olp "~/work/org/coronavirus.org" "Information" "Traffic" "Roads")
         "+ %t %?\n")
	("viw" "Work" item (file+olp "~/work/org/coronavirus.org" "Information" "Work")
         "+ %t %?\n")
	("va" "Activities for Isla") ;; Isla
	("vae" "Educational" item (file+olp "~/work/org/coronavirus.org" "Activities for Isla" "Educational")
         "+ %t %?\n" :prepend t)
	("vax" "Exercise" item (file+olp "~/work/org/coronavirus.org" "Activities for Isla" "Exercise")
         "+ %t %?\n" :prepend t)
	("vaf" "Fun" item (file+olp "~/work/org/coronavirus.org" "Activities for Isla" "Fun")
         "+ %t %?\n" :prepend t)
	("vh" "Humour") ;; Humour
	("vhg" "Graphics" item (file+olp "~/work/org/coronavirus.org" "Humour" "Graphics")
         "+ %t %?\n" :prepend t)
	("vhx" "XKCD" item (file+olp "~/work/org/coronavirus.org" "Humour" "XKCD")
         "+ %t %?\n" :prepend t))))
