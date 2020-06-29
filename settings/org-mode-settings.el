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

;; Hook - Reftex
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

;; org-babel
;;
;; Set up evaluation languages
(org-babel-do-load-languages
 'org-babel-load-languages '((R . t)
			     (python . t)))
;; Header skeleton
(define-skeleton org-skeleton
  "Header info for a emacs-org file."
  "Title: "
  "#+TITLE:" str " \n"
  "#+AUTHOR: Neil Shephard\n"
  "#+email: nshephard@gmail.com\n"
  "#+INFOJS_OPT: \n"
  "#+BABEL: :session *org-R* :cache yes :results output graphics :exports both :tangle yes \n"
  "-----"
 )
(global-set-key [C-S-f4] 'org-skeleton)

;; Default header arguments
(add-to-list 'org-babel-default-header-args
             '((:AUTHOR . "Neil Shephard")
	       (:email . "nshephard@gmail.com"))
	      )
(add-to-list 'org-babel-default-header-args:R
             '((:session . "*org-R*")
	       (:width . 1024) (:height . 768)
	       (:cache . "yes")
	       (:results . "output graphics")
	       (:exports . "both")
	       (:tangle . "yes")
	      ))
(add-to-list 'org-babel-default-inline-header-args
             '(:colnames . "nil"))

;; org-roam
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/work/org")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
