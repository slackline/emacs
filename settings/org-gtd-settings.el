;;; Org-gtd
;;; https://github.com/Trevoke/org-gtd.el/blob/master/doc/org-gtd.org
;;;
;;; Also includes customiastion for org-agenda
(use-package org-gtd
  :after org
  ;; :quelpa (org-gtd :fetcher github :repo "trevoke/org-gtd.el"
  ;;                  :branch "2.0.0" :upgrade t)
  :demand t
  :custom
  ;; (org-gtd-directory stag-org-gtd-directory)
  (org-edna-use-inheritance t)
  :config
  (org-edna-mode)
  (setq org-gtd-directory '"~/org/gtd")
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-process-map
   ("C-c d g" . org-gtd-choose)))
