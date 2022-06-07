;;; Org-Roam
;;;
;;; https://www.orgroam.com/manual.html
;;;
;;; For more notes see C-c n f org
;; (use-package sqlite3
;;   :straight t)
;; (use-package emacsql-libsqlite3
;;   :straight t
;;   :custom
;;   (org-roam-database-connector 'libsqlite3))
(use-package org-roam
;;  :ensure t
  :defer 10
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/work/org-roam")
  (org-roam-dailies-directory "daily/")
  (org-roamd-db-location "~/work/org-roam/org-roam.db")
  (org-roam-db-autosync-mode)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "main/${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n#+FILETAGS: ${tags}")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+TITLE: %{title}\n#+DATE: %U\n"))))
  ;; To add : https://www.reddit.com/r/OrgRoam/comments/tfcwki/org_roam_capture_create_nametitle_of_a_note/
  :hook
  (after-init . org-roam-mode)
  :bind (("C-c n a" . org-roam-alias-add)
	     ("C-c n c" . org-roam-capture)
	     ("C-c n d" . org-roam-dailies-capture-today)
	     ("C-c n f" . org-roam-node-find)
	     ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-graph-show)
	     ("C-c n l" . org-roam-buffer-toggle)
	     ("C-c n o" . org-id-get-create)
	     ("C-c n s" . org-roam-db-sync)
	     ("C-c n t" . org-tag-add)
         ("C-c n u s" . org-roam-ui-open)
         ))
  ;; :config
  ;; (org-roam-setup))

;; org-roam-ui https://github.com/org-roam/org-roam-ui
(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam-bibtex-mode
  :init
  (setq org-roam-ui-sync-theme t
	    org-roam-ui-follow t
	    org-roam-ui-update-on-save t
	    org-roam-ui-open-on-start t))
