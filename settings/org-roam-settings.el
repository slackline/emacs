;;; Org-Roam
;;;
;;; https://www.orgroam.com/manual.html
;;;
;;; https://systemcrafters.cc/build-a-second-brain-in-emacs/getting-started-with-org-roam/
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/work/org-roam")
  (org-roam-dailies-directory "daily/")
  (org-roam-db-autosync-mode)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
  :hook
  (after-init . org-roam-mode)
  :bind (("C-c n a" . org-roam-alias-add)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-graph-show)
	 ("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n o" . org-id-get-create)
	 ("C-c n t" . org-tag-add))
  :config
  (org-roam-setup))

;; org-roam-ui https://github.com/org-roam/org-roam-ui
(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam-bibtex-mode:config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))
