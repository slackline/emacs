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
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :hook
  (after-init . org-roam-mode)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph-show)
	 ("C-c n i" . org-roam-node-insert))
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
