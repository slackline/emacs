;;; Org-Roam
(use-package org-roam
      :ensure t
      :custom
      (org-roam-directory "~/org/roam")
      :hook
      (after-init . org-roam-mode)
      :config
      (setq org-roam-v2-ack t)
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
