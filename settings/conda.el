;; CONDA CONFIGURATION
;; --------------------------------------
(use-package conda
  :ensure t
  :init
  (setq conda-env-autoactivate-mode t)
  (setq conda-anaconda-home (expand-file-name "~/miniforge3"))
  (setq conda-env-home-directory (expand-file-name "~/miniforge3")))
