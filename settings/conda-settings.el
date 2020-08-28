;; CONDA CONFIGURATION
;; --------------------------------------
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/.miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/.miniconda3")))
