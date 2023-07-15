;; CONDA CONFIGURATION
;; --------------------------------------
(use-package conda
	     :defer 1
	     :ensure t
	     :init
	     (setq conda-anaconda-home (expand-file-name "~/.miniconda3"))
	     (setq conda-env-home-directory (expand-file-name "~/.miniconda3")))
