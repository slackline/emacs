;;; COMPANY
;;; https://company-mode.github.io/manual/
(use-package company
	     :defer t
	     :config
	     (setq company-minimum-prefix-length 3)
	     (setq company-idle-delay 0.3)
	     (setq company-selection-wrap-around t))
