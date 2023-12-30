;;; COMPANY
;;; https://company-mode.github.io/manual/
(use-package company
  :ensure t
  :defer 0.5
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.3)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-other-buffers t)
  (setq company-dabbrev-code-other-buffers t)
  :hook ((text-mode . company-mode)
	 (prog-mode . company-mode)))
