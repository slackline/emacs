;;; CSV-MODE SETTINGS
;;; https://elpa.gnu.org/packages/csv-mode.html
(use-package csv-mode
	     :ensure t
	     :defer t
	     :mode (("\\.csv" . csv-mode))
	     :config)
