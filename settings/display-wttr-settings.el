;;; DISPLAY-WTTR SETTINGS
;;;
;;; https://github.com/josegpt/display-wttr
(use-package display-wttr
  :custom
  ;; (display-wttr-format "4")
  (display-wttr-locations '("Sheffield"))
  ;; (display-wttr-interval (* 60 60))
  :config
  (display-wttr-mode))
