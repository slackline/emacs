;;; early-init.el --- Emacs early init file -*- lexical-binding: t; -*-
;;; Commentary:
;; Runs before init.el is loaded.
;;; Code:

;; If using straight.el then disable package.el to prevent all packages being loaded on startup
(setq package-enable-at-startup t)

(provide 'early-init)
;;; early-init.el ends here
