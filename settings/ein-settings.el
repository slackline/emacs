;; EIN CONFIGURATION
;; --------------------------------------
;;; Delete the current cell (WARNING! : No Undo)
;; (define-key ein:notebook-mode-map "\C-c\C-d"
;;               'ein:worksheet-delete-cell)
;;; Turn off white-space deletion (causes warning messages https://goo.gl/X75omY)
;;; Actual solution (https://emacs.stackexchange.com/a/40773/10100) added to init.el
;;(add-hook 'ein:notebook-multilang-mode-hook
;;	  (lambda () (emacs/toggle-whitespace-cleanup-off)))
(custom-set-variables
 '(ein:output-area-inlined-images t)
 '(ein:polymode t)
 '(ein:query-timeout 100000))

;; EIN commands
(global-set-key [f12] 'ein:worksheet-delete-cell)
(local-set-key [f5] 'ein:notebook-reconnect-kernel)
