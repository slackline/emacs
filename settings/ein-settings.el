;;; Delete the current cell (WARNING! : No Undo)
;; (define-key ein:notebook-mode-map "\C-c\C-d"
;;               'ein:worksheet-delete-cell)
;;; Turn off white-space deletion (causes warning messages https://goo.gl/X75omY)
;;(add-hook 'ein:notebook-multilang-mode-hook
;;	  #'(lambda () (emacs/toggle-whitespace-cleanup-off)))
