;;; Delete the current cell (WARNING! : No Undo)
;; (define-key ein:notebook-mode-map "\C-c\C-d"
;;               'ein:worksheet-delete-cell)
;;; Turn off white-space deletion (causes warning messages https://goo.gl/X75omY)
;;; Actual solution (https://emacs.stackexchange.com/a/40773/10100) added to init.el
;;(add-hook 'ein:notebook-multilang-mode-hook
;;	  (lambda () (emacs/toggle-whitespace-cleanup-off)))
