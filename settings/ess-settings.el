;;; ESS
(use-package ess
  :defer 1)
;;; Some generally useful key-bindings (mostly ESS specific) from
;;; http://stats.blogoverflow.com/page/2/
(define-key global-map [f1] 'Control-X-prefix)
(define-key global-map [f2] 'save-buffer)
(define-key global-map [f3] 'find-file)
(define-key global-map [f5] 'switch-to-buffer)
(define-key global-map [f6] 'other-window)
(define-key global-map [f8] 'kill-buffer)
(define-key global-map [f9] 'ess-load-file)
;;; Other specific ESS settings you can use are the following:
(setq comint-input-ring-size 1000)
(setq ess-indent-level 4)
(setq ess-arg-function-offset 4)
(setq ess-else-offset 4)
(setq ess-eval-visibly-p nil)
(setq ess-ask-for-ess-directory nil)
;;; Free Emacs when it takes a long time to execute commands
(setq ess-eval-visibly 'nowait)
;;; Set the width of the buffer automatically from
;;; https://stat.ethz.ch/pipermail/ess-help/2009-July/005455.html
(defun my-ess-post-run-hook ()
  (ess-execute-screen-options)
  (local-set-key "\C-cw" 'ess-execute-screen-options))
(add-hook 'ess-post-run-hook 'my-ess-post-run-hook)
;;; Auto-complete and ESS http://goo.gl/utAi2Z
(require 'auto-complete)
(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")
(ac-config-default)
(auto-complete-mode)
(setq ess-use-auto-complete t)
;;; Smart underscore  https://www.emacswiki.org/emacs/ess-smart-underscore.el
(require 'ess-smart-underscore)
