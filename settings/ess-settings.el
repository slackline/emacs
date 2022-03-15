;; ESS CONFIGURATION
;; --------------------------------------
;;; Smart underscore  https://www.emacswiki.org/emacs/ess-smart-underscore.el
;; (use-package ess-smart-underscore)

(use-package ess
  :ensure t
  :defer 1
  :init
  (setq comint-input-ring-size 1000
        ess-indent-level 4
        ess-arg-function-offset 4
        ess-else-offset 4
        ess-eval-visibly-p nil
        ess-ask-for-ess-directory nil
        ess-togggle-underscore nil
        ess-eval-visibly 'nowait
        ess-r--no-company-meta t ;; https://github.com/emacs-ess/ESS/issues/1062
        ess-use-auto-complete t)
  :mode (("/R/.*\\.q\\'"       . R-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rd\\'"           . Rd-mode))
  :interpreter (("R" . R-mode)
                ("R" . R-transcript-mode)
                ("R" . Rd-mode))
  :config
;;  (setq ess-r-backend 'lsp)
  )

;; Various links on how to set things up
;;
;; General
;; https://github.com/emacs-ess/ESS/issues/809
;; https://github.com/emacs-lsp/lsp-mode/issues/1383#issue-560006302
;;
;; Pipes
;; https://emacs.stackexchange.com/a/65148
;; https://github.com/emacs-ess/ESS/issues/809#issuecomment-453538386
(use-package ess-r-mode
  :ensure ess
  :defer 1
  :bind (:map ess-r-mode-map ("C-|" . " |>\n"))
  (:map ess-r-mode-map (";" . ess-insert-assign))
        )

;; Now deprecated (see https://emacs.stackexchange.com/questions/48134/ess-smart-underscore-does-not-work-in-emacs25)
;; See also https://github.com/emacs-ess/ESS/issues/809
;; Restore functionality with...
;(define-key ess-mode-map "_" 'ess-insert-assign)
;(define-key inferior-ess-r-mode-map "_" 'ess-insert-assign)

;;; Pipe operator https://emacs.stackexchange.com/a/8055
;; (defun then_R_operator ()
;;   "R - |> operator or 'then' pipe operator"
;;   (interactive)
;;   (just-one-space 1)
;;   (insert "|> ")
;;   (reindent-then-newline-and-indent))
;; (define-key ess-mode-map (kbd "C-c |") 'then_R_operator)
;; (define-key inferior-ess-mode-map (kbd "C-c |") 'then_R_operator)


;;; Set the width of the buffer automatically from
;;; https://stat.ethz.ch/pipermail/ess-help/2009-July/005455.html
(defun my-ess-post-run-hook ()
  (ess-execute-screen-options)
  (local-set-key "\C-cw" 'ess-execute-screen-options))
(add-hook 'ess-post-run-hook 'my-ess-post-run-hook)
;;; Auto-complete and ESS http://goo.gl/utAi2Z
(use-package auto-complete)
;; (use-package auto-complete-config)
;; (ac-config-default)
(auto-complete-mode)
