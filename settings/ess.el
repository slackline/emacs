;; ESS CONFIGURATION
;; --------------------------------------
;;
;; Useful pages to read on configuration of ESS and sometimes LSP mode
;;
;; ESS
;; https://weikaichen.gitee.io/notes/emacs-ess-r/
;;
;; + lsp-mode
;; https://github.com/emacs-ess/ESS/issues/809
;; https://github.com/emacs-lsp/lsp-mode/issues/1383#issue-560006302
;;
;; Pipes and assignment customisation
;;
;; https://emacs.stackexchange.com/a/8055
;; Notes on underscore for assignment
;;
;; https://emacs.stackexchange.com/questions/48134/ess-smart-underscore-does-not-work-in-emacs25)
;; https://github.com/emacs-ess/ESS/issues/809
;; https://chainsawriot.com/postmannheim/2022/12/24/aoe24.html
;; https://emacs.stackexchange.com/a/65148
;; https://github.com/emacs-ess/ESS/issues/809#issuecomment-453538386
;;
;; Plots in Emacs buffers
;; https://emacs.stackexchange.com/questions/2292/ess-plot-directly-to-an-emacs-buffer
;;
;; Better still plots in the browser
;;
;; https://www.youtube.com/watch?v=uxyhmhRVOfw
(use-package ess
	     :ensure t
	     ;; :defer 1
             ;;     :requires ess-r-mode
             ;;     ess-r-package
	     :init
	     :mode (("/R/.*\\.q\\'"       . R-mode)
		    ("\\.[rR]\\'"         . R-mode)
		    ("\\.[rR]profile\\'"  . R-mode)
		    ("NAMESPACE\\'"       . R-mode)
		    ("CITATION\\'"        . R-mode)
		    ("\\.[Rr]out"         . R-transcript-mode)
		    ("\\.Rmd\\'"          . Rd-mode)
		    ("\\.Rd\\'"           . Rd-mode))
	     :interpreter (("R" . R-mode)
			   ("R" . R-transcript-mode)
			   ("R" . Rd-mode))
	     :config
             (require 'ess-r-mode)
             (require 'ess-r-package)
	     (setq ess-r-backend 'lsp)
	     (setq comint-input-ring-size 1000)
	     (setq ess-indent-offset 2)
	     (setq ess-eval-visibly-p nil)
             (setq ess-startup-directory nil)
	     (setq ess-ask-for-ess-directory nil)
	     (setq ess-togggle-underscore nil)
	     (setq ess-eval-visibly 'nowait)
             (setq ess-use-tracebug nil)
	     :hook
	     (ess-mode . company-mode)
	     (inferior-ess-mode . company-mode)
             :bind
             (:map ess-r-mode-map
		   ("_" . 'ess-insert-assign)  ;;
		   ("C-q" . 'ess-eval-region-or-line-and-step)
		   ("C-|" . " |>\n"))
             (:map inferior-ess-r-mode-map
		   ("_" . 'ess-insert-assign)
		   ("C-|" . " |>\n")))

;; Quarto mode https://github.com/quarto-dev/quarto-emacs
;; By default associated with .qmd files
(use-package quarto-mode
	     :mode (("\\.Rmd" . poly-quarto-mode))
	     :bind (("C-c q" . quarto-preview)))


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

;;; https://github.com/mattfidler/r-autoyas.el
;; Disabled 2023-01-28 was causing errors (see https://github.com/emacs-ess/ESS/issues/967#issuecomment-541276597)
;; (use-package r-autoyas
;; 	     :ensure t
;; 	     :defer 3
;; 	     :hook
;; 	     (ess-mode . r-autoyas-ess-activate))
