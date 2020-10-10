;; POLYMODE CONFIGURATION
;; --------------------------------------
;; Load Polymode https://github.com/vspinu/polymode
(defun rmd-mode ()
  "ESS Markdown mode for rmd files"
  (interactive)
  (setq load-path
    (append '("~/.emacs.d/lisp/polymode/"  "~/.emacs.d/lisp/polymode/modes")
        load-path))
  (use-package poly-R)
  (use-package poly-noweb)
  (use-package poly-markdown)
  (poly-markdown+r-mode))

;;; Register file types
;;; MARKDOWN
;;;(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;;; Hopefully work around the freezes from https://goo.gl/l6mtG5
(defun markdown-match-propertized-text (property last)
  "Match text with PROPERTY from point to LAST.
Restore match data previously stored in PROPERTY."
  (let ((saved (get-text-property (point) property))
        pos)
    (unless saved
      (setq pos (next-single-char-property-change (point) property nil last))
      (setq saved (get-text-property pos property)))
    (when saved
      (set-match-data saved)
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (match-end 0) (point)))
                      (point-max)))
      saved)))
;;; This hasn't worked, I still get lock-ups, but perhaps a little less frequently.
;;; Found another thread at https://stat.ethz.ch/pipermail/ess-help/2016-January/010886.html
;;;
;;; Other pages on installing Polymode are...
;;;
;;; http://johnstantongeddes.org/open%20science/2014/03/26/Rmd-polymode.html
;;; http://simon.bonners.ca/bonner-lab/wpblog/?p=142
;;;
;;; Python Polymode (Markdown + Python)
;;;
;;; https://stackoverflow.com/questions/52489905/emacs-polymode-for-markdown-and-python
;;; https://emacs.stackexchange.com/questions/20437/polymode-with-python-and-latex-mode/
;;;
;;; Further discussion and working solution at...
;;;
;;; https://github.com/polymode/polymode/issues/180


;; define pweave polymodes
(use-package poly-noweb)
(use-package poly-markdown)

;; Python/Markdown
(defcustom pm-inner/noweb-python
  (clone pm-inner/noweb
         :name "noweb-python"
         :mode 'python-mode)
  "Noweb for Python"
  :group 'poly-innermodes
  :type 'object)

(define-polymode poly-pweave-mode poly-markdown-mode
  :innermodes '(pm-inner/noweb-python :inherit))

(add-to-list 'auto-mode-alist '("\\.pymd" . poly-pweave-mode))


;; Python/LaTeX (see https://emacs.stackexchange.com/a/20446)
(use-package polymode)
(defcustom pm-inner/python
  (clone pm-inner/noweb
	 :name "python"
	 :mode 'python-mode
	 :head-matcher  "\\\\begin{pycode}"
	 :tail-matcher  "\\\\end{pycode}")
  "Python inline code."
  :group 'innermodes
  :type 'object)

;; (defcustom pm-poly/latex-python
;;   (pm-polymode-one :name "latex-python"
;; 		   :hostmode 'pm-host/latex
;; 		   :innermode 'pm-inner/python)
;;   "latex-python typical polymode."
;;   :group 'polymodes
;;   :type 'object)

(define-polymode poly-latex+python-mode
  :hostmode 'pm-host/latex
  :innermodes '(pm-inner/python))
(add-to-list 'auto-mode-alist '("\\.texw$" . poly-latex+python-mode))
