;;; Settings for flycheck, flymake and flyspell
;;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;;; Define custom pylintrc file for checking Python code
;;(defcustom flycheck-pylintrc)

;;; Flyspell for automatic spell checking
;;; https://www.emacswiki.org/emacs/FlySpell#h5o-3
(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
	(if (derived-mode-p 'prog-mode)
	    (progn
	      (message "Flyspell on (code)")
	      (flyspell-prog-mode))
	  ;; else
	  (progn
	    (message "Flyspell on (text)")
	    (flyspell-mode 1)))
	;; I tried putting (flyspell-buffer) here but it didn't seem to work
	)))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
	(message "Flyspell off")
	(flyspell-mode -1))
    ;; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

(add-hook 'find-file-hook 'flyspell-on-for-buffer-type)

;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))

;; (dolist (mode '(emacs-lisp-mode-hook
;;                 inferior-lisp-mode-hook
;;                 clojure-mode-hook
;;                 python-mode-hook
;;                 js-mode-hook
;;                 R-mode-hook))
;;   (add-hook mode
;;             #'(lambda ()
;; 		(flyspell-prog-mode))))

;; Flyover
;; https://github.com/konrad1977/flyover
(use-package flyover
  :ensure t
  :hook
  (flycheck-mode-hook . flyover-mode)
  :config
  (setq flyover-levels '(error warning info))
  (setq flyover-use-theme-colors t)
  (setq flyover-background-lightness 45) ;;  lower values = darker
  (setq flyover-percent-darker 40) ;; Make icon background darker than foreground
  (setq flyover-text-tint 'lighter) ;; or 'darker or nil
  (setq flyover-text-tint-percent 50) ;; Percentage to lighten or darken the text when tinting is enabled.
  (setq flyover-checkers '(flycheck flymake)) ;; Which checks to use
  (setq flyover-debug nil) ;; Enable debug messages
  (setq flyover-debounce-interval 0.2) ;; Time in seconds to wait before checking and displaying errors after a change
  (setq flyover-line-position-offset 1) ;; Lines below error to display overlay (0 = same)
  (setq flyover-wrap-messages t)  ;; Enable wrapping of long error messages across multiple lines
  (setq flyover-max-line-length 80) ;; Maximum length of each line when wrapping messages
  (setq flyover-hide-checker-name t) ;; Hide checker name for a cleaner UI
  (setq flyover-hide-when-cursor-is-on-same-line t)
  )
;;; fly.el ends here
