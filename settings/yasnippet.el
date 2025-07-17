;;; YASNIPPET CONFIGURATION
;;; --------------------------------------
;;;
;;; https://github.com/joaotavora/yasnippet
;;;
;;; https://emacs.stackexchange.com/questions/67991/how-to-enable-yasnippets-for-all-modes
;;;
;;; Official snippets are from https://github.com/AndreaCrotti/yasnippet-snippets but it doesn't look like you need to
;;; explicitly load the package, they just need to be installed and they are loaded automatically.
;;;
;;; Installed packages (configured under mode settings rather than here)
;;;
;;;
;;; | Language     | Package        | Config           | Homepage                                   |
;;; |--------------|----------------|------------------|--------------------------------------------|
;;; | R            | r-autoyas      | ess-settings.el  | https://github.com/mattfidler/r-autoyas.el |
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'ess-r-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)
