;; LaTeX Extra (https://github.com/Malabarba/latex-extra)
(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

;; Enable RefTex on start up (https://blog.karssen.org/2011/08/21/reftex-how-could-i-have-missed-this/)
;; also has a function and hook enabled under org-settings.el
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)

;; Preview Pane (https://github.com/jsinglet/latex-preview-pane)
;;(latex-preview-pane-enable)

