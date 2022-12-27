;;; TREE SITTER CONFIGURATION
;;; =========================
;;;
;;; Homepage    : https://emacs-tree-sitter.github.io/installation/
;;; GitHub      : https://github.com/emacs-tree-sitter/elisp-tree-sitter
;;; Tree-Sitter : https://tree-sitter.github.io/tree-sitter/

(use-package tree-sitter
	     :ensure t
	     :init (global-tree-sitter-mode)
	     :hook (python-mode-hook . tree-sitter-hl-mode)
             (ess-r-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)
