;;; TREE SITTER CONFIGURATION
;;; =========================
;;;
;;; Homepage    : https://emacs-tree-sitter.github.io/installation/
;;; GitHub      : https://github.com/emacs-tree-sitter/elisp-tree-sitter
;;; Tree-Sitter : https://tree-sitter.github.io/tree-sitter/

(use-package tree-sitter
	     :ensure t
	     :init
             (global-tree-sitter-mode))

(use-package tree-sitter-langs
	     :hook
	     (tree-sitter-after-on . tree-sitter-hl-mode))
