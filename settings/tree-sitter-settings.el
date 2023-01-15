;;; TREE SITTER CONFIGURATION
;;; =========================
;;;
;;; Homepage    : https://emacs-tree-sitter.github.io/installation/
;;; GitHub      : https://github.com/emacs-tree-sitter/elisp-tree-sitter
;;; Tree-Sitter : https://tree-sitter.github.io/tree-sitter/
;;;
;;; https://wwwtech.de/articles/2022/dec/emacs-29-install-tree-sitter-parser-modules-with-a-minor-mode
(use-package tree-sitter
	     :ensure t
	     :init
             (global-tree-sitter-mode))

(use-package tree-sitter-langs
	     :hook
	     (tree-sitter-after-on . tree-sitter-hl-mode))
