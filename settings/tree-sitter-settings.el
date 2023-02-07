;;; TREE SITTER CONFIGURATION
;;; =========================
;;;
;;; Homepage    : https://emacs-tree-sitter.github.io/installation/
;;; GitHub      : https://github.com/emacs-tree-sitter/elisp-tree-sitter
;;; Tree-Sitter : https://tree-sitter.github.io/tree-sitter/
;;;
;;; https://wwwtech.de/articles/2022/dec/emacs-29-install-tree-sitter-parser-modules-with-a-minor-mode
;;; https://archive.casouri.cc/note/2023/tree-sitter-starter-guide/index.html
;;; https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html
;;;
;;; Extensions
;;;
;;; https://github.com/erickgnavar/tree-sitter-ispell.el
(use-package tree-sitter
	     :ensure t
	     :init
             (global-tree-sitter-mode))

(use-package tree-sitter-langs
	     :hook
	     (tree-sitter-after-on . tree-sitter-hl-mode))

;; For Emacs 29.0
;; https://github.com/renzmann/treesit-auto
;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (treesit-auto-apply-remap))
