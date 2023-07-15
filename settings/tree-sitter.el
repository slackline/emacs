;;; TREE SITTER CONFIGURATION
;;; =========================
;;;
;;; Homepage    : https://emacs-tree-sitter.github.io/installation/
;;; GitHub      : https://github.com/emacs-tree-sitter/elisp-tree-sitter
;;; Tree-Sitter : https://tree-sitter.github.io/tree-sitter/
;;; ts-fold     : https://github.com/emacs-tree-sitter/ts-fold
;;;
;;; https://wwwtech.de/articles/2022/dec/emacs-29-install-tree-sitter-parser-modules-with-a-minor-mode
;;; https://archive.casouri.cc/note/2023/tree-sitter-starter-guide/index.html
;;; https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html
;;; https://www.reddit.com/r/emacs/comments/r0i031/comment/hlxwhyu/
;;; https://takeonrules.com/2023/03/08/coloring-regular-expression-via-modus-themes-for-treesit-faces/
;;; https://www.youtube.com/watch?v=MZPR_SC9LzE
;;; https://mastodon.social/@spaceotter@mastodon.online/109671596235038019
;;; https://mastodon.social/@spaceotter@mastodon.online/109774686483526367
;;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
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
