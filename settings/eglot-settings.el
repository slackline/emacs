;;; Eglot settings
;;;
;;; https://joaotavora.github.io/eglot/ / https://github.com/joaotavora/eglot
;;;
;;; Language Servers
;;; ----------------
;;;
;;; Bash https://github.com/bash-lsp/bash-language-server
;;; Dockerfile https://github.com/rcjsuen/dockerfile-language-server-nodejs
;;; JSON https://github.com/hrsh7th/vscode-langservers-extracted
;;; Markdown https://github.com/artempyanykh/marksman
;;; Python https://github.com/pappasam/jedi-language-server
;;; R https://cran.r-project.org/package=languageserver
;;; Rust https://github.com/rust-analyzer/rust-analyzer
;;; Tex/LaTeX https://github.com/astoff/digestif
;;; YAML https://github.com/redhat-developer/yaml-language-server
;;;
;;; Org-mode isn't yet supported but see https://github.com/joaotavora/eglot/issues/523


(use-package eglot
	     :ensure t
	     :defer t
	     :config
	     (define-key global-map (kbd "C-c E") (make-sparse-keymap))
	     (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
	     :init
	     (setq lsp-keymap-prefix "C-c l"
		   lsp-bash-highlight-parsing-errors t
		   lsp-pylsp-plugins-pylint-args ["--rcfile=/home/neil/dotfiles/python/.pylintrc"])
	     :hook
	     ((bash-mode . eglot-ensure)
	      (ess-r-mode . eglot-ensure)
	      (html-mode . eglot-ensure)
	      (latex-mode . eglot-ensure)
	      (markdown-mode . eglot-ensure)
	      (python-mode . eglot-ensure)
	      (R-mode . eglot-ensure)
	      (rust-mode . eglot-ensure)
	      (sh-mode . eglot-ensure))
             :bind
             (("C-c e l" . eglot)
              ("C-c e c" . eglot-reconnect)
	      ("C-c e s" . eglot-shutdown)
              ("C-c e d" . flymake-show-buffer-diagnostics)
              ("C-c e f f" . eglot-format)
              ("C-c e f b" . eglot-format-buffer)
              ("C-c e r r" . eglot-rename))
             )
