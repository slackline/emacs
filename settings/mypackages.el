;;; MYPACKAGES CONFIGURATION
;;; --------------------------------------
(defvar myPackages
  '(alert
    autopair
    auto-compile
    auto-complete
    auto-package-update
    better-defaults
    blacken
    bug-hunter
    centaur-tabs
    challenger-deep-theme
    ;; company-jedi
    ;; company-lsp
    company-prescient
    conda
    dap-mode
    ein
    elfeed-org
    elfeed-web
    ess
    ess-smart-equals
    ess-smart-underscore
    forge
    flycheck
    ;; flycheck-shellcheck
    gitlab
    helm-org
    hide-mode-line
    highlight-indent-guides
    highlight-parentheses
    inf-mongo
    ivy-mpdel
    ;; jedi
    json-mode
    keychain-environment
    latex-extra
    latex-preview-pane
    literate-calc-mode
    lsp-jedi
    lsp-latex
    lsp-pyright
    lsp-treemacs
    lsp-ui
    magit
    magit-todos
    mmm-mode
    modus-vivendi-theme
    numpydoc
    org-cliplink
    org-parser
    org-present
    org-roam
    org-roam-bibtex
    org-ref
    org-sidebar
    package-utils
    pass
    polymode
    poly-markdown
    poly-noweb
    poly-org
    poly-R
    powerline
    prescient
    projectile
    projectile-git-autofetch
    py-autopep8
    py-yapf
    pyvenv
    pylint
    pytest
    python-mode
    python-pytest
    rainbow-delimiters
    treemacs
    use-package
    vterm
    weblorg
    wide-column
    with-shell-interpreter
    yaml-mode
    yapfify
    ytdl))

;; Make sure all of the above packages are installed
(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; (defvar my/packages
;;   '(ace-window
;;     alert
;;     anaphora
;;     async
;;     atom-one-dark-theme
;;     auctex
;;     auth-source-pass
;;     auto-compile
;;     auto-complete
;;     auto-package-update
;;     autopair
;;     avy
;;     better-defaults
;;     biblio
;;     biblio-core
;;     bibtex-completion
;;     bind-key
;;     blacken
;;     bug-hunter
;;     bui
;;     centaur-tabs
;;     cfrs
;;     challenger-deep-theme
;;     closql
;;     company
;;     company-lsp
;;     company-prescient
;;     conda
;;     dap-mode
;;     dash
;;     dash-functional
;;     deferred
;;     ebib
;;     ein
;;     eink-theme
;;     elfeed
;;     elfeed-org
;;     elfeed-web
;;     emacsql
;;     emacsql-sqlite
;;     emacsql-sqlite3
;;     epl
;;     ess
;;     ess
;;     ess-smart-equals
;;     ess-smart-underscore
;;     f
;;     find-file-in-project
;;     flycheck
;;     flycheck-mypy
;;     flymake-shellcheck
;;     forge
;;     forge
;;     ghub
;;     git-commit
;;     gitlab
;;     gntp
;;     helm
;;     helm
;;     helm-bibtex
;;     helm-core
;;     helm-core
;;     helm-org
;;     hide-mode-line
;;     highlight-indent-guides
;;     highlight-indentation
;;     highlight-parentheses
;;     hl-todo
;;     ht
;;     htmlize
;;     hydra
;;     inf-mongo
;;     ivy
;;     ivy-mpdel
;;     js2-mode
;;     json-mode
;;     json-reformat
;;     json-snatcher
;;     julia-mode
;;     key-chord
;;     keychain-environment
;;     latex-extra
;;     latex-preview-pane
;;     libmpdel
;;     libmpdel
;;     literate-calc-mode
;;     log4e
;;     lsp-jedi
;;     lsp-jedi
;;     lsp-latex
;;     lsp-latex
;;     lsp-mode
;;     lsp-mode
;;     lsp-mode
;;     lsp-mode
;;     lsp-pyright
;;     lsp-treemacs
;;     lsp-ui
;;     lv
;;     magit
;;     magit
;;     magit-section
;;     magit-todos
;;     map
;;     markdown-mode
;;     material-theme
;;     mmm-mode
;;     modus-vivendi-theme
;;     mpdel
;;     navigel
;;     org-analyzer
;;     org-cliplink
;;     org-parser
;;     org-plus-contrib
;;     org-present
;;     org-ql
;;     org-ref
;;     org-roam
;;     org-sidebar
;;     org-super-agenda
;;     org-time-budgets
;;     ov
;;     package+
;;     package-utils
;;     packed
;;     parsebib
;;     pass
;;     password-store
;;     password-store-otp
;;     pcre2el
;;     pdf-tools
;;     peg
;;     pfuture
;;     pkg-info
;;     poly-R
;;     poly-markdown
;;     poly-markdown
;;     poly-noweb
;;     poly-org
;;     polymode
;;     popup
;;     popup
;;     posframe
;;     powerline
;;     prescient
;;     projectile
;;     projectile-git-autofetch
;;     py-autopep8
;;     py-yapf
;;     pylint
;;     pytest
;;     python-mode
;;     python-pytest
;;     pythonic
;;     pyvenv
;;     rainbow-delimiters
;;     request
;;     restart-emacs
;;     s
;;     scihub
;;     seq
;;     simple-httpd
;;     spinner
;;     tablist
;;     templatel
;;     transient
;;     treemacs
;;     treepy
;;     ts
;;     use-package
;;     vimish-fold
;;     vterm
;;     weblorg
;;     websocket
;;     wide-column
;;     with-editor
;;     with-shell-interpreter
;;     yaml-mode
;;     yapfify
;;     yasnippet
;;     ytdl
;;     ))
