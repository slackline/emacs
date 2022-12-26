;;; MYPACKAGES CONFIGURATION
;;; --------------------------------------
(defvar myPackages
  '(alert
    ;; autopair
    auto-compile
    auto-complete
    auto-package-update
    better-defaults
    bind-key
    blacken
    bug-hunter
    centaur-tabs
    ;; challenger-deep-theme
    citar-org
    ;; company-jedi
    ;; company-lsp
    code-review
    ;; company-emoji
    company-prescient
    conda
    crontab-mode
    dap-mode
    dockerfile-mode
    easy-hugo
    eglot
    ein
    elfeed-org
    elfeed-web
    elisp-refs
    emojify
    ess
    ess-smart-equals
    ess-smart-underscore
    forge
    format-all
    flycheck
    ;; flycheck-shellcheck
    gh-notify
    gist
    gitlab
    git-modes
    groovy-mode
    helm-org
    helpful
    hide-mode-line
    highlight-indent-guides
    highlight-parentheses
    inf-mongo
    ivy-mpdel
    ;; jedi
    jenkinsfile-mode
    json-mode
    julia-vterm
    keychain-environment
    latex-extra
    latex-preview-pane
    literate-calc-mode
    lsp-jedi
    lsp-julia
    lsp-latex
    lsp-ltex
    lsp-pyright
    lsp-treemacs
    lsp-ui
    magit
    magit-todos
    mastodon
    mmm-mode
    modus-themes
    numpydoc
    ob-mermaid
    org-analyzer
    org-cliplink
    org-gtd
    org-kanban
    org-notifications
    org-parser
    org-present
    org-roam
    org-roam-bibtex
    org-roam-ui
    org-ref
    org-sidebar
    osm
    ox-hugo
    ox-pandoc
    ox-reveal
    ox-rst
    ox-spectacle
    ox-tufte
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
    quarto-mode
    rainbow-delimiters
    smartparens
    ;; transient-dwim
    treemacs
    use-package
    vterm
    weblorg
    which-key
    why-this
    wide-column
    with-shell-interpreter
    yaml-mode
    yapfify
    yasnippet
    yasnippet-snippets
    ytdl))

;; Make sure all of the above packages are installed
;; (mapc #'(lambda (package)
;; 	  (unless (package-installed-p package)
;; 	    (package-install package)))
;;       myPackages)
