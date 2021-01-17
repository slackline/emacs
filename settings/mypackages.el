;;; MYPACKAGES CONFIGURATION
;;; --------------------------------------
(defvar myPackages
  '(alert
    autopair
    auto-compile
    auto-package-update
    better-defaults
    blacken
    centaur-tabs
    company-jedi
    company-prescient
    conda
    dap-mode
    ein
    elfeed-org
    elpy
    ess
    ess-smart-underscore
    forge
    flycheck
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
    lsp-pyright
    lsp-ui
    magit
    magit-todos
    mmm-mode
    modus-vivendi-theme
    org-cliplink
    org-parser
    org-present
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
    pygen
    pyvenv
    pylint
    pytest
    python-mode
    python-pytest
    rainbow-delimiters
    treemacs
    use-package
    vterm
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
