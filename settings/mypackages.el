;; MYPACKAGES CONFIGURATION
;; --------------------------------------
(defvar myPackages
  '(alert
    ;; apiwrap
    autopair
    auto-package-update
    ;;colour-parentheses
    darktooth-theme
    dracula-theme
    better-defaults
    blacken
    ein
    elpy
    ess
    ess-smart-underscore
    flycheck
    ;;flycheck-json
    ;;flycheck-pycheck
    ;;flycheck-yaml
    gitlab
    highlight-parentheses
    ivy-mpdel
    jedi
    kaolin-themes
    ;; latex-extra
    latex-preview-pane
    magit
    ;;magithub
    material-theme
    mmm-mode
    nova-theme
    ;;org-mode
    org-cliplink
    org-parser
    pass
    polymode
    poly-markdown
    poly-noweb
    poly-org
    poly-R
    projectile
    projectile-git-autofetch
    py-autopep8
    py-yapf
    pyvenv
    pylint
    pytest
    python-mode
    python-pytest
    wide-column
    yaml-mode))

;; Make sure all of the above packages are installed
(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)
