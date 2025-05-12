;;; Dirvish and Dired settings
;;;
;;; https://pragmaticemacs.wordpress.com/category/dired/ - multiple useful articles
;;;
;;; https://github.com/Fuco1/dired-hacks - multiple useful packages
;;;
;;; Dirvish Settings
;;; https://github.com/alexluigit/dirvish
(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes
	'(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
  ;; Placement
  ;; (setq dirvish-use-header-line nil)     ; hide header line (show the classic dired header)
  ;; (setq dirvish-use-mode-line nil)       ; hide mode line
  (setq dirvish-use-header-line 'global)    ; make header line span all panes

  ;; Height
  ;;; '(25 . 35) means
  ;;;   - height in single window sessions is 25
  ;;;   - height in full-frame sessions is 35
  (setq dirvish-header-line-height '(25 . 35))
  (setq dirvish-mode-line-height 25) ; shorthand for '(25 . 25)

  ;; Segments
  ;;; 1. the order of segments *matters* here
  ;;; 2. it's ok to place raw string inside
  (setq dirvish-header-line-format
	'(:left (path) :right (free-space))
	dirvish-mode-line-format
	'(:left (sort file-time " " file-size symlink) :right (omit yank index))))

(use-package all-the-icons
  :ensure t
  :after dirvish)

(use-package all-the-icons-dired
  :ensure t)

(use-package all-the-icons-ibuffer
  :ensure t)

(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup))

;; https://github.com/stsquad/dired-rsync
(use-package dired-rsync
  :ensure t)

;; https://github.com/stsquad/dired-rsync
(use-package dired-rsync-transient
  :ensure t)


;;
(use-package dired-ranger
  :ensure t
  :defer 0.5
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))


(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))
