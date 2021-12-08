;;; ORG-BABEL CONFIGURATION
;;; --------------------------------------
;; org-babel
;;
;; Set up evaluation languages
(org-babel-do-load-languages
 'org-babel-load-languages '((R . t)
			     (python . t)))
;; Hooks for in-line images (https://emacs.stackexchange.com/a/21267/10100)
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;;; Hook to rsync html output to OVH on export
(add-hook 'org-html-export-to-html 'rsync-html)


;; Skeletons
(define-skeleton org-R-skeleton
  "Header info for a org file with R."
  "Title: "
  "#+TITLE:" str " \n"
  "#+AUTHOR: Neil Shephard\n"
  "#+EMAIL: nshephard@protonmail.com\n"
  "#+PROPERTY: header-args:R  :session *org-R*\n"
  "#+PROPERTY: header-args:R  :cache yes\n"
  "#+PROPERTY: header-args:R  :results graphics\n"
  "#+PROPERTY: header-args:R  :width 1024\n"
  "#+PROPERTY: header-args:R  :height 768\n"
  "#+PROPERTY: header-args:R  :tangle yes\n"
  "#+INFOJS_OPT: \n"
  "#+BABEL:  :session *org-R*  :cache yes  :exports both  :results output graphics  :tangle yes  :width 1024  :height 768 \n"
  "-----"
 )
(global-set-key [C-S-f4] 'org-R-skeleton)

(define-skeleton org-python-skeleton
  "Header info for a org file with Python."
  "Title: "
  "#+TITLE:" str " \n"
  "#+AUTHOR: Neil Shephard\n"
  "#+EMAIL: nshephard@gmail.com\n"
  "#+PROPERTY: header-args:python  :session *org-python*\n"
  "#+PROPERTY: header-args:python  :cache yes\n"
  "#+PROPERTY: header-args:python  :results graphics\n"
  "#+PROPERTY: header-args:python  :width 1024\n"
  "#+PROPERTY: header-args:python  :height 768\n"
  "#+PROPERTY: header-args:python  :tangle yes\n"
  "#+INFOJS_OPT: \n"
  "#+BABEL:  :session *org-R*  :cache yes  :exports both  :results output graphics  :tangle yes  :width 1024  :height 768 \n"
  "-----"
 )
(global-set-key [C-S-f5] 'org-python-skeleton)

;; Default header arguments
(add-to-list 'org-babel-default-header-args
             '(:AUTHOR . "Neil Shephard")
	     '(:EMAIL . "nshephard@gmail.com")
	     )
(add-to-list 'org-babel-default-header-args:R
             '(:session . "*org-R*")
	     )
;; 	       (:width . 1024) (:height . 768)
;; 	       (:cache . "yes")
;; 	       (:results . "output graphics")
;; 	       (:exports . "both")
;; 	       (:tangle . "yes")
;; 	      ))
(add-to-list 'org-babel-default-inline-header-args
             '(:colnames . "nil"))
;; Insert code blocks (https://emacs.stackexchange.com/a/12847)
(add-to-list 'org-structure-template-alist
             '("r" "#+NAME: ?\n#+BEGIN_SRC R :session ** :eval yes :exports none :results output silent\n\n#+END_SRC"))
(add-to-list 'org-structure-template-alist
             '("p" "#+NAME: ?\n#+BEGIN_SRC Python :session ** :eval yes :exports none :results output silent\n\n#+END_SRC"))

;; Embed CSS (https://stackoverflow.com/a/37132338)
(defun org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/org/solarized-dark.css" path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))
(add-hook 'org-export-before-processing-hook 'org-inline-css-hook)

;; Functions as default heading arguments
;; https://blog.tecosaur.com/tmio/2021-11-30-element.html#functions-as-default
;;
;; Generate sha1 based filename
(defun my/org-src-sha-to-image ()
  (concat "generated-"
          (substring
           (sha1 (org-element-property :value (org-element-at-point)))
           0 8)
          ".png"))
;;          ".svg"))

;; Check whether a source block produces a plot (i.e. if there is a plot or ggplot command)
(defun my/org-src-guess-results-type ()
  (if (string-match-p "^ *\\(?:plot\\|ggplot\\)([^\n]+\n?\\'"
                      (org-element-property :value (org-element-at-point)))
      "graphics file" "replace"))

;; Finally use these functions to set default headers for :results and :file
(setq org-babel-default-header-args:R
      '((:results . my/org-src-guess-results-type)
        (:file . my/org-src-sha-to-image)))
