;;; ELFEED CONFIGURATION
;;; --------------------------------------
;;;
;;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
;;;
;;; https://github.com/SqrtMinusOne/elfeed-summary/


(global-set-key (kbd "C-x w") 'elfeed)

;; use an org file to organise feeds
(use-package elfeed-org
	     :ensure t
	     :config
	     (elfeed-org)
	     (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elfeed feed reader                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shortcut functions
(defun nds:elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))
(defun nds:elfeed-show-humor ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-humor"))
(defun nds:elfeed-show-climbing ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-climbing"))
(defun nds:elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))
(defun nds:elfeed-show-reading ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-reading"))
(defun nds:elfeed-show-statistics ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-statistics"))
(defun nds:elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))
;; functions to support syncing .elfeed between machines
(defun nds:elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))

(defun nds:elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun nds:elfeed-updater ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-save)
  (quit-window)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))

(use-package simple-httpd
	     :ensure t
	     :config
	     (setq httpd-host (cdr
                               (assoc (nth 0 (split-string (system-name) "\\.")) httpd-system-name)))
	     (setq httpd-port "8818")
             :hook (after-init-hook . elfeed-web-start))


(use-package elfeed
	     :ensure t
	     :bind (:map elfeed-search-mode-map
			 ("A" . nds:elfeed-show-all)
			 ("c" . nds:elfeed-show-climbing)
			 ("C" . nds:elfeed-show-computing)
			 ("e" . nds:elfeed-show-emacs)
			 ("R" . nds:elfeed-show-reading)
			 ("H" . nds:elfeed-show-humor)
			 ("S" . nds:elfeed-show-statistics)
			 ("D" . nds:elfeed-show-daily)
			 ("q" . nds:elfeed-save-db-and-bury))
	     :config
	     (setq httpd-port 8818)
	     ;; run-with-timer takes an argument in seconds
	     ;; (* 30 60) therefore returns 30 minutes in seconds
	     (run-at-time "07:00" (* 120 60) 'nds:elfeed-updater)
	     (setq elfeed-web-enabled t))


;; Use elfeed-web to periodically update
;; (use-package elfeed-web
;;   :ensure t
;;   :config
;;   (setq httpd-port 8818))

;; Deleting entries from the database https://github.com/skeeto/elfeed/issues/392
(defun nds:elfeed-db-remove-entry (id)
  "Removes the entry for ID"
  (avl-tree-delete elfeed-db-index id)
  (remhash id elfeed-db-entries))

(defun nds:elfeed-search-remove-selected ()
  "Remove selected entries from database"
  (interactive)
  (let* ((entries (elfeed-search-selected))
	 (count (length entries)))
    (when (y-or-n-p (format "Delete %d entires?" count))
      (cl-loop for entry in entries
	       do (nds:elfeed-db-remove-entry (elfeed-entry-id entry)))))
  (elfeed-search-update--force))
