;;; ELFEED CONFIGURATION
;;; --------------------------------------
;;;
;;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/

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
(defun nds:elfeed-show-computing ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-computing"))
(defun nds:elfeed-show-reading ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-reading"))
(defun nds:elfeed-show-jobs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-jobs"))
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

(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
              ("A" . nds:elfeed-show-all)
              ("c" . nds:elfeed-show-climbing)
              ("C" . nds:elfeed-show-computing)
              ("R" . nds:elfeed-show-reading)
              ("H" . nds:elfeed-show-humor)
              ("J" . nds:elfeed-show-jobs)
              ("S" . nds:elfeed-show-statistics)
              ("D" . nds:elfeed-show-daily)
              ("q" . nds:elfeed-save-db-and-bury)))


;; Use elfeed-web to periodically update
(use-package elfeed-web
  :ensure t)
;; run-with-timer takes an argument in seconds
;; (* 30 60) therefore returns 30 minutes in seconds
;; (run-with-timer 0 (* 120 60) 'nds:elfeed-updater)
;; Alternatively we run at 07:00 each day
(run-at-time "07:00" nil 'nds:elfeed-updater)
(setq httpd-port 8461)
(elfeed-web-start)
