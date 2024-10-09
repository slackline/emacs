;; CENTAUR TABS CONFIGURATION
;; --------------------------------------
;; https://github.com/ema2159/centaur-tabs
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)

  ;; Via https://themkat.net/2024/01/04/emacs_centaur_tabs.html
  (defun centaur-tabs-buffer-groups ()
    "Groups tabs based on which project root they are in if possible"
    (let ((get-closest-projectile-project
	   (lambda (path)
	     (let ((expanded-path (f-long path)))
	       (-first (lambda (proj)
			 (s-starts-with? proj
					 expanded-path))
		       (-map (lambda (proj)
			       (f-long proj))
			     projectile-known-projects))))))
      (list (cond
	     ;; Group as part of projectile project if directly part of it
	     ((condition-case _err
		  (projectile-project-root)
		(error nil))
	      (f-expand (projectile-project-root)))
	     ;; Try to group as part of projectile project if indirectly part of it (started from the same directory, not yet tracked, or maybe temporary buffer)
	     (get-closest-projectile-project default-directory)
	     ((string-equal "*" (substring (buffer-name) 0 1))
	      "proc-buffers")
	     ;; ... other groupings ...
	     (t
	      "Other")))))
  :init
  (setq centaur-tabs-enable-key-bindings t
	centaur-tabs-style "wave"
	centaur-tabs-set-icons t
	centaur-tabs-set-bar 'under
	x-underline-at-descent-line t
	centaur-tabs-cycle-scope 'default
	centaur-tabs-set-modified-marker t
	centaur-tabs-modified-marker "⏺")
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))
;; ("C-c t C-<right>" . centaur-tabs-move-current-tab-to-right)
;; ("C-c t C-<left>" . centaur-tabs-move-current-tab-to-left))


(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.
;;     Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
;;     All buffer name start with * will group to \"Emacs\".
;;     Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ;;; Attempts to group mastodon buffers, not working 2023-01-02
    ;; ((string-equal "*mastodon" (substring (buffer-name) 0 8))
    ;;  "Mastodon")
    ((or (string-equal "*" (substring (buffer-name) 0 1))
	 (memq major-mode '(magit-process-mode
			    magit-status-mode
			    magit-diff-mode
			    magit-log-mode
			    magit-file-mode
			    magit-blob-mode
 			    magit-blame-mode)))
     "Emacs")
    ;; ((derived-mode-p 'prog-mode) "Editing")
    ;; ((memq major-mode '(python-mode)) "Python")
    ;;; Following attempts to split magit buffers out to their own group, not working 2023-01-02
    ;; ((string-equal "*" (substring (buffer-name) 0 1))
    ;;  "Emacs")
    ;; ((string-equal "magit" (substring (buffer-name) 0 4)) "Magit")
    ;; ((memq major-mode '(magit-process-mode
    ;; 		            magit-status-mode
    ;; 		            magit-diff-mode
    ;; 		            magit-log-mode
    ;; 		            magit-file-mode
    ;; 		            magit-blob-mode
    ;; 		            magit-blame-mode))
    ;;  "Magit")
    ;; ((derived-mode-p 'dired-mode) "Dired")
    ((memq major-mode '(helpful-mode
                        help-mode))
     "Help")
    ((memq major-mode '(org-mode
			org-agenda-clockreport-mode
			org-src-mode
			org-agenda-mode
			org-beamer-mode
			org-indent-mode
			org-bullets-mode
			org-cdlatex-mode
			org-agenda-log-mode
			diary-mode))
     "OrgMode")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))
     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p "*temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
	  (not (file-name-extension name)))
     )))
