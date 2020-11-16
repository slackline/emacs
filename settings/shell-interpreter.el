;; SHELL COMMANDS
;; --------------------------------------
;;
;; Examples : https://www.eigenbahn.com/2020/01/19/painless-emacs-shell-commands
(use-package with-shell-interpreter)

;; ------------------------------------------------------------------------
;; VARS
(defvar prf-default-remote-shell-interpreter "/bin/bash")
(defvar prf-default-remote-shell-interpreter-args '("-c" "export EMACS=; export TERM=dumb; stty echo; bash"))
(defvar prf-default-remote-shell-interpreter-command-switch "-c")

;; ;; ------------------------------------------------------------------------
;; ;; MAIN
;; (cl-defun eval-with-shell-interpreter (&key form path
;;                                             interpreter interpreter-args command-switch)
;;   (unless path
;;     (setq path default-directory))
;;   (unless (file-exists-p path)
;;     (error "Path %s doesn't seem to exist" path))

;;   (let* ((func
;;           (if (functionp form) form
;;             ;; Try to use the "current" lexical/dynamic mode for `form'.
;;             (eval `(lambda () ,form) lexical-binding)))
;;          (is-remote (file-remote-p path))
;;          (interpreter (or interpreter
;;                           (if is-remote
;;                               prf-default-remote-shell-interpreter
;;                             shell-file-name)))
;;          (interpreter (with-shell-interpreter--normalize-path interpreter))
;;          (interpreter-name (with-shell-interpreter--get-interpreter-name interpreter))
;;          (explicit-interpreter-args-var (intern (concat "explicit-" interpreter-name "-args")))
;;          (interpreter-args (or interpreter-args (when is-remote prf-default-remote-shell-interpreter-args)))
;;          (command-switch (or command-switch
;;                              (if is-remote
;;                                  prf-default-remote-shell-interpreter-command-switch
;;                                shell-command-switch)))
;;          (default-directory path)
;;          (shell-file-name interpreter)
;;          (explicit-shell-file-name interpreter)
;;          (shell-command-switch command-switch))
;;     (cl-progv
;;         (list explicit-interpreter-args-var)
;;         (list (or interpreter-args
;;                   (when (boundp explicit-interpreter-args-var)
;;                     (symbol-value explicit-interpreter-args-var))))
;;       (funcall func))))



;; ------------------------------------------------------------------------
;; COMMANDS
;; rsync html versions of org files
(defun rsync_html ()
  (interactive)
  (with-shell-interpreter
   :path "~/org"
   :interpreter "bash"
   :form
   (message "Rsync org to OVH")
   (message (shell-command-to-string "rsync ~/org/*.html ~/org/export ~/org/training ovh:~/www/. --exclude='*.org'"))))
