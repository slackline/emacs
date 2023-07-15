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

;; ------------------------------------------------------------------------
;; COMMANDS
;; rsync html versions of org files
(defun rsync-html ()
  (interactive)
  (with-shell-interpreter
   :path "~/org"
   :interpreter "bash"
   :form
   (message "rsync org html output to OVH")
   (message (shell-command-to-string "rsync -av ~/org/*.html ~/org/export ~/org/training ovh:~/www/. --exclude='*.org'"))))
