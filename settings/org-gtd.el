;;; Org-gtd https://github.com/Trevoke/org-gtd.el/blob/master/doc/org-gtd.org
;;;
;;; Useful...
;;;
;;; https://blog.jethro.dev/posts/capturing_inbox/
;;; https://blog.jethro.dev/posts/processing_inbox/
;;;
;;; Also includes customiastion for org-agenda
(use-package org-gtd
  :ensure t
  :after org
  :init
  (setq org-gtd-update-ack "3.0.0")
  (setq org-gtd-update-ack "4.0.0")
  :demand t
  :custom
  (org-gtd-directory '"~/org/gtd")
  (org-edna-use-inheritance t)
  :config
  (org-edna-mode)
  ;; All your GTD keywords must be in the same sequence
  (setq org-todo-keywords
	'((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL")))

  ;; Then map GTD semantic states to your keywords
  (setopt org-gtd-keyword-mapping
          '((todo . "TODO")      ;; tasks not ready to be acted upon
            (next . "NEXT")      ;; tasks ready to act on immediately
            (wait . "WAIT")      ;; tasks blocked or delegated
            (done . "DONE")      ;; tasks successfully completed
            (canceled . "CNCL"))) ;; tasks that won't be completed
  (setq org-agenda-files (list org-gtd-directory))
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-clarify-map
   ("C-c c" . org-gtd-organize)
   ))


;;; These are copied from the following
;;; https://github.com/jethrokuan/.emacs.d/blob/master/init.el
(defun my/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'my/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defvar my/org-current-effort "1.00"
  "Current effort for agenda item.")

(defun my/org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (setq my/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
			  (with-current-buffer buffer
			    (widen)
			    (goto-char pos)
			    (org-show-context 'agenda)
			    (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
			    (end-of-line 1)
			    (setq newhead (org-get-heading)))
			  (org-agenda-change-all-lines newhead hdmarker))))
