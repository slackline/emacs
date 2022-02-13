;;; HOST SPECIFIC SETTINGS
;;; ======================
;;;
;;; Inspired by https://github.com/paul-jewell/guix-home/blob/master/home/files/emacs/site-specific.el
(defun ns/is-linux-p ()
  "True if run in linux environment."
  (string= "gnu/linux" system-type))
;;; Generic function for finding out if on a given system
(defun ns/is-host-p (name)
  "True if run in linux environment."
  (string= name (system-name)))
;;; Host specific
(defun ns/is-kimura-p ()
  "True if run in linux environment."
  (string= "kimura" (system-name)))
(defun ns/is-alarmpi-4b-p ()
  "True if run in linux environment."
  (string= "alarmpi-4b" (system-name)))
(defun ns/is-vps-bb669593-p ()
  "True if run in linux environment."
  (string= "vps-bb669593" (system-name)))
