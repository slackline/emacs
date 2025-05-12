;; https://github.com/anonimitoraf/exercism.el

(use-package exercism
  :ensure t
  :config
  (global-set-key (kbd "C-c x") #'exercism))

;; Submits the current buffer to exercism and opens a temp buffer with the output and some additional information like
;; the URL of the current practice. Via https://emacs.christianbaeuerlein.com/#orgabe4cc7
(defun exercism-submit ()
  (interactive)
  (with-output-to-temp-buffer "*exercism*"
    (princ(shell-command-to-string (concat "exercism submit " (buffer-file-name))))
    )
  (pop-to-buffer "*exercism*"))
