(setq
 erc-nick "slackline"
 erc-user-fill-name "Neil")
(defun libera-chat ()
  (interactive)
  (erc :server "libera.chat"
       :port "6667"))

(use-package erc
  :ensure t
  :init
  :config
  (setq erc-nick "slackline")
  (setq erc-user-fill-name "Neil")
  )
