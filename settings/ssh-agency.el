;; https://github.com/magit/ssh-agency
(use-package ssh-agency
  :ensure t
  :custom (ssh-agency-keys '("~/.ssh/id_ed25519" "~/.ssh/haldane_ed25519")))
