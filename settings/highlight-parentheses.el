;;;
(use-package highlight-parentheses
  :config
  (setq hl-paren-colors '("gold" "IndianRed" "cyan" "green" "orange" "magenta"))
  :hook
  (prog-mode-hook . highlight-highlight-mode))
