;;; https://github.com/alphapapa/activities.el
(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)

  :bind
  (("C-x C-a n" . activities-new)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)    ; Alias for `-suspend'
   ("C-x C-a a" . activities-resume)
   ;; For convenience, we also bind `activities-resume' to "C-x C-a
   ;; C-a", so the user need not lift the Control key.
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a l" . activities-list)))
