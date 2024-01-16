;;; https://git.sr.ht/~protesilaos/pulsar
(use-package pulsar
  :custom
  (pulsar-pulse-functions '(recenter-top-bottom
                            move-to-window-line-top-bottom
                            reposition-window
                            bookmark-jump
                            other-window
                            delete-window
                            delete-other-windows
                            aw-delete-window
                            forward-page
                            backward-page
                            scroll-up-command
                            scroll-down-command
                            evil-window-right
                            evil-window-left
                            evil-window-up
                            evil-window-down
                            aw-move-window
                            aw-swap-window
                            aw-copy-window
                            aw-split-window-vert
                            aw-split-window-horz
                            aw-split-window-fair
                            ha-new-window
                            winum-select-window-1
                            winum-select-window-2
                            winum-select-window-3
                            winum-select-window-4
                            winum-select-window-5
                            winum-select-window-6
                            winum-select-window-7
                            winum-select-window-8
                            winum-select-window-9
                            winner-undo
                            winner-redo
                            tab-new
                            tab-close
                            tab-next
                            org-next-visible-heading
                            org-previous-visible-heading
                            org-forward-heading-same-level
                            org-backward-heading-same-level
                            outline-backward-same-level
                            outline-forward-same-level
                            outline-next-visible-heading
                            outline-previous-visible-heading
                            outline-up-heading))
  (pulsar-face 'pulsar-generic)
  (pulsar-delay 0.05)
  :bind ("<f8>" . pulsar-pulse-line)
  :config (pulsar-global-mode 1))
