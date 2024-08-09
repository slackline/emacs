;;; OSM CONFIGURATION
;;; --------------------------------------
;;;
;;; https://elpa.gnu.org/packages/osm.html#org666a5ba
(use-package osm
  :ensure t
  :init
  :custom
  (osm-server 'default)
  (osm-home '(53.356116 -1.463397 15))
  :bind
  (("C-c o" . osm-prefix-map))
  )
