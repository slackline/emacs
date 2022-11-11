;;; OSM CONFIGURATION
;;; --------------------------------------
;;;
;;; https://elpa.gnu.org/packages/osm.html#org666a5ba
(use-package osm
	     :bind (("C-c m h" . osm-home)
		    ("C-c m s" . osm-search)
		    ("C-c m v" . osm-server)
		    ("C-c m t" . osm-goto)
		    ("C-c m x" . osm-gpx-show)
		    ("C-c m j" . osm-bookmark-jump))
	     :init
	     ;; Load Org link support
	     (with-eval-after-load 'org
	       (require 'osm-ol))
	     :custom
	     (osm-server 'default)
	     (osm-home '(53.356116 -1.463397 15))
	     )
