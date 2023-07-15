;;; OSM CONFIGURATION
;;; --------------------------------------
;;;
;;; https://elpa.gnu.org/packages/osm.html#org666a5ba
(use-package osm
	     :bind (("C-c o h" . osm-home)
		    ("C-c o s" . osm-search)
		    ("C-c o v" . osm-server)
		    ("C-c o t" . osm-goto)
		    ("C-c o x" . osm-gpx-show)
		    ("C-c o j" . osm-bookmark-jump))
	     :init
	     ;; Load Org link support
	     (with-eval-after-load 'org
	       (require 'osm-ol))
	     :custom
	     (osm-server 'default)
	     (osm-home '(53.356116 -1.463397 15))
	     )
