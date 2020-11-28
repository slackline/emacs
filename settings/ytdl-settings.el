;; YTDL CONFIGURATION
;; --------------------------------------
(use-package ytdl
  :init
    (setq ytdl-music-folder (expand-file-name "~/music/downloads")
	  ytdl-video-folder (expand-file-name "~/videos/downloads")
	  ytdl-always-query-default-filename nil
	  ytdl-always-ask-delete-confirmation t)
  )

(ytdl-add-field-in-download-type-list "podcasts"
                                       "p"
                                       (expand-file-name "~/music/podcasts/downloads")
                                       nil)
