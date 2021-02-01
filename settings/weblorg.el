;;; WEBLORG CONFIGURATION
;;; --------------------------------------
;;; https://emacs.love/weblorg/posts/v0-1-1-we-re-live.html
(use-package weblorg
  :name "posts"
  :input-pattern "~/org/posts/*.org"
  :template "post.html"
  :output "./posts/{{ slug }}.html"
  :url "/posts/{{ slug }}.html")

(weblorg-copy-static
  :output "static/{{ file }}"
  :url "/static/{{ file }}")

(weblorg-export)
