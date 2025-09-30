;; MISCELLANEOUS CONFIGURATION
;; --------------------------------------

;; Inserting Wikipedia links (http://ergoemacs.org/emacs/elisp_html_word_to_wikipedia_linkify.html)
;; Tweaked to insert an org-link instead though
(defun wikipedia-linkify ()
  "Make the current word or text selection into a Wikipedia link.
For Example:
 Emacs
becomes
 <a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>

URL `http://ergoemacs.org/emacs/elisp_html_word_to_wikipedia_linkify.html'
Version 2015-07-27"
  (interactive)
  (let ($p0 $p1 $p2 $linkText)
    (if (region-active-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (progn
        (setq $p0 (point))
        (skip-chars-backward "^ \t\n")
        (setq $p1 (point))
        (goto-char $p0)
        (skip-chars-forward "^ \t\n")
        (setq $p2 (point))))
    (setq $linkText
          (replace-regexp-in-string "_" " " (buffer-substring-no-properties $p1 $p2)))
    (delete-region $p1 $p2)
    (insert (concat "[[http://en.wikipedia.org/wiki/"
                    (replace-regexp-in-string " " "_" $linkText)
                    "]][[" $linkText "]]"))))

;; Create non-existent directory
;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
(defun er-auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)

;; Info version of Structure and Interpretation of Computer Programs (2nd Edition)
;; https://mitpress.mit.edu/9780262510875/structure-and-interpretation-of-computer-programs/
;; (use-package sicp)

;; https://github.com/lmq-10/iso-date
(use-package iso-date
  :ensure t)
