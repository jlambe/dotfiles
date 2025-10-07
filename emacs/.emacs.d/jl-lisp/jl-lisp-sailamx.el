;;; -*- lexical-binding: t -*-
(defun sailamx-fetch-fast-forward-staging ()
    "Utility command to fetch fast-forward the project staging branch."
    (interactive)
    (let* (
	   (git-process (make-process
			 :name "Git Fetch Fast Forward"
			 :command '("git" "fetch" "origin" "staging:staging")
			 :buffer (get-buffer-create "*git fetch*")
			 ))
	   )
    (message "%s" buffer)))

(provide 'jl-lisp-sailamx)
