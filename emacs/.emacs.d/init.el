(mapc
 (lambda (relative-path)
   (add-to-list 'load-path (locate-user-emacs-file relative-path)))
 '("jie-emacs-lisp" "jie-emacs-modules"))

(setq initial-buffer-choice t)

(setq fill-column 80)

(require 'jie-emacs-orgmode)
(require 'jie-emacs-theme)
(require 'jie-emacs-skeletons)
