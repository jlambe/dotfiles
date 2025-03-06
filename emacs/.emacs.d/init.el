(mapc
 (lambda (relative-path)
   (add-to-list 'load-path (locate-user-emacs-file relative-path)))
 '("jie-emacs-lisp" "jie-emacs-modules"))

(dolist (package '(("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives package t))

(setq initial-buffer-choice t)

(setq fill-column 80)

(require 'jie-emacs-completion)
(require 'jie-emacs-editor)
(require 'jie-emacs-orgmode)
(require 'jie-emacs-prog)
(require 'jie-emacs-theme)
(require 'jie-emacs-skeletons)
