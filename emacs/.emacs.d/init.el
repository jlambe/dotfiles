(mapc
 (lambda (relative-path)
   (add-to-list 'load-path (locate-user-emacs-file relative-path)))
 '("jie-emacs-lisp" "jie-emacs-modules"))

(dolist (package '(("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives package t))

(use-package quelpa
  :ensure t)

(setq initial-buffer-choice t)

(setq fill-column 80)

(setq make-backup-files nil) ;; Does not create a backup file on first save
(setq backup-inhibited t) ;; Disable backup

(require 'jie-emacs-completion)
(require 'jie-emacs-editor)
(require 'jie-emacs-orgmode)
(require 'jie-emacs-prog)
(require 'jie-emacs-theme)
(require 'jie-emacs-skeletons)
(require 'jie-emacs-vc)
