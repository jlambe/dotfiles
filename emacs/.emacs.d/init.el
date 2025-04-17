(mapc
 (lambda (relative-path)
   (add-to-list 'load-path (locate-user-emacs-file relative-path)))
 '("jie-emacs-lisp" "jie-emacs-modules"))

;;; Path to currently installed Node binaries.
;;; This is required in order to make the PHP Lsp Server Intelliphense work.
(add-to-list 'exec-path "~/.config/nvm/versions/node/v23.10.0/bin")

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(marginalia magit-popup geiser-guile edit-indirect bui web-mode vertico quelpa php-mode orderless magit ef-themes doom-themes corfu-terminal cape)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
