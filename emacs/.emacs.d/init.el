;;; -*- lexical-binding: t -*-

(mapc
  (lambda (relative-path)
    (add-to-list 'load-path (locate-user-emacs-file relative-path)))
  '("jie-lisp" "jie-emacs"))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

(dolist (package '(("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives package t))

(use-package quelpa
  :ensure t)

(setq initial-buffer-choice t)

;;; When working on macos, keep the right option key as the meta modifier
;;; and disable the left option key to work as a default alt key so I can
;;; special characters from my ch-fr layout.
(when (string= system-type "darwin")
  (setq mac-right-option-modifier 'meta
	mac-option-modifier 'none))

(setq fill-column 80)

(setq make-backup-files nil) ;; Does not create a backup file on first save
(setq backup-inhibited t) ;; Disable backup

;; Emacs lisp functions.
(require 'jie-lisp-orgmode)
(require 'jie-lisp-prog)

;; Emacs modules and their configuration.
(require 'jie-emacs-completion)
(require 'jie-emacs-editor)
(require 'jie-emacs-windows)
(require 'jie-emacs-orgmode)
(require 'jie-emacs-writing)
(require 'jie-emacs-prog)
(require 'jie-emacs-theme)
(require 'jie-emacs-skeletons)
(require 'jie-emacs-vc)
(require 'jie-emacs-vterm)
(require 'jie-emacs-bindings)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cape corfu-terminal doom-themes ef-themes exec-path-from-shell
	  json-mode magit marginalia markdown-mode nerd-icons
	  orderless php-mode quelpa vertico vterm web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
