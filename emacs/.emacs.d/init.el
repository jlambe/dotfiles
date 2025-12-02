;;; -*- lexical-binding: t -*-

(mapc
  (lambda (relative-path)
    (add-to-list 'load-path (locate-user-emacs-file relative-path)))
  '("jie-lisp" "jie-emacs" "jl-lisp" "jl-emacs"))

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
(setq vc-make-backup-files nil) ;; Disable backup if under version control

;; Emacs lisp functions.
(require 'jie-lisp-common)
(require 'jie-lisp-orgmode)
(require 'jie-lisp-prog)

;; Emacs modules and their configuration.
(require 'jie-emacs-completion)
(require 'jie-emacs-editor)
(require 'jie-emacs-windows)
(require 'jl-emacs-comint)
(require 'jie-emacs-orgmode)
(require 'jie-emacs-writing)
(require 'jie-emacs-prog)
(require 'jie-emacs-theme)
(require 'jie-emacs-skeletons)
(require 'jie-emacs-vc)
(require 'jie-emacs-vterm)
(require 'jie-emacs-bindings)
(require 'jl-emacs-reading)
