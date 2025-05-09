;;; -*- lexical-binding: t -*-

(mapc
  (lambda (relative-path)
    (add-to-list 'load-path (locate-user-emacs-file relative-path)))
  '("jie-emacs-lisp" "jie-emacs-modules"))

;;; This is required in order to make the PHP Lsp Server Intelliphense work.
;;; I'm currently sharing a version with neovim and simply point to it.
(add-to-list 'exec-path "~/.local/share/nvim/mason/bin")

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

(require 'jie-emacs-completion)
(require 'jie-emacs-editor)
(require 'jie-emacs-orgmode)
(require 'jie-emacs-prog)
(require 'jie-emacs-theme)
(require 'jie-emacs-skeletons)
(require 'jie-emacs-vc)
