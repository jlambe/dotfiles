;;; jl-emacs-editor.el --- Editor related configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

(require 'jl-utilities)

(defvar jl-fill-column 80
  "Default variable `fill-column' value.")

(defvar jl-scroll-margin 8
  "Default variable `scroll-margin' value.")

(defvar jl-screen-context-lines 8
  "Default variable `next-screen-context-lines' value.")

;; Load the "exec-path-from-shell" package.
;; This package makes Emacs use the $PATH setup by my shell. So any environment
;; variables set within shell scripts (.bashrc, .zshrc,...) are available within
;; Emacs.
;; Load the package only if under macos or X window systems (linux).
(use-package exec-path-from-shell
  :ensure t
  :if (jl-window-system-p (list jl-window-system-macos jl-window-system-x))
  :init (exec-path-from-shell-initialize))

;; Emacs editor global generic configuration.
(use-package emacs
  :init
  ;; Backup
  ;;; Does not create a backup file on first save.
  (setq make-backup-files nil)
  ;;; Disable backup.
  (setq backup-inhibited t)
  ;;; Disable backup if under version control.
  (setq vc-make-backup-files nil)

  ;; MacOS
  ;;; Change meta key modifier support on macos.
  ;;; Set meta key on the right option keyboard key and disable the default left
  ;;; option key as the meta key (macos only).
  (when (jl-macos-p)
    (setq mac-right-option-modifier 'meta
	  mac-option-modifier 'none))

  ;; Customization
  ;;; Setup the file `custom.el' in .emacs.d directory.
  ;;; when the file is available, Emacs stores customization code into that file, in
  ;;; order to avoid to pollute the main file `init.el'.
  (jl-emacs-load-elisp-file "custom.el")

  ;; Scroll bar
  ;;; Disable scroll bar display in buffers.
  (scroll-bar-mode -1)
  ;;; Set scroll-margin to 8 lines to automatically scroll before reaching the top or bottom of a window.
  (setq scroll-margin jl-scroll-margin)
  ;;; Allow to scroll up to the beginning or down to the end of the buffer.
  (setq scroll-error-top-bottom t)
  ;;; Preserve screen position while scrolling.
  (setq scroll-preserve-screen-position t)
  ;;; Number of lines of continuity when scrolling by screenfuls.
  (setq next-screen-context-lines jl-screen-context-lines)

  ;; Menu bar
  ;;; Disable the menu bar.
  (menu-bar-mode -1)

  ;; Tab bar
  ;;; Set tab-bar-show mode to hide if there is only one tab left.
  (setq tab-bar-show 1)

  ;; Tool bar
  ;; Disable the tool bar.
  (tool-bar-mode -1)

  ;; Buffer text
  ;;; Automatically add newlines if moving to next-line at end of buffer.
  (setq next-line-add-newlines t)

  ;; Fill mode
  ;;; Set default variable `fill-column' length.
  (setq fill-column jl-fill-column)

  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode 1)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Advice the `compile' command to work in interactive mode by default.
  (advice-add #'compile :around #'jl-advice-compile-interactive)
  :custom
  ;; Enable indentation+completion using TAB key
  (tab-always-indent 'complete)
  );; (use-package emacs) ends here

;; Dired
(use-package dired
  :hook
  ;; By default, hide all files and directories details.
  ((dired-mode . dired-hide-details-mode)))

;; This package provides additional font locks in Dired mode (prettier UI basically).
;; https://github.com/purcell/diredfl
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

(use-package dired-git-info
  :ensure t)

;; Version Control Management
;; Install Magit.
(use-package magit
  :ensure t)

;; Add version control gutter highlights.
(use-package diff-hl
  :if (display-graphic-p)
  :ensure t
  :config
  (global-diff-hl-mode)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

;; Vterm
(use-package vterm
  :ensure t)

(provide 'jl-emacs-editor)

;;; jl-emacs-editor.el ends here
