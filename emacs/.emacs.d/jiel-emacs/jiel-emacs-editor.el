;;; jiel-emacs-editor.el --- Editor related configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

(require 'jiel-utilities)

(defvar jiel-fill-column 80
  "Default variable `fill-column' value.")

(defvar jiel-scroll-margin 8
  "Default variable `scroll-margin' value.")

(defvar jiel-screen-context-lines 8
  "Default variable `next-screen-context-lines' value.")

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
  (when (jiel-macos-p)
    (setq mac-right-option-modifier 'meta
	  mac-option-modifier 'none))

  ;; Scroll bar
  ;;; Disable scroll bar display in buffers.
  (scroll-bar-mode -1)
  ;;; Set scroll-margin to 8 lines to automatically scroll before reaching the top or bottom of a window.
  (setq scroll-margin jiel-scroll-margin)
  ;;; Allow to scroll up to the beginning or down to the end of the buffer.
  (setq scroll-error-top-bottom t)
  ;;; Preserve screen position while scrolling.
  (setq scroll-preserve-screen-position t)
  ;;; Number of lines of continuity when scrolling by screenfuls.
  (setq next-screen-context-lines jiel-screen-context-lines)

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
  (setq fill-column jiel-fill-column)
  );; (use-package emacs) ends here

(provide 'jiel-emacs-editor)

;;; jiel-emacs-editor.el ends here
