;;; jiel-init.el --- Emacs initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

(require 'jiel-utilities)

;; Load the "exec-path-from-shell" package.
;; This package makes Emacs use the $PATH setup by my shell. So any environment
;; variables set within shell scripts (.bashrc, .zshrc,...) are available within
;; Emacs.
;; Load the package only if under macos or X window systems (linux).
(use-package exec-path-from-shell
  :ensure t
  :if (jiel-window-system-p (list jiel-window-system-macos jiel-window-system-x))
  :init (exec-path-from-shell-initialize))

;; Add "jiel-lisp" and "jiel-emacs" directories to load path.
;; The lisp directory contains additional lisp functions to help me manage my
;; GNU Emacs configuration or elisp development.
;; The emacs directory contains the configuration files to customize Emacs.
(jiel-emacs-directory-load-path '("jiel-lisp" "jiel-emacs"))

;; Personal project files.
;; Load any extra elisp files created for specific project.
;; TODO?
