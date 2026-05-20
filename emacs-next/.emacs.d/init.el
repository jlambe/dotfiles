;;; init.el --- Emacs initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

;; Dependencies
(require 'jl-utilities)

;; Load the "exec-path-from-shell" package.
;; This package makes Emacs use the $PATH setup by my shell. So any environment
;; variables set within shell scripts (.bashrc, .zshrc,...) are available within
;; Emacs.
;; Load the package only if under macos or X window systems (linux).
(use-package exec-path-from-shell
  :ensure t
  :if (jl-window-system-p (list jl-window-system-macos jl-window-system-x))
  :init (exec-path-from-shell-initialize))

;; Add "jl-lisp" and "jl-emacs" directories to load path.
;; The lisp directory contains additional lisp functions to help me manage my
;; GNU Emacs configuration or elisp development.
;; The emacs directory contains the configuration files to customize Emacs.
(jl-emacs-directory-load-path '("jl-lisp" "jl-emacs"))

;; Add the Melpa packages archive.
(jl-emacs-add-packages '(("melpa" . "https://melpa.org/packages/")))

;; Load my Emacs modules.
(require 'jl-emacs-editor)

;; Personal project files.
;; Load any extra elisp files created for specific project.
;; TODO (julien): should I load paths to external directories?

;;; init.el ends here
