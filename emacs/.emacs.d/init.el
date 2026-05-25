;;; init.el --- Emacs initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

(defun jl-emacs-directory-load-path (paths &optional append)
  "Utility function to add or append load paths.
Add each path from PATHS to the `load-path' global variable.
A path can be relative or absolute.  If relative, the path is concatenated
with user .emacs.d directory path.

If APPEND is t, each path is appended to the `load-path' global variable."
  (when (listp paths)
    (dolist (path paths)
      (if (file-name-absolute-p path)
	  (add-to-list 'load-path (directory-file-name path) append)
	(add-to-list 'load-path (locate-user-emacs-file path) append)))))

(defun jl-emacs-add-packages (packages &optional append)
  "Register PACKAGES to the `package-archives'.
PACKAGES is a list of alist elements where the key is the package
archive name string and the value is the url string.

If APPEND is t, each package archive is appended to
the `package-archives' variable."
  (when (listp packages)
    (dolist (package packages)
      (add-to-list 'package-archives package append))))

;;; Configuration starts here

;; Add "jl-lisp" and "jl-emacs" directories to load path.
;; The lisp directory contains additional lisp functions to help me manage my
;; GNU Emacs configuration or elisp development.
;; The emacs directory contains the configuration files to customize Emacs.
(jl-emacs-directory-load-path '("jl-lisp" "jl-emacs" "/Volumes/Materia/dev/emacs-packages/sailamx"))

;; Add the Melpa packages archive.
(jl-emacs-add-packages '(("melpa" . "https://melpa.org/packages/")))

;; Load my Elisp modules.
(require 'jl-lisp-utilities)
(require 'jl-lisp-advice)

;; Load my Emacs modules.
(require 'jl-emacs-editor)
(require 'jl-emacs-theme)
(require 'jl-emacs-windows)
(require 'jl-emacs-writing)
(require 'jl-emacs-programming)
(require 'jl-emacs-completion)

(require 'jl-emacs-keybindings)

;;; init.el ends here
