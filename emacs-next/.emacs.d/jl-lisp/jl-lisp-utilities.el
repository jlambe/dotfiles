;;; jl-lisp-utilities.el --- Emacs generic utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.
;; It contains generic utility elisp functions to help me
;; manage and customize my GNU Emacs environment.

;;; Code:

(defconst jl-system-macos 'darwin
  "Variable `system-type' symbol for macos.")

(defconst jl-window-system-macos 'ns
  "Variable `window-system' symbol for macos.")

(defconst jl-window-system-x 'x
  "Variable `window-system' symbol for X window systems.")

(defun jl-macos-p ()
  "Predicate function that return t if system is darwin/macos."
  (eq system-type jl-system-macos))

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

(defun jl-emacs-directory-load-path-when (condition paths &optional append)
  "Utility function to conditionally add load paths.
Add each path from PATHS to the `load-path' global variable if the
CONDITION is non nil.
A path can be relative or absolute.  If relative, the path is concatenated
with user .emacs.d directory path.

If APPEND is t, each path is appended to the `load-path' global variable."
  (when condition
    (jl-emacs-directory-load-path paths)))

(defun jl-emacs-load-elisp-file (filename)
  "Execute a file of Lisp code named FILENAME.
The FILENAME is a relative file path from user .emacs.d directory."
  (when (not (stringp filename))
    (error "Invalid relative filename path for loading"))
  (let ((filepath (locate-user-emacs-file filename)))
    (if (not (file-exists-p filepath))
	(message "Cannot load filename %s, file does not exist at path %s" filename filepath)
      (load filepath))))

(defun jl-emacs-add-packages (packages &optional append)
  "Register PACKAGES to the `package-archives'.
PACKAGES is a list of alist elements where the key is the package
archive name string and the value is the url string.

If APPEND is t, each package archive is appended to the `package-archives' variable."
  (when (listp packages)
    (dolist (package packages)
      (add-to-list 'package-archives package append))))

(defun jl-window-system-p (window-systems)
  "Return t if variable `window-system' symbol is in WINDOW-SYSTEMS list."
  (when (listp window-systems)
    (let ((result (memq window-system window-systems)))
      (not (not result)))))

(defun jl-copy-file-path (arg)
  "Copy to clipboard (kill ring) current buffer file path.
By default, file path is relative to current project root directory.
If universal ARG is set, the absolute path of the buffer file is used."
  (interactive "p")
  (let* ((path (file-truename (buffer-file-name)))
         (dir (project-root (project-current)))
         (relative-path (file-relative-name path dir)))
    (if (> arg 1)
        (kill-new path)
      (kill-new relative-path))))

(provide 'jl-lisp-utilities)

;;; jl-lisp-utilities.el ends here
