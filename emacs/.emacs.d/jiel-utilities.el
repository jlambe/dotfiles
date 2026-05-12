;;; jiel-utilities.el --- Emacs generic utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.
;; It contains generic utility elisp functions to help me
;; manage and customize my GNU Emacs environment.

;;; Code:

(defconst jiel-system-macos 'darwin
  "Variable `system-type' symbol for macos.")

(defconst jiel-window-system-macos 'ns
  "Variable `window-system' symbol for macos.")

(defconst jiel-window-system-x 'x
  "Variable `window-system' symbol for X window systems.")

(defun jiel-macos-p ()
  "Predicate function that return t if system is darwin/macos."
  (eq system-type jiel-system-macos))

(defun jiel-macos-default-modifiers ()
  "Setup macos default keyboard modifiers.
Set the right option modifier key as the meta key."
  (when (jiel-macos-p)
    (setq mac-right-option-modifier 'meta
	  mac-option-modifier 'none)))

(defun jiel-emacs-directory-load-path (paths &optional append)
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

(defun jiel-emacs-directory-load-path-when (condition paths &optional append)
  "Utility function to conditionally add load paths.
Add each path from PATHS to the `load-path' global variable if the
CONDITION is non nil.
A path can be relative or absolute.  If relative, the path is concatenated
with user .emacs.d directory path.

If APPEND is t, each path is appended to the `load-path' global variable."
  (when condition
    (jiel-emacs-directory-load-path paths)))

(defun jiel-emacs-load-elisp-file (filename)
  "Execute a file of Lisp code named FILENAME.
The FILENAME is a relative file path from user .emacs.d directory."
  (when (not (stringp filename))
    (error "Invalid relative filename path for loading"))
  (load (locate-user-emacs-file filename)))

(defun jiel-emacs-add-packages (packages &optional append)
  "Register PACKAGES to the `package-archives'.
PACKAGES is a list of alist elements where the key is the package
archive name string and the value is the url string.

If APPEND is t, each package archive is appended to the `package-archives' variable."
  (when (listp packages)
    (dolist (package packages)
      (add-to-list 'package-archives package append))))

(defun jiel-emacs-disable-backup ()
  "Disable Emacs backup file support.
The function sets the following variables:

- `make-backup-files': set to nil so it does not create a backup file on first
  save.
- `backup-inhibited': set to t to disable backup.
- `vc-make-backup-files': set to nil to disable backup if under version control."
  (setq make-backup-files nil
	backup-inhibited t
	vc-make-backup-files nil))

(defun jiel-window-system-p (window-systems)
  "Return t if variable `window-system' symbol is in WINDOW-SYSTEMS list."
  (when (listp window-systems)
    (let ((result (memq window-system window-systems)))
      (not (not result)))))

(defun jiel-copy-file-path (arg)
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

(provide 'jiel-utilities)

;;; jiel-utilities.el ends here
