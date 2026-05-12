;;; jiel-init.el --- Emacs generic utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.
;; It contains generic utility elisp functions to help me
;; manage and customize my GNU Emacs environment.

;;; Code:

(defconst jiel-system-macos 'darwin
  "MacOS `system-type' symbol.")

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

(defun jiel-emacs-disable-backup ()
  "Disable Emacs backup file support."
  (setq make-backup-files nil      ;; Does not create a backup file on first save.
	backup-inhibited t         ;; Disable backup.
	vc-make-backup-files nil)) ;; Disable backup if under version control.

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

(provide 'jiel-init)

;;; jiel-init.el ends here
