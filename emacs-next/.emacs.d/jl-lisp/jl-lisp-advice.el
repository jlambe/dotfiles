;;; jl-lisp-advice.el --- Emacs advice functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.
;; The file contains function declarations for advice usage.

;;; Code:

;; Always make the "compile" command to run "interactively" instead of just displaying program output.
;; Idea coming from MasteringEmacs: https://www.masteringemacs.org/article/compiling-running-scripts-emacs
(defun jl-advice-compile-interactive (function &rest args)
  "Advice FUNCTION for the `compile' command using ARGS.
Sets its COMINT argument to t in order to run interactively."
  (let ((command (car args)))
    (apply function (list command t))))

(provide 'jl-lisp-advice)

;;; jl-lisp-advice.el ends here
