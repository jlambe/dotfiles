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

;; Enhance copy/cut commands to automatically copy/cut current line if no region selected
;; https://github.com/fniessen/emacs-leuven/blob/master/docs/emacs-leuven.txt#deletion-and-killing
;; Enhance the kill-region command to either cut selected region or default to current line.
(defun jl-advice-slick-kill-region (function beg end &rest args)
  "Advice FUNCTION for the `kill-region' command using BEG, END and ARGS.
Cut the selected region or current line if no region is active and called interactively."
  (interactive (if (and (use-region-p) (mark t))
                   ;; Return the function arguments to cut active region
                   (list (region-beginning) (region-end))
                 ;; Return the function arguments to cut current line
                 (list (line-beginning-position) (line-beginning-position 2))))
  ;; Let's just call the original function with new arguments from interactive.
  (apply function beg end args))

(provide 'jl-lisp-advice)

;;; jl-lisp-advice.el ends here
