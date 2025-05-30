;;; -*- lexical-binding: t -*-

;; Function to handle the creation of a new note using org mode capture.
(defun jie-org-capture-create-note ()
  "Handle creation of a new captured note."
  (call-interactively 'find-file))

(provide 'jie-lisp-orgmode)
