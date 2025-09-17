;;; -*- lexical-binding: t -*-
  (defun jl-split-window-right-and-switch-to-prev-buffer ()
    "Split window vertically and move current buffer to the right
and switch to previous buffer on selected window."
    (interactive)
    (split-window-right)
    (switch-to-prev-buffer))

(provide 'jie-lisp-common)
