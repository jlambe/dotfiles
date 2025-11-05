;;; -*- lexical-binding: t -*-
(defun jl-split-window-right-and-switch-to-prev-buffer ()
  "Split window vertically and move current buffer to the right
    and switch to previous buffer on selected window."
  (interactive)
  (split-window-right)
  (switch-to-prev-buffer))

(defun jl-copy-file-path (arg)
  "Set current buffer file path the latest kill in the kill ring
     and automatically copy it to the clipboard. If universal ARG is set,
the absolute path of the buffer file is used."
  (interactive "p")
  (let* ((path (file-truename (buffer-file-name)))
         (dir (project-root (project-current)))
         (relative-path (file-relative-name path dir)))
    (if (> arg 1)
        (kill-new path)
      (kill-new relative-path))))

(provide 'jie-lisp-common)
