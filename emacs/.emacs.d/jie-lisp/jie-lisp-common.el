;;; -*- lexical-binding: t -*-
(defun jl-copy-file-path (arg)
  "Set current buffer relative project file path the latest kill in the kill ring
and automatically copy it to the clipboard. If universal ARG is set,
the absolute path of the buffer file is used."
  (interactive "p")
  (let* ((path (file-truename (buffer-file-name)))
         (dir (project-root (project-current)))
         (relative-path (file-relative-name path dir)))
    (if (> arg 1)
        (Kill-new path)
      (kill-new relative-path))))

(defun jl-current-frame-width ()
  "Get the current frame width in pixels."
  (interactive)
  (frame-native-width))

(defun jl-get-width-ratio (desired-ratio fallback-ratio)
  "Return the DESIRED-RATIO width if current frame width
is equal or larger than 1920 pixels, the FALLBACK-RATIO otherwise."
  (if (>= (jl-current-frame-width) 1920)
      desired-ratio
    fallback-ratio))

(provide 'jie-lisp-common)
