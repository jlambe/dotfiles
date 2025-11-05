;;; -*- lexical-binding: t -*-
(defun jl-split-window-right-and-switch-to-prev-buffer ()
  "Split window vertically and move current buffer to the right
and switch to previous buffer on selected window."
  (interactive)
  (split-window-right)
  (switch-to-prev-buffer))

(defun jl-file-absolute-path ()
  "Show current buffer file path in minibuffer and automatically
copy it to the clipboard."
  (interactive)
  (let* ((path (buffer-file-name))
	 (name (file-truename path)))
    ;; TODO - Update to have the option to copy relative path from project root
    (kill-new name)
    (message name)))

(provide 'jie-lisp-common)
