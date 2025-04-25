;;; Set tab-bar-show mode to hide if there is only one tab left
(setq tab-bar-show 1)

;;; Navigation/View
;;;; Set scroll-margin to 8 lines to automatically scroll before reaching the top or bottom of a window.
(setq scroll-margin 8)

;;; Disable menu bar
(menu-bar-mode -1)

;;; Disable tool bar
(tool-bar-mode -1)

;;; Default font for macos
(when (string= system-type "darwin")
  (add-to-list 'default-frame-alist
	       '(font . "Menlo 16")))

(provide 'jie-emacs-editor)
