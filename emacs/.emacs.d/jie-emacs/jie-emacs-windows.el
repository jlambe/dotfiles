;;; -*- lexical-binding: t -*-

(use-package window
  :config
  (setq switch-to-buffer-obey-display-actions t)
  :custom
  (display-buffer-alist
   '(
     ;; Display the help and xref buffers in bottom right side-window.
     ("\\*\\([Hh]elp\\|xref\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1))
     ;; Display the eldoc buffer in bottom left side-window.
     ("\\*[Ee]ldoc\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     ;; Display magit status to its own tab.
     ((derived-mode . magit-status-mode)
      (display-buffer-reuse-mode-window display-buffer-in-tab)
	(mode . magit-status-mode))
     ))
  :bind
  ;; Toggle on/off display of side windows while pressing the F8 function key.
  ("<f8>" . window-toggle-side-windows))

;; C-L Move to window right
;; C-H Move to window left
;; C-J Move to window below
;; C-K Move to window up
(use-package windmove
  :bind (("C-S-l" . windmove-right)
	 ("C-S-h" . windmove-left)
	 ("C-S-j" . windmove-down)
	 ("C-S-k" . windmove-up)))

(provide 'jie-emacs-windows)
