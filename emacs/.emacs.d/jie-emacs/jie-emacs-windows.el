;;; -*- lexical-binding: t -*-

(use-package window
  :config
  (setq switch-to-buffer-obey-display-actions t)
  :custom
  (display-buffer-alist
   '(
     ;; Display the xref buffers in bottom right side of the current frame.
     ("\\*xref\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1))
     ;; Display the eldoc buffer in bottom left side of the current frame.
     ("\\*[Ee]ldoc\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     ;; Display the info buffer on the right side of the current frame.
     ("\\*\\([Hh]elp\\|[Ii]nfo\\)\\*"
	(display-buffer-in-side-window)
	(window-width . 0.5)
	(side . right)
	(slot . 0))
     ;; Display "Apropos" buffer on the right side, bottom of the current frame.
     ("\\*Apropos\\*"
      (display-buffer-in-side-window)
      (window-width . 0.5)
      (side . right)
      (slot . 1))
     ;; Display magit status to its own tab on the current frame.
     ((derived-mode . magit-status-mode)
      (display-buffer-reuse-mode-window display-buffer-in-tab)
      (tab-name . (lambda (buffer _alist) (buffer-name buffer)))
      (reusable-frames . :just-the-selected-frame)
      (mode . magit-status-mode)
      (inhibit-switch-frame . t))
     ;; Display magit diff to the right side of the current frame.
     ("magit-diff"
	(display-buffer-in-side-window)
	(window-width . 0.5)
	(side . right)
	(slot . 1))
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
