;;; -*- lexical-binding: t -*-
(use-package window
  :config
  (setq switch-to-buffer-obey-display-actions t
	window-combination-resize t
	;; Force "other-window" related functions to never split on vertical axis.
	split-height-threshold nil)
  :custom
  (display-buffer-alist
   `(
     ;; Always hide the Async Shell Command standard output buffer by default
     ("\\*Async Shell Command\\*" (display-buffer-no-window))
     ;; Display the xref buffers in bottom right side of the current frame.
     ;; Height is controlled by the number of "Vertico" items.
     ("\\*xref\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1))
     ;; Display the eldoc buffer in bottom left side of the current frame.
     ("\\*[Ee]ldoc\\*"
      (display-buffer-in-side-window)
      (window-width . 0.33)
      (side . right)
      (slot . 1))
     ;; Display the info buffer on the right side of the current frame.
     ("\\*\\([Hh]elp\\|[Ii]nfo\\)\\*"
	(display-buffer-in-side-window)
	(window-width . 0.33)
	(side . right)
	(slot . 0))
     ;; Display "Apropos" buffer on the right side, bottom of the current frame.
     ("\\*Apropos\\*"
      (display-buffer-in-side-window)
      (window-width . 0.33)
      (side . right)
      (slot . 1))
     ;; Display vterm to its own tab.
     ("\\*vterm\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.5)
      (slot . -2)
      (reusable-frames . :just-the-selected-frame))
     ;; Display SQL: MySQL in current window using full frame.
     ;; Just to test, might still want to move it to its own tab later on.
     ("\\*SQL: MySQL\\*"
      (display-buffer-reuse-window display-buffer-full-frame))
     ;; Display magit status to its own tab on the current frame.
     ((derived-mode . magit-status-mode)
      (display-buffer-reuse-mode-window display-buffer-in-tab)
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
