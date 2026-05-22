;;; jl-emacs-windows.el --- Windows and frames configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

(use-package window
  :config
  ;; This variable tells the "swith-to-buffer" command to obey the rules
  ;; declared in the `display-buffer-alist' variable (and others).
  (setq switch-to-buffer-obey-display-actions t)

  ;; When sets to true, the variable makes sure that when splitting a window,
  ;; the windows get the same size, preferably in proportions.
  (setq window-combination-resize t)

  ;; Force "other-window" related functions to never split on vertical axis.
  (setq split-height-threshold nil)
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

     ;; Display the eldoc buffer in bottom right side of the current frame.
     ("\\*[Ee]ldoc\\*"
      (display-buffer-in-side-window)
      (window-width . 0.33)
      (side . bottom)
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

     ;; Display SQL: MySQL in current frame using new tab.
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
     )) ;; display-buffer-alist ends here
  ) ;; use-package ends here

(provide 'jl-emacs-windows)

;;; jl-emacs-windows.el ends here
