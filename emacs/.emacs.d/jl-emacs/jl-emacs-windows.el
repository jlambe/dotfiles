;;; jl-emacs-windows.el --- Windows and frames configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.
;; Window helper functions inspired from Protesilaos dotfiles:
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-window.el

;;; Code:

(defvar jl-window-sizes
  '(:max-height (lambda() (floor (frame-height) 3))
    :min-height 10
    :max-width (lambda () (floor (frame-width) 4))
    :min-width .2)
  "Property list of maximum and minimum window sizes.
The property keys are `:max-height', `:min-height', `:max-width'
and `:min-width'.  They all accept a value of either a number (integer
or float) or a function.")

(defun jl-window--get-window-size (key)
  "Extract the value of KEY from `jl-window-sizes' variable."
  (when-let* ((value (plist-get jl-window-sizes key)))
    (cond
     ((functionp value)
      (funcall value))
     ((numberp value)
      value)
     (t
      (error "The value of `%s' is neither a number nor a function" key)))))

(defun jl-window-select-fit-size (window)
  "Select WINDOW and resize it.
The resize pertains to the maximum and minimum values for height
and width, per `jl-window-sizes'.
Use this as the `body-function' in a `display-buffer-alist' entry."
  (select-window window)
  (fit-window-to-buffer
   window
   (jl-window--get-window-size :max-height)
   (jl-window--get-window-size :min-height)
   (jl-window--get-window-size :max-width)
   (jl-window--get-window-size :min-width)))

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
     ;; Embark Export
     ("\\*Embark Export.+\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1))

     ;; Man Buffers
     ("\\*Man.+\\*"
      (display-buffer-same-window))

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
      (window-height . fit-window-to-buffer)
      (side . bottom)
      (slot . 1))

     ;; Display the info buffer on the right side of the current frame.
     ("\\*\\([Hh]elp\\|[Ii]nfo\\)\\*"
      (display-buffer-in-side-window)
      (side . right)
      (body-function . jl-window-select-fit-size))

     ;; Display "Apropos" buffer on the right side, bottom of the current frame.
     ("\\*Apropos\\*"
      (display-buffer-in-side-window)
      (window-width . fit-window-to-buffer)
      (side . right)
      (slot . 1))

     ;; Display vterm to its own tab.
     ("\\*vterm\\*"
      (display-buffer-pop-up-window)
      (reusable-frames . :just-the-selected-frame)
      (body-function . jl-window-select-fit-size))

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



