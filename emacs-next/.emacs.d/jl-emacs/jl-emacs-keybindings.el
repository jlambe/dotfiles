;;; jl-emacs-keybindings.el --- Keybindings configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

;; Global keybindings
;;; Undo and redo
(keymap-global-set "C-z" 'undo)
(keymap-global-set "C-S-z" 'undo-redo)

;;; Scroll up and down by line
(keymap-global-set "C-S-y" 'scroll-down-line)
(keymap-global-set "C-S-e" 'scroll-up-line)

;;; Switch between two recent buffers (in same window)
(keymap-global-set "C-6" 'mode-line-other-buffer)

;;; Cursor movements
;;; Move forward to word
(keymap-global-set "C-<right>" 'forward-to-word)

;;; Text manipulation
(keymap-global-set "C-S-d" 'duplicate-dwim)

;;; Comment/uncomment region
(keymap-global-set "C-S-c" 'comment-or-uncomment-region)

;;; Vterm
(keymap-global-set "C-x C-t" 'vterm)

;;; Windmove
;;; Select window on the right
(keymap-global-set "C-S-l" 'windmove-right)
;;; Select window on the left
(keymap-global-set "C-S-h" 'windmove-left)
;;; Select window on the bottom
(keymap-global-set "C-S-j" 'windmove-down)
;;; Select window on the top
(keymap-global-set "C-S-k" 'windmove-up)

;;; Dired
;;; Toggle git info mode in Dired
(keymap-set dired-mode-map ")" 'dired-git-info-mode)

(provide 'jl-emacs-keybindings)

;;; jl-emacs-keybindings.el ends here
