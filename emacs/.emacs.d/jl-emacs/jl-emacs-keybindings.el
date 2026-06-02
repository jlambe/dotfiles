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
(keymap-global-set "C-c C-d" 'duplicate-dwim)

;;; Comment/uncomment region
(keymap-global-set "C-S-c" 'comment-or-uncomment-region)

;;; Vterm
(keymap-global-set "C-x C-t" 'vterm)

;;; Window
(keymap-global-set "<f8>" 'window-toggle-side-windows)

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

;;; Flymake
(keymap-set flymake-mode-map "M-n" 'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" 'flymake-goto-prev-error)

;;; Eglot
(keymap-global-set "C-, D" 'eglot-find-declaration)
(keymap-global-set "C-, i" 'eglot-find-implementation)
(keymap-global-set "C-, x" 'eglot-code-actions)
(keymap-global-set "<f6>" 'eglot-rename)
(keymap-global-set "<f7>" 'eglot-format-buffer)

;;; Xref
(keymap-global-set "C-, ," 'xref-go-back)
(keymap-global-set "C-, C-," 'xref-go-back)
(keymap-global-set "C-, d" 'xref-find-definitions)
(keymap-global-set "C-, 4 d" 'xref-find-definitions-other-window)
(keymap-global-set "C-, r" 'xref-find-references)
(keymap-global-set "C-, R" 'xref-find-references-and-replace)

;;; Consult
(keymap-global-set "C-x b" 'consult-buffer)
(keymap-global-set "C-x 4 b" 'consult-buffer-other-window)
(keymap-global-set "C-x 5 b" 'consult-buffer-other-frame)
(keymap-global-set "C-x t b" 'consult-buffer-other-tab)
(keymap-global-set "C-x r b" 'consult-bookmark)
(keymap-global-set "C-x p b" 'consult-project-buffer)
(keymap-global-set "M-y" 'consult-yank-pop)
(keymap-global-set "M-g f" 'consult-flymake)
(keymap-global-set "M-g g" 'consult-goto-line)
(keymap-global-set "M-g M-g" 'consult-goto-line)
(keymap-global-set "M-g i" 'consult-imenu)
(keymap-global-set "M-s r" 'consult-ripgrep)
(keymap-global-set "M-s g" 'consult-line)

;;; Embark
(keymap-global-set "C-, a" 'embark-act)
(keymap-global-set "C-, e" 'embark-dwim)

;;; Magit
(keymap-global-set "C-x C-g" 'magit-status)
(keymap-global-set "C-x g" 'magit-status)

(provide 'jl-emacs-keybindings)

;;; jl-emacs-keybindings.el ends here
