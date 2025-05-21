;;; C-A move the point at the beginning of line text
(keymap-global-set "C-S-a" 'beginning-of-line-text)

;;; C-E keybinding to scroll up by line
;;; C-Y keybinding to scroll down by line
(keymap-global-set "C-S-Y" 'scroll-down-line)
(keymap-global-set "C-S-E" 'scroll-up-line)

;;; C-L Move to window right
;;; C-H Move to window left
;;; C-J Move to window below
;;; C-K Move to window up
(keymap-global-set "C-S-l" 'windmove-right)
(keymap-global-set "C-S-h" 'windmove-left)
(keymap-global-set "C-S-j" 'windmove-down)
(keymap-global-set "C-S-k" 'windmove-up)

;;; LSP related keybindings
(keymap-global-set "C-, C-," 'xref-go-back)
(keymap-global-set "C-, C-d" 'xref-find-definitions)
(keymap-global-set "C-, C-r" 'xref-find-references)

(keymap-global-set "C-, C-S-d" 'eglot-find-declaration)
(keymap-global-set "<f7>" 'eglot-format-buffer)
(keymap-global-set "<f6>" 'eglot-rename)

(provide 'jie-emacs-bindings)
