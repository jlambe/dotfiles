;;; C-i move the point at the beginning of line text
(keymap-global-set "C-i" 'beginning-of-line-text)

;;; C-E keybinding to scroll down by line
;;; C-Y keybinding to scroll up by line
(keymap-global-set "C-S-E" 'scroll-down-line)
(keymap-global-set "C-S-Y" 'scroll-up-line)

(provide 'jie-emacs-bindings)
