;;; C-c i move the point at the beginning of line text
(keymap-global-set "C-c i" 'beginning-of-line-text)

;;; C-E keybinding to scroll down by line
;;; C-Y keybinding to scroll up by line
(keymap-global-set "C-S-E" 'scroll-down-line)
(keymap-global-set "C-S-Y" 'scroll-up-line)

;;; C-c l Move to window right
;;; C-c h Move to window left
;;; C-c j Move to window below
;;; C-c k Move to window up
(keymap-global-set "C-c l" 'windmove-right)
(keymap-global-set "C-c h" 'windmove-left)
(keymap-global-set "C-c j" 'windmove-down)
(keymap-global-set "C-c k" 'windmove-up)

(provide 'jie-emacs-bindings)
