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
