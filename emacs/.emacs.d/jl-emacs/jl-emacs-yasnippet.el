;;; -*- lexical-binding: t -*-
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  :hook
  ((prog-mode . yas-minor-mode)))

;; https://github.com/elken/yasnippet-capf
;; Add completion at point function to expand yasnippets in buffer.
;; This is required as I'm using `cape with `corfu to handle completion
;; inside buffers.
(use-package yasnippet-capf
  :ensure t
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'jl-emacs-yasnippet)
