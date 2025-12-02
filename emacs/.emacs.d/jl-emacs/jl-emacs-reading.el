;;; -*- lexical-binding: t -*-

(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'jl-emacs-reading)
