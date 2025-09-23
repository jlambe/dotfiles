;;; -*- lexical-binding: t -*-
(use-package magit
  :ensure t)

;; Add version control gutter highlights.
(use-package diff-hl
  :if (display-graphic-p)
  :ensure t
  :config
  (global-diff-hl-mode)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

(provide 'jie-emacs-vc)
