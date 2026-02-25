;;; -*- lexical-binding: t -*-

(use-package dired
  :hook
  ((dired-mode . dired-hide-details-mode)))

;; This package provides additional font locks in Dired mode (prettier UI basically).
;; https://github.com/purcell/diredfl
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

(use-package dired-git-info
  :ensure t
  :bind
  (
   :map dired-mode-map
	(")" . dired-git-info-mode)))

(provide 'jl-emacs-dired)
