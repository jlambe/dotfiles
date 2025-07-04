;;; -*- lexical-binding: t -*-

;; (use-package catppuccin-theme
;;   :ensure t
;;   :config
;;   (setq catppuccin-flavor 'latte)
;;   (load-theme 'catppuccin t))

(use-package doom-themes
   :ensure t
   :config
   (setq doom-themes-enable-bold t
         doom-themes-enable-italic t)
   (load-theme 'doom-one-light t))

(provide 'jie-emacs-theme)
