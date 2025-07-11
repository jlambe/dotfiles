;;; -*- lexical-binding: t -*-

(use-package doom-themes
   :ensure t
   :config
   (setq doom-themes-enable-bold t
         doom-themes-enable-italic t)
   (load-theme 'doom-one-light t))

(provide 'jie-emacs-theme)
