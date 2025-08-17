;;; -*- lexical-binding: t -*-

(use-package doom-themes
   :ensure t
   :config
   (setq doom-themes-enable-bold t
         doom-themes-enable-italic t)
   (load-theme 'doom-one-light t)
   (custom-set-faces
    ;; Customize variables face color.
    `(font-lock-variable-name-face ((t (:foreground ,(doom-color 'blue)))))))

(use-package nerd-icons
  :ensure t)

(provide 'jie-emacs-theme)
