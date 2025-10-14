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
  :ensure t
  :custom
  (nerd-icons-font-family "CodeNewRoman Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :if (display-graphic-p)
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-xref
  :ensure t
  :if (display-graphic-p)
  :after xref
  :config
  (nerd-icons-xref-mode 1))

(use-package nerd-icons-grep
  :ensure t
  :if (display-graphic-p)
  :after grep
  :config
  (when grep-use-headings
    (nerd-icons-grep-mode 1)))

(use-package rainbow-mode
  :ensure t
  :config
  (rainbow-mode))

(provide 'jie-emacs-theme)
