;;; jl-emacs-theme.el --- Theme configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

(defvar jl-default-font-family "DejaVuSansM Nerd Font Mono"
  "Default font family name.")

(defvar jl-default-theme 'ef-light
  "Default theme to load.
Currently only supporting ef-themes.")

(use-package emacs
  :init
  ;; Default font
  ;;; Specify a default font in the `default-frame-alist' variable.
  ;;; This is setting the default font for all new graphical frames.
  ;;; Follows the Fontconfig pattern.
  ;;; See Emacs > Fonts documentation for details.
  (add-to-list 'default-frame-alist '(font . "DejaVuSansM Nerd Font Mono-14"))
  )

;; Default theme
(use-package ef-themes
  :ensure t
  :init
  ;; Indicates modus theme functions to care only about ef-themes.
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  ;; Load the default theme.
  (modus-themes-load-theme jl-default-theme))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family jl-default-font-family))

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

(provide 'jl-emacs-theme)

;;; jl-emacs-theme.el ends here
