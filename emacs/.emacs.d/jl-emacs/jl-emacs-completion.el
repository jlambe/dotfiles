;;; jl-emacs-completion.el --- Completion configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

;;; Orderless
;;; The [orderless] package provides enhanced fuzzy search during
;;; completion. The current configuration here is putting the "orderless"
;;; style up front.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )

;;; Use the Vertico package to get vertical minibuffer UI.
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-count 16)
  :init (vertico-mode))

;;; Use Marginalia package to add annotations on the minibuffer entries.
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;;; Use the Corfu package for buffer completion UI
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode)

  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; c-mode completion fix
  ;; Use TAB within c-mode to perform completion
  ;; For what I understand, it maps "completion-at-point" on the current
  ;; key TAB (replace default function).
  ;; Disable this code snippet if c-mode does not behave normally.
  (when (equal tab-always-indent 'complete)
    (define-key c-mode-base-map [remap c-indent-line-or-region] #'completion-at-point))
  :config
  ;; Enable corfu-terminal-mode if not using Emacs GUI
  (add-hook 'corfu-mode-hook (lambda ()
			       (unless (display-graphic-p)
				 (corfu-terminal-mode 1)))))

  ;;; Add Capfs capabilites
(use-package cape
  :init
  ;; This one is important, makes eglot completion "full"
  (add-hook 'completion-at-point-functions #'eglot-completion-at-point)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

  ;;; Use Consult package
(use-package consult
  :ensure t
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package popon
  :ensure t)

;; Only enables the package if on Emacs version < 31.
(use-package corfu-terminal
  :ensure t
  :if (< (string-to-number emacs-version) 31)
  :after '(corfu popon))

;; Snippets
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

(provide 'jl-emacs-completion)

;;; jl-emacs-completion.el ends here
