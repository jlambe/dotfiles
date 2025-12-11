;;; -*- lexical-binding: t -*-
;; Turn on hl-line-mode when programming.
;; Highlight the current line.
(use-package emacs
  :hook
  ((prog-mode . hl-line-mode)
   (prog-mode . (lambda()
		  ;; Display file name (absolute) of currently visited
		  ;; buffer in frame title bar (if supported by window manager)
		  ;; when working in code source.
		  (setq-local frame-title-format "%f")
		  ;;; Show trailing whitespaces
		  (setq-local show-trailing-whitespace t)
		  ))))

;; Install web-mode
(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)))

;; Install php-mode
(use-package php-mode
  :ensure t
  :hook (
    	 (php-mode . jie-php-mode)))

;; Install flymake-phpstan package.
;; Provides phpstan reporting to flymake diagnostics.
(use-package flymake-phpstan
  :ensure t
  :config
  (add-hook 'php-ts-mode 'flymake-phpstan-turn-on))

;; Configure PHP Tree Sitter mode
;; Keymap reference - Chapter 51 - Section 3: Customizing Key Bindings
;; Note that "define-keymap" function is deprecated or mentioned as legacy
;; and that it is better to leverage "keymap-set" functions.
(use-package php-ts-mode
  :hook (
	 (php-ts-mode . (lambda ()
			  (setq tab-width 4
				indent-tabs-mode nil
				html-ts-mode-indent-offset 4
				php-ts-mode-js-css-indent-offset 4
				)))
	 (flymake-mode . (lambda()
			   (keymap-set flymake-mode-map "M-n" 'flymake-goto-next-error)
			   (keymap-set flymake-mode-map "M-p" 'flymake-goto-prev-error)))
	 ))

;; Install composer.el package
(use-package composer
  :ensure t)

;; Configure sane indentation
(use-package emacs
  :config
  (setq-default electric-indent-inhibit t)
  (setq tab-width 4
        indent-tabs-mode nil
        indent-line-function 'insert-tab
        )
  :hook (
	 (c-mode . (lambda ()
		     (setq tab-width 4)))
	     ))

;; Configure indentation for Typescript
(use-package typescript-ts-mode
  :config
  (setq typescript-ts-mode-indent-offset 4))

;; Eglot -  LSP
;; Intelephense for PHP
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '(php-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
	       '((typescript-mode) "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
	       '(c-mode . ("clangd")))
  :bind
  (("C-, D" . eglot-find-declaration)
   ("C-, i" . eglot-find-implementation)
   ("C-, x" . eglot-code-actions)
   ("<f7>" . eglot-format-buffer)
   ("<f6>" . eglot-rename)))

;; /!\ Missing "emacs-lsp-booster" RUST program in $PATH.
;; - https://github.com/blahgeek/emacs-lsp-booster
;;
;; Eglot Booster
;; Enhance performance between Emacs and LSP servers.
;; Manually install using package-vc-install with repository URL:
;; - "https://github.com/jdtsmith/eglot-booster"
;;(use-package eglot-booster
;;  :vc (:url "https://github.com/jdtsmith/eglot-booster")
;;  :after eglot
;;  :config (eglot-booster-mode))

;; Xref
(use-package xref
  :bind
  (("C-, ," . xref-go-back)
   ("C-, d" . xref-find-definitions)
   ("C-, r" . xref-find-references)))

;; JSON and JSONC
(use-package json-mode
  :ensure t)

(provide 'jie-emacs-prog)
