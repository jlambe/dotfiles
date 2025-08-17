;;; -*- lexical-binding: t -*-
;; Turn on hl-line-mode when programming.
;; Highlight the current line.
(use-package emacs
  :hook
  ((prog-mode . hl-line-mode)))

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
  :hook
(php-mode #'flymake-phpstan-turn-on))

;; Configure PHP Tree Sitter mode
(use-package php-ts-mode
  :hook (
	 (php-ts-mode . (lambda ()
			  (setq tab-width 4
				indent-tabs-mode nil)))))
;; Configure sane indententation
(use-package emacs
  :config
  (setq-default electric-indent-inhibit t)
  (setq tab-width 4
        indent-tabs-mode nil
        indent-line-function 'insert-tab
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
