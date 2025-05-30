;;; -*- lexical-binding: t -*-

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


;; Eglot -  LSP
;; Intelephense for PHP
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '(php-mode . ("intelephense" "--stdio")))
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

(provide 'jie-emacs-prog)
