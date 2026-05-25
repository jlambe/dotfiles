;;; jl-emacs-programming.el --- Programming configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

;; Treesitter
(use-package emacs
  :init
  ;; List the tree-sitter language grammars to download and install.
  (setq treesit-language-source-alist
        '(
  	(css . ("https://github.com/tree-sitter/tree-sitter-css"))
  	(dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
  	(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
  	(json . ("https://github.com/tree-sitter/tree-sitter-json"))
  	(php . ("https://github.com/tree-sitter/tree-sitter-php" "v0.24.2" "php/src"))
  	(make . ("https://github.com/alemuller/tree-sitter-make"))
  	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
  	(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
  	(c . ("https://github.com/tree-sitter/tree-sitter-c"))
  	(cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
  	(yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :mode
  ;; Specify file extension patterns and which "tree-sitter" mode to use for each.
  (
   ("\\.tsx\\'" . tsx-ts-mode)
   ("\\.jsx\\'" . tsx-ts-mode)
   ("\\.js\\'" . typescript-ts-mode)
   ("\\.ts\\'" . typescript-ts-mode)
   ("\\.mjs\\'" . typescript-ts-mode)
   ("\\.mts\\'" . typescript-ts-mode)
   ("\\.cjs\\'" . typescript-ts-mode)
   ("\\.json\\'" . json-ts-mode)
   ("\\.Dockerfile\\'" . dockerfile-ts-mode)
   ("\\.php\\'" . php-ts-mode)
   ("\\.yaml\\'" . yaml-ts-mode)
   ("\\.yml\\'" . yaml-ts-mode)
   ("\\.c\\'" . c-ts-mode)
   ("\\.cpp\\'" . cpp-ts-mode)
   ) ;; :mode ends here
  :preface
  ;; Remap major modes to their tree-sitter counterpart.
  (dolist (mapping
	   '((css-mode . css-ts-mode)
	     (typescript-mode . typescript-ts-mode)
	     (js-mode . typescript-ts-mode)
	     (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (json-mode . json-ts-mode)
             (php-mode . php-ts-mode)
	     ))
    (add-to-list 'major-mode-remap-alist mapping))
  ) ;; (use-package emacs) ends here

;; Prog-mode
(use-package emacs
  :custom
  ;; Only highlight on current/active buffer.
  (hl-line-sticky-flag nil)
  :hook
  (
   (prog-mode . hl-line-mode)
   (prog-mode . electric-pair-mode)
   (prog-mode . (lambda ()
		  ;; Display absolute file name of current buffer in the frame title bar.
		  (setq-local frame-title-format "%f")
		  ;; Show trailing whitespaces.
		  (setq-local show-trailing-whitespace t)))
   (prog-mode . (lambda ()
		  ;; Custom font-lock faces.
		  ;; @TODO, @NOTE
		  (font-lock-add-keywords nil
					  '(
					    ("\\<\\(@?TODO\s?\\(([a-zA-Z0-9]+)\\)?\\)" 1 font-lock-warning-face t)
      					    ("\\<\\(@?NOTE\s?\\(([a-zA-Z0-9]+)\\)?\\)" 1 font-lock-string-face t)
					    ))
		  )))
  ) ;; (use-package emacs) ends here

(provide 'jl-emacs-programming)

;;; jl-emacs-programming.el ends here
