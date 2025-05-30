;;; -*- lexical-binding: t -*-

(use-package emacs
  :init
  ;; Disable menu bar if on Linux GUI or terminal(need to update code here).
  (menu-bar-mode -1)

  ;; Set tab-bar-show mode to hide if there is only one tab left
  (setq tab-bar-show 1)

  ;; Navigation/View
  ;;; Set scroll-margin to 8 lines to automatically scroll before reaching the top or bottom of a window.
  (setq scroll-margin 8)

  ;; Disable tool bar
  (tool-bar-mode -1)

  ;; Default font for macos
  (when (string= system-type "darwin")
    (add-to-list 'default-frame-alist
  	       '(font . "Menlo 16")))

  ;; Tree-sitter configuration
  (setq treesit-language-source-alist '(
					  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))
  :bind
  (
   ;; C-A move the point at the beginning of line text
   ("C-S-a" . beginning-of-line-text)

   ;; C-E keybinding to scroll up by line
   ;; C-Y keybinding to scroll down by line-
   ("C-S-y" . scroll-down-line)
   ("C-S-e" . scroll-up-line))

  :hook
  (
   ;; Use relative line numbers while working on code files.
   (prog-mode . (lambda ()
		  (setq display-line-numbers 'relative)))))

(provide 'jie-emacs-editor)
