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
  ;;; Allow to scroll up to the beginning or down to the end of the buffer.
  (setq scroll-error-top-bottom t)
  ;;; Preserve screeb position while scrolling
  (setq scroll-preserve-screen-position t)
  ;;; Number of lines of continuity when scrolling by screenfuls.
  (setq next-screen-context-lines 8)

  ;; Disable tool bar
  (tool-bar-mode -1)

  ;; Default font for macos
  (when (string= system-type "darwin")
    (add-to-list 'default-frame-alist
  	       '(font . "Menlo 13")))

  ;; Tree-sitter configuration
  (setq treesit-language-source-alist
	'(
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (php "https://github.com/tree-sitter/tree-sitter-php" "v0.23.12" "php/src")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
	  (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")))
  :mode
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
	 ("\\.php\\'" . php-ts-mode))
  :preface
      ;; Remap major modes to their Treesitter counterpart.
      (dolist (mapping
	       '((css-mode . css-ts-mode)
		 (typescript-mode . typescript-ts-mode)
		 (js-mode . typescript-ts-mode)
		 (js2-mode . typescript-ts-mode)
		 (c-mode . c-ts-mode)
		 (c++-mode . c++-ts-mode)
		 (json-mode . json-ts-mode)
		 (php-mode . php-ts-mode)))
	(add-to-list 'major-mode-remap-alist mapping))
  :bind
  (
   ;; C-A move the point at the beginning of line text
   ("C-S-a" . beginning-of-line-text)

   ;; C-E keybinding to scroll up by line
   ;; C-Y keybinding to scroll down by line
   ("C-S-y" . scroll-down-line)
   ("C-S-e" . scroll-up-line)

   ;; C-6 keybinding to switch between two recent buffers
   ("C-6" . mode-line-other-buffer)

   ;; C-<right> move forward to word
   ;; Replace default word right keymap
   ("C-<right>" . forward-to-word)
   )
  :hook
  (
   ;; Use relative line numbers while working on code files.
   (prog-mode . (lambda ()
		  (setq display-line-numbers 'relative)))))

(provide 'jie-emacs-editor)
