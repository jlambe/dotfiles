;;; jl-emacs-writing.el --- Writing configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 by Julien Lambé

;;; Commentary:

;; This file is part of my personal GNU Emacs configuration.

;;; Code:

(defvar jl-org-directory "~/Notes"
  "Default `org-mode' directory.")

;; Org-mode
(use-package org
  :init
  ;; Setup default org-directory.
  (setq org-directory jl-org-directory)

  ;; Configure Org mode capture.
  (setq org-default-notes-files (concat org-directory "/inbox.org"))

  :hook
  ;; Enable auto-fill-mode when working within an .org file.
  ((org-mode . auto-fill-mode)))

;; Markdown
(use-package markdown-mode
  :ensure t)

;; Epub
(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'jl-emacs-writing)

;;; jl-emacs-writing.el ends here
