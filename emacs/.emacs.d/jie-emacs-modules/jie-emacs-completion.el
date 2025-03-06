;;; Use the Oderless package to provide fuzzy find search style.
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless basic)))

;;; Use the Vertico package to get vertical minibuffer UI.
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-count 4)
  :init (vertico-mode))

(provide 'jie-emacs-completion)
