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

;;; Use the Corfu package for buffer completion UI
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode))

(use-package emacs
  :custom
  ;; Enable indentation+completion using TAB key
  (tab-always-indent 'complete))

(provide 'jie-emacs-completion)
