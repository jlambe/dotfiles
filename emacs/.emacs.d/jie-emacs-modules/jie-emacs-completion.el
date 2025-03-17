;;; Use the Oderless package to provide fuzzy find search style.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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
  (global-corfu-mode)
  :config
  (add-hook 'corfu-mode-hook #'jie-init-corfu-terminal))

;;; Add Capfs capabilites
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package popon
  :ensure t)
;; Only enables the package if on Emacs version < 31.
(use-package corfu-terminal
  :ensure t
  :if (< (string-to-number emacs-version) 31)
  :after '(corfu popon))

(defun jie-init-corfu-terminal ()
  "Enable corfu terminal if Emacs version is less than 31."
  (unless (display-graphic-p)
            (corfu-terminal-mode +1)))

(use-package emacs
  :custom
  ;; Enable indentation+completion using TAB key
  (tab-always-indent 'complete))

(provide 'jie-emacs-completion)
