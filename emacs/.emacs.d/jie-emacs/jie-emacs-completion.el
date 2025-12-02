;;; -*- lexical-binding: t -*-

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
  (vertico-count 8)
  :init (vertico-mode))

;;; Use Marginalia package to add annotations on the minibuffer entries.
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;;; Use the Corfu package for buffer completion UI
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode)
  ;; c-mode completion fix
  ;; Use TAB within c-mode to perform completion
  ;; For what I understand, it maps "completion-at-point" on the current
  ;; key TAB (replace default function).
  ;; Disable this code snippet if c-mode does not behave normally.
  (when (equal tab-always-indent 'complete)
    (define-key c-mode-base-map [remap c-indent-line-or-region] #'completion-at-point))
  :config
  (add-hook 'corfu-mode-hook #'jie-init-corfu-terminal))

;;; Add Capfs capabilites
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; Use Consult package
(use-package consult
  :ensure t
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (
   ;; C-x bindings in `ctl-x-map'
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)
   ;; M-g bindings in `goto-map'
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ;; M-s bindings in `search-map'
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)))

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
