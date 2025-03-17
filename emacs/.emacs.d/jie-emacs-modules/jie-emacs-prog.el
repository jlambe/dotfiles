;;; Use relative line numbers while working on code files.
(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-line-numbers 'relative)))

;;; Modes
;;;; Install web-mode
(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ))

;;;; Install php-mode
(defun jie-php-mode ()
  "Personal php-mode init configuration."
  (progn
    (subword-mode 1)
    (setq-local show-trailing-whitespace 1)))

(use-package php-mode
  :ensure t
  ;;:init
  ;;(add-hook 'completion-at-point-functions #'php-complete-complete-function)
  :config
  (add-hook 'php-mode-hook #'jie-php-mode)
  (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil 1))

(provide 'jie-emacs-prog)
