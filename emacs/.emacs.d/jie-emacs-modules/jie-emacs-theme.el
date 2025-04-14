;; (require 'ef-themes)
;; (load-theme 'ef-light :no-confirm)
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-oksolar-light t))

(provide 'jie-emacs-theme)
