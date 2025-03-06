;;; Use relative line numbers while working on code files.
(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-line-numbers 'relative)))

(provide 'jie-emacs-prog)
