;; Always make the "compile" command to run "interactively" instead of just displaying program output.
;; Idea coming from MasteringEmacs: https://www.masteringemacs.org/article/compiling-running-scripts-emacs
(defun jl-advice-compile-interactive (original-function &rest args)
  "Advice function for the `compile' command that sets
its COMINT argument to t in order to run interactively."
  (let ((command (car args)))
    (apply original-function (list command t))))

(advice-add #'compile :around #'jl-advice-compile-interactive)

(provide 'jl-emacs-comint)
