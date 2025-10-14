;; Always make the "compile" command to run "interactively" instead of
  ;; just displaying program output.
  ;; There is no variable to toggle this automatically so we wrap the compile
  ;; command with "defadvice".
  ;; Idea coming from MasteringEmacs: https://www.masteringemacs.org/article/compiling-running-scripts-emacs
  ;; defadvice is deprecated, should be rewritten using "advice-add"
  ;;(defadvice compile (before ad-compile-smart activate)
  ;;  "Advises `compile' so it sets the argument COMINT to t."
  ;;  (ad-set-arg 1 t))

;;  (advice-add 'compile
;;  	    :before (lambda (command)
;;  		      (apply 'compile command t)))

  (provide 'jl-emacs-comint)
