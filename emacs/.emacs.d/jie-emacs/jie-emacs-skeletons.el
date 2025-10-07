;;; -*- lexical-binding: t -*-

(define-skeleton jie-skeleton-org-mode-code-block
  "A skeleton to generate an org-mode code block structure."
  "Code language: "
  "#+begin_src " str | "emacs-lisp" " " ("Header: " str)
  \n
  -
  \n
  "#+end_src")

;; PHP - Skeletons
(define-skeleton jl-php-class-skeleton
  "A skeleton to generate a PHP class code structure."
  "PHP Class"
  "<?php"
  \n
  \n
  "namespace " ("Namespace: " str)
  \n
  \n
  (car
   (cdr
    (read-multiple-choice "Insert: " '((?a "abstract class ") (?c "class ") (?i "interface ") (?t "trait "))))
   )
  ("Name: " str)
  \n
  "{"
  \n
  \n
  -
  "}"
  )

(define-skeleton jl-explore-skeleton
  "A skeleton to explore the API."
  nil
  (car(cdr(read-multiple-choice "Insert:" '((?a "abstract class") (?c "class")))))
  -)

(provide 'jie-emacs-skeletons)
