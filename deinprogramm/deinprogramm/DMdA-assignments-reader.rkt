#lang scheme/base
(require deinprogramm/DMdA/private/DMdA-reader)
(provide (rename-out (-read-syntax read-syntax))
         (rename-out (-read read)))
(define -read-syntax (make-read-syntax '(lib "DMdA-assignments.rkt" "deinprogramm")))
(define -read (make-read '(lib "DMdA-assignments.rkt" "deinprogramm")))
