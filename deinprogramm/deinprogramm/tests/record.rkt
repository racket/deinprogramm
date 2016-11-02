#lang racket/base

(provide all-record-tests)

(require rackunit
	 deinprogramm/define-record-procedures
	 racket/match)

(define-record-procedures pare
  kons pare?
  (kar kdr))

(define-record-procedures chocolate-cookie
  make-chocolate-cookie chocolate-cookie?
  (chocolate-cookie-chocolate chocolate-cookie-cookie))

(define all-record-tests
  (test-suite
   "Tests for DeinProgramm records."

   (test-case
    "basics"
    (define p1 (kons 1 2))
    (define p2 (kons 3 4))

    (check-true (pare? p1))
    (check-true (pare? p2))

    (check-false (pare? 5))
    (check-false (pare? (make-chocolate-cookie 1 2)))

    (check-equal? (kar p1) 1)
    (check-equal? (kdr p1) 2)
    (check-equal? (kar p2) 3)
    (check-equal? (kdr p2) 4))

   (test-case
    "matching"
    (define p (kons 1 2))
    (define c (make-chocolate-cookie 3 4))

    (define t
      (lambda (r)
	(match r
	  ((kons a b) (list 'kons a b))
	  ((make-chocolate-cookie ch ck) (list 'make-chocolate-cookie ch ck)))))

    (check-equal? (t p) '(kons 1 2))
    (check-equal? (t c) '(make-chocolate-cookie 3 4)))))

