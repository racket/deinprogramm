#lang racket/base

(require (only-in deinprogramm/sdp/private/primitives
                  check-property for-all ==>
                  integer)
         (except-in rackunit check-within)
         test-engine/racket-tests
         test-engine/test-engine)

(require racket/format)

(define (assert-failed-check failed-check reason? . selector+value-list)
  (check-pred failed-check? failed-check)
  (let ((reason (failed-check-reason failed-check)))
    (check-pred reason? reason)
    (let loop ((selector+value-list selector+value-list))
      (if (null? selector+value-list)
          #t
          (let ((selector (car selector+value-list))
                (expected-or-pred (cadr selector+value-list)))
            (if (procedure? expected-or-pred)
                (check-pred expected-or-pred (selector reason))
                (check-equal? (selector reason)
                              expected-or-pred))
            (loop (cddr selector+value-list)))))))

(define (check-success)
  (let* ((test-object (run-tests!))
         (failed-checks (test-object-failed-checks test-object)))
    (check-pred null? failed-checks))
  (initialize-test-object!))

(define (check-failure reason? . selector+value-list)
  (let* ((test-object   (run-tests!))
         (failed-checks (test-object-failed-checks test-object)))
    (check-equal? (length failed-checks) 1)
    (when (null? failed-checks)
      (define names
        (for/fold ([l (object-name reason?)])
                  ([f selector+value-list] [i (in-naturals)] #:when (even? i))
          (~a l ", " (object-name f))))
      (error 'check-failure "expected failed check, none failed (~a)" names))
    (apply assert-failed-check (car failed-checks) reason? selector+value-list))
  (initialize-test-object!))

(check-property
 (for-all ((a integer)
           (b integer))
   (= (+ a b) (+ b a))))
(check-success)

(check-property
 (for-all ((a integer)
           (b integer))
   (==> (> a 0) (> a 0))))
(check-success)

(check-property
 (for-all ((a integer)
           (b integer))
   (= (+ a b) (+ b a 1))))
(check-failure property-fail?)

(check-property
 (for-all ((a integer)
           (b integer))
   (= (/ a b) (/ b a))))
(check-failure property-fail?)

; check for malformed property
(check-property
 (for-all ((minutes)) #t))
(check-failure property-error?)



        
