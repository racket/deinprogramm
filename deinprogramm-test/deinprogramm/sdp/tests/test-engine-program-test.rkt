#lang racket
(require tests/test-engine/test-engine-program-test
         test-engine/test-engine)


(check-success #<<--
#lang deinprogramm/sdp
(check-expect 1 1)
--
               )

(check-failure #<<--
#lang deinprogramm/sdp
(check-expect 1 2)
--
               unequal?
               unequal-actual 1
               unequal-expected 2)

(check-success #<<--
#lang deinprogramm/sdp

(define-record foo
  make-foo
  foo?
  (foo-bar number))

(: make-foo (number -> foo))
(: foo? (any -> boolean))
(: foo-bar (foo -> number))
--
               )

(check-success #<<--
#lang deinprogramm/sdp
(define-singleton empty-list empty empty?)

(: empty empty-list)

(: empty? (any -> boolean))
--
               )
