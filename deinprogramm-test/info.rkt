#lang info
(define collection 'multi)

(define deps '("base"
               "deinprogramm-lib"
               "deinprogramm-signature"))

(define build-deps '("at-exp-lib"
                     "racket-index"
                     "rackunit-lib"
                     "scheme-lib"
                     "htdp-lib"))

(define update-implies '("deinprogramm-lib"))

(define pkg-desc "Tests for Deinprogramm teaching languages")

(define implies '("deinprogramm-signature"))

(define pkg-authors '(sperber))

(define license
  '(Apache-2.0 OR MIT))
