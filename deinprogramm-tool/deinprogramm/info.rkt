#lang info

(define name "DeinProgramm")

(define tools '("sdp/private/sdp-langs.rkt"))

(define tool-icons '(("logo-small.png" "deinprogramm")))
(define tool-names '("DeinProgramm SDP"))
(define tool-urls '("http://www.deinprogramm.de/"))

(define compile-omit-paths
  '("sdp/record.scm"
    "sdp/private/convert-explicit.scm"))
(define test-omit-paths compile-omit-paths)

(define get-textbook-pls
  '("textbook-pls-spec.rkt" textbook-pls))
