#lang info

(define name "DeinProgramm")

(define tools '("DMdA/private/DMdA-langs.rkt"))

(define tool-icons '(("logo-small.png" "deinprogramm")))
(define tool-names '("DeinProgramm"))
(define tool-urls '("http://www.deinprogramm.de/"))

(define compile-omit-paths
  '("DMdA/define-record-procedures.scm"
    "DMdA/private/convert-explicit.scm"
    "DMdA/teachpack/line3d.scm"))
(define test-omit-paths compile-omit-paths)

(define get-textbook-pls
  '("textbook-pls-spec.rkt" textbook-pls))
