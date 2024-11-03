#lang info
(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "deinprogramm-signature"
               "errortrace-lib"
               "htdp-lib"
               "pconvert-lib"
               "scheme-lib"
               "simple-tree-text-markup-lib"
               "string-constants-lib"
               "trace"
               "gui-lib"
               "wxme-lib"
               "snip-lib"
               "draw-lib"))

(define build-deps '("at-exp-lib"
                     "racket-index"
                     "rackunit-lib"))

(define pkg-desc "Teaching languages for _Schreibe Dein Programm!_")

(define implies '("deinprogramm-signature"))

(define pkg-authors '(sperber))

(define license
  '(Apache-2.0 OR MIT))
