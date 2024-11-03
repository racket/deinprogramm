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
               "wxme-lib"
               "snip-lib"
               "draw-lib"
               "deinprogramm-lib"))

(define build-deps '("at-exp-lib"
                     "htdp-doc"
                     "racket-doc"
                     "racket-index"
                     "scribble-lib"
                     "gui-lib"))

(define pkg-desc "Documentation for DeinProgramm teaching languages")

(define implies '("deinprogramm-signature"))

(define pkg-authors '(sperber))

(define license
  '(Apache-2.0 OR MIT))
