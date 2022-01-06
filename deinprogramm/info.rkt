#lang info
(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "deinprogramm-signature"
               "drracket"
               "drracket-plugin-lib"
               "errortrace-lib"
               "gui-lib"
               "htdp-lib"
               "pconvert-lib"
               "scheme-lib"
               "simple-tree-text-markup-lib"
               "string-constants-lib"
               "trace"
               "wxme-lib"
               "snip-lib"
               "draw-lib"))

(define build-deps '("at-exp-lib"
                     "htdp-doc"
                     "racket-doc"
                     "racket-index"
                     "rackunit-lib"
                     "scribble-lib"))

(define pkg-desc "Teaching languages for _Schreibe Dein Programm!_")

(define implies '("deinprogramm-signature"))

(define pkg-authors '(sperber))

(define license
  '(Apache-2.0 OR MIT))
