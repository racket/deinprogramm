#lang info

(define collection 'multi)

(define deps '("base"
               "scribble-lib"
               "deinprogramm-lib"
               "drracket-plugin-lib"
               "scheme-lib"
               "string-constants-lib"
               "gui-lib"
               "compatibility-lib"
               "htdp-lib"
               "pconvert-lib"
               "errortrace-lib"
               "trace"
               "wxme-lib"))
(define build-deps '("at-exp-lib"))

(define implies '("deinprogramm-lib"))

(define pkg-desc "Tool for menu-based DeinProgramm teaching languages")

(define pkg-authors '(sperber))

(define version "1.0")

(define license
  '(Apache-2.0 OR MIT))
