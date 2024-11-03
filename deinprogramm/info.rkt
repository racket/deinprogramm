#lang info
(define collection 'multi)

(define deps '("deinprogramm-lib"
               "deinprogramm-doc"
               "deinprogramm-tool"))
(define implies '("deinprogramm-lib"
                  "deinprogramm-doc"
                  "deinprogramm-tool"))

(define pkg-desc "Teaching languages for _Schreibe Dein Programm!_")

(define pkg-authors '(sperber))

(define license
  '(Apache-2.0 OR MIT))
