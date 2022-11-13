#lang racket/base
(provide define-singleton
         (for-syntax singleton-info?))
 
(require (except-in racket/struct make-constructor-style-printer)
         mzlib/pconvert-prop
         deinprogramm/private/explicit-write
         mzlib/pretty
         racket/promise
         deinprogramm/signature/signature
	 deinprogramm/signature/signature-german
	 (only-in deinprogramm/quickcheck/quickcheck arbitrary-one-of)

         (for-syntax racket
                     stepper/private/syntax-property
                     racket/struct-info
                     deinprogramm/private/syntax-checkers))


(define (make-singleton name)
  (define-values (type-descriptor
                  raw-constructor
                  raw-predicate
                  raw-generic-access
                  raw-generic-mutate)
    (make-struct-type
     name #f 0 0
     #f
     (list
      (cons prop:print-convert-constructor-name name)
      (cons prop:custom-write
            (make-constructor-style-printer
             (lambda (obj)
               (string->symbol (string-append "singleton:" (symbol->string name))))
             (lambda (obj)
               '())))
      (cons prop:print-converter
            (lambda (r _recur)
              (list name '()))))
     (make-inspector)))

  (values (raw-constructor) raw-predicate))

(begin-for-syntax
  (define-struct singleton-info ()
    #:super struct:struct-info)
  (define (skip def)
    (stepper-syntax-property def 'stepper-skip-completely #t)))

(define-syntax define-singleton
  (lambda (x)
    (syntax-case x ()
      ((define-singleton ?signature ?name ?predicate)
       (begin
         (check-for-id! #'?signature
                        "Signatur in Singleton-Definition muss ein Name sein")
         (check-for-id! #'?name
                        "Name in Singleton-Definition muss ein Name sein")
         (check-for-id! #'?predicate
                        "Prädikat in Singleton-Definition muss ein Name sein")
         ;; new name that the stepper will print as the old name
         (let ((val-name (datum->syntax #f (syntax->datum #'?name) #'?name)))
           #`(begin
               #,(stepper-syntax-property
                  (quasisyntax/loc x (define-values (#,val-name ?predicate) (make-singleton '?name)))
                  'stepper-black-box-expr x)
               #,(skip
                  #`(define-syntax ?name
                      (let ()
                        (define-struct info ()
                          #:super struct:singleton-info
                          ;; support `signature'
                          #:property 
                          prop:procedure
                          (lambda (_ stx)
                            (syntax-case stx ()
                              [(self . args)
                               (raise (exn:fail:syntax
                                       "Um ein Singleton gehören keine Klammern"
                                       (current-continuation-marks)
                                       (list stx)))]
                              [else #'(#%expression #,val-name)])))
                        (make-info (lambda ()
                                     (list #f
                                           #f
                                           #'?predicate
                                           '()
                                           '()
                                           #f))))))
               #,(skip
                  #'(define ?signature
                      (let ((sig
                             (make-predicate-signature '?name
                                                       (delay ?predicate)
                                                       #'?predicate)))
                        (set-signature-arbitrary-promise! sig
                                                          (arbitrary-one-of eq? ?name))
                        sig))))))))))
