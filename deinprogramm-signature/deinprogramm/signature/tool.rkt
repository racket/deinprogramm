#lang racket/base

(require racket/file racket/class racket/unit racket/contract racket/match
         drracket/tool
         mred mrlib/switchable-button framework
         string-constants
         deinprogramm/sdp/private/stepper-button)
(provide tool@)

(preferences:set-default 'signatures:enable-checking? #t boolean?)

(define tool@
  (unit (import drracket:tool^) (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define (signatures-frame-mixin %)
      (class* % ()
        (inherit get-current-tab)

        (inherit register-capability-menu-item get-language-menu)

        (define/private (signatures-menu-init)
          (let ([language-menu (get-language-menu)]
                [enable-label (string-constant signature-enable-checks)]
                [disable-label (string-constant signature-disable-checks)])
                
            (make-object separator-menu-item% language-menu)
            (register-capability-menu-item 'signatures:signatures-menu language-menu)
            (letrec ([enable-menu-item%
                      (class menu:can-restore-menu-item%
                        (define enabled? #t)
                        (define/public (is-signature-checking-enabled?) enabled?)
                        (define/public (set-signature-checking-enabled?! e) (set! enabled? e))
			(inherit set-label)
                        (define/public (enable-signature-checking)
                          (unless enabled?
                            (set! enabled? #t)
                            (set-label disable-label)
                            (preferences:set 'signatures:enable-checking? '#t)))
                        (define/public (disable-signature-checking)
                          (when enabled?
                            (set! enabled? #f)
                            (set-label enable-label)
                            (preferences:set 'signatures:enable-checking? '#f)))
                        (super-instantiate ()))]
                     [enable? (preferences:get 'signatures:enable-checking?)]
                     [enable-menu-item (make-object enable-menu-item%
                                         (if enable? disable-label enable-label)
                                         language-menu 
                                         (lambda (_1 _2)
                                           (if (send _1 is-signature-checking-enabled?)
                                               (send _1 disable-signature-checking)
                                               (send _1 enable-signature-checking))) #f)])
              
              (send enable-menu-item set-signature-checking-enabled?! enable?)
              (register-capability-menu-item 'signatures:signatures-menu language-menu))))
                
        (unless (drracket:language:capability-registered? 'signatures:signatures-menu)
          (drracket:language:register-capability 'signatures:signatures-menu (flat-contract boolean?) #f))
        (super-instantiate ())
        (signatures-menu-init)
        ))

    (drracket:get/extend:extend-unit-frame signatures-frame-mixin)

    (for ([lang (in-list '(vanilla beginner advanced))])
      (match-define (list button-label button-image callback)
        (sdp-stepper-drracket-button lang))
      (drracket:module-language-tools:add-opt-in-toolbar-button
       (λ (drr-frame parent)
         (new switchable-button%
              [parent parent]
              [label button-label]
              [bitmap button-image]
              [callback (lambda (dont-care) (callback drr-frame))]))
       (string->symbol (format "deinprogramm:stepper:~a" lang))
       #:number 59)) ;; this should be the same as `stepper-button-number` in stepper/stepper-tool
    ))
