#lang scheme/base

(require string-constants
	 framework
	 (prefix-in et: errortrace/stacktrace)
	 (prefix-in tr: trace/stacktrace)
	 mzlib/pretty
	 (prefix-in pc: mzlib/pconvert)
	 mzlib/file
	 mzlib/unit
	 mzlib/class
	 mzlib/list
	 racket/match
	 racket/path
         (only-in racket/list add-between last)
         racket/contract
	 mzlib/struct
	 mzlib/compile
	 drscheme/tool
	 mred
	 syntax/moddep
	 mrlib/cache-image-snip
	 compiler/embed
	 wxme/wxme
	 setup/dirs
	 setup/getinfo
	 setup/collects

	 lang/stepper-language-interface
	 lang/debugger-language-interface
	 lang/run-teaching-program
	 lang/private/continuation-mark-key
         deinprogramm/sdp/private/rewrite-error-message
	 
	 lang/private/tp-dialog
         (only-in test-engine/syntax test-execute test)
	 (except-in test-engine/test-engine signature-violation)
	 test-engine/test-markup
	 test-engine/test-display-gui
	 deinprogramm/signature/signature
         
         lang/htdp-langs-interface
	 deinprogramm/sdp/private/runtime)


  (require mzlib/pconvert-prop)

  (require deinprogramm/sdp/private/convert-explicit)

  (require (only-in mrlib/syntax-browser render-syntax/snip))
  
  (provide tool@)
  
  (define ellipses-cutoff 200)
  
  (define o (current-output-port))
  (define (oprintf . args) (apply fprintf o args))
  
  (define generic-proc
    (procedure-rename void '?))

  ;; adapted from collects/drracket/private/main.rkt
  (preferences:set-default 'drracket:deinprogramm:sdp:last-set-teachpacks/multi-lib
                           '() 
                           (lambda (x)
                             (and (list? x)
                                  (andmap (lambda (x)
                                            (and (list? x)
                                                 (pair? x)
                                                 (eq? (car x) 'lib)
                                                 (andmap string? (cdr x))))
                                          x))))


  (define tool@
    (unit
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      
      (define drs-eventspace (current-eventspace))
      
      ;; writing-style : {explicit, datum}
      ;; tracing? : boolean
      ;; teachpacks : (listof require-spec)
      (define-struct (deinprogramm-lang-settings drscheme:language:simple-settings)
        (writing-style tracing? teachpacks))
      (define deinprogramm-lang-settings->vector (make-->vector deinprogramm-lang-settings))
      (define deinprogramm-teachpacks-field-index 
        (+ (procedure-arity drscheme:language:simple-settings) 2))
      
      (define image-string "<image>")
      
      (define deinprogramm-language<%>
        (interface ()
          get-module
          get-language-position
          get-sharing-printing
          get-abbreviate-cons-as-list
          get-allow-sharing?
          get-use-function-output-syntax?
          get-accept-quasiquote?
          get-read-accept-dot))
      
      ;; module-based-language-extension :    (implements drscheme:language:module-based-language<%>) 
      ;;                                   -> (implements drscheme:language:module-based-language<%>)
      ;; changes the default settings and sets a few more paramters during `on-execute'
      (define (module-based-language-extension printing-style writing-style super%)
        (class* super% ()
          
          (inherit get-sharing-printing get-abbreviate-cons-as-list)
          
          (define/override (default-settings)
            (make-deinprogramm-lang-settings 
             #f
             printing-style
             'repeating-decimal
             (get-sharing-printing)
             #t
             'none
	     writing-style
             #f
	     (preferences:get 'drracket:deinprogramm:sdp:last-set-teachpacks/multi-lib)))
          
          (define/override (default-settings? s)
            (and (not (drscheme:language:simple-settings-case-sensitive s))
		 (eq? (drscheme:language:simple-settings-printing-style s)
		      printing-style)
		 (eq? (drscheme:language:simple-settings-fraction-style s)
		      'repeating-decimal)
		 (eqv? (drscheme:language:simple-settings-show-sharing s)
		       (get-sharing-printing))
		 (drscheme:language:simple-settings-insert-newlines s)
		 (eq? (drscheme:language:simple-settings-annotations s)
		      'none)
		 (eq? writing-style (deinprogramm-lang-settings-writing-style s))
                 (not (deinprogramm-lang-settings-tracing? s))
		 (null? (deinprogramm-lang-settings-teachpacks s))))
          
          (define/override (marshall-settings x)
            (list (super marshall-settings x)
		  (deinprogramm-lang-settings-writing-style x)
                  (deinprogramm-lang-settings-tracing? x)
		  (deinprogramm-lang-settings-teachpacks x)))
          
          (define/override (unmarshall-settings x)
            (if (and (list? x)
                     (= (length x) 4)
		     (symbol? (list-ref x 1)) ; ####
                     (boolean? (list-ref x 2))
                     (list-of-require-specs? (list-ref x 3)))
                (let ([drs-settings (super unmarshall-settings (first x))])
                  (make-deinprogramm-lang-settings
                   (drscheme:language:simple-settings-case-sensitive drs-settings)
                   (drscheme:language:simple-settings-printing-style  drs-settings)
                   (drscheme:language:simple-settings-fraction-style  drs-settings)
                   (drscheme:language:simple-settings-show-sharing  drs-settings)
                   (drscheme:language:simple-settings-insert-newlines  drs-settings)
                   (drscheme:language:simple-settings-annotations drs-settings)
                   (cadr x)
                   (caddr x)
		   (cadddr x)))
                (default-settings)))

          (define/private (list-of-require-specs? l)
            (and (list? l)
                 (andmap (lambda (x)
                           (and (list? x)
                                (andmap (lambda (x) (or (string? x) (symbol? x))) x)))
                         l)))
          
          (inherit get-allow-sharing? get-use-function-output-syntax? 
                   get-accept-quasiquote? get-read-accept-dot)
          (define/override (config-panel parent)
            (sharing/not-config-panel (get-allow-sharing?) (get-accept-quasiquote?) parent))
          
          (define/override (on-execute settings run-in-user-thread)
            ;; do this first so we can overide global-port-print-handler
            (super on-execute settings run-in-user-thread)
            
            (let ([drs-namespace (current-namespace)]
                  [scheme-test-module-name
                   ((current-module-name-resolver) '(lib "test-engine/racket-tests.rkt") #f #f #t)]
                  [scheme-signature-module-name
                   ((current-module-name-resolver) '(lib "deinprogramm/signature/signature-german.rkt") #f #f #t)]
                  [tests-on? (preferences:get 'test-engine:enable?)])
              (run-in-user-thread
               (lambda ()
		 (when (getenv "PLTDRHTDPNOCOMPILED") (use-compiled-file-paths '()))
                 (read-accept-quasiquote (get-accept-quasiquote?))
                 (ensure-drscheme-secrets-declared drs-namespace)
                 (namespace-attach-module drs-namespace ''drscheme-secrets)
                 (error-display-handler teaching-languages-error-display-handler)
		 (error-value->string-handler 
                  (lambda (x y) (teaching-languages-error-value->string settings x y)))
		 (current-eval (add-annotation (deinprogramm-lang-settings-tracing? settings) (current-eval)))

                 (error-print-source-location #f)
                 (read-decimal-as-inexact #f)
                 (read-accept-dot (get-read-accept-dot))
                 (namespace-attach-module drs-namespace scheme-test-module-name)
                 (namespace-require scheme-test-module-name)

                 (namespace-attach-module drs-namespace scheme-signature-module-name)
                 (namespace-require scheme-signature-module-name)
		 (initialize-test-object!)
		 ;; record signature violations with the test engine
                 (display-test-results-parameter
                  (lambda (markup)
                    (test-display-results! (drscheme:rep:current-rep)
                                           drs-eventspace
                                           markup)))
                 (test-execute tests-on?)
		 (signature-checking-enabled?
		  (if (preferences:default-set? 'signatures:enable-checking?) ; Signatures tool not present
		      (preferences:get 'signatures:enable-checking?)
		      #t))
                 (configure/settings
                  (sdp-runtime-settings (drscheme:language:simple-settings-printing-style settings)
                                        (deinprogramm-lang-settings-writing-style settings)
                                        (drscheme:language:simple-settings-fraction-style settings)
                                        (drscheme:language:simple-settings-show-sharing settings)
                                        (drscheme:language:simple-settings-insert-newlines settings)
                                        (deinprogramm-lang-settings-tracing? settings)))))))

          (define/private (teaching-languages-error-value->string settings v len)
            (let ([sp (open-output-string)])
              (print v sp)
              (flush-output sp)
              (let ([s (get-output-string sp)])
		(cond
                 [(<= (string-length s) len) s]
                 [else (string-append (substring s 0 (- len 3)) "...")]))))
	
          (define/override (render-value/format value settings port width)
            (sdp-render-value/format value port width))
          
          (define/override (render-value value settings port)
            (sdp-render-value/format value port 'infinity))
          
          (super-new)))

      ;; this inspector should be powerful enough to see
      ;; any structure defined in the user's namespace
      (define drscheme-inspector (current-inspector))

      ;; FIXME: brittle, mimics drscheme-secrets
      ;; as declared in lang/htdp-langs.rkt.
      ;; Is it even needed for DeinProgramm langs?
      ;; Only used by htdp/hangman teachpack.
      (define (ensure-drscheme-secrets-declared drs-namespace)
        (parameterize ((current-namespace drs-namespace))
          (define (declare)
            (eval `(,#'module drscheme-secrets mzscheme
                     (provide drscheme-inspector)
                     (define drscheme-inspector ,drscheme-inspector)))
            (namespace-require ''drscheme-secrets))
          (with-handlers ([exn:fail? (lambda (e) (declare))])
            ;; May have been declared by lang/htdp-langs tool, if loaded
            (dynamic-require ''drscheme-secrets 'drscheme-inspector))
          (void)))

      ;; sharing/not-config-panel :  boolean boolean parent -> (case-> (-> settings) (settings -> void))
      ;; constructs the config-panel for a language without a sharing option.
      (define (sharing/not-config-panel allow-sharing-config? accept-quasiquote? _parent)
        (let* ([parent (make-object vertical-panel% _parent)]
               
               [input-panel (instantiate group-box-panel% ()
                              (parent parent)
                              (label (string-constant input-syntax))
                              (alignment '(left center)))]
               
               [output-panel (instantiate group-box-panel% ()
                               (parent parent)
                               (label (string-constant output-syntax))
                               (alignment '(left center)))]
               
               [tp-group-box (instantiate group-box-panel% ()
                               (label (string-constant teachpacks))
                               (parent parent)
                               (alignment '(center top)))]
               [tp-panel (new vertical-panel%
                              [parent tp-group-box]
                              [alignment '(center center)]
                              [stretchable-width #f]
                              [stretchable-height #f])]

               [case-sensitive (make-object check-box%
                                 (string-constant case-sensitive-label)
                                 input-panel
                                 void)]
               [output-style (make-object radio-box%
                               (string-constant output-style-label)
                               (if accept-quasiquote?
                                   (list (string-constant constructor-printing-style)
                                         (string-constant quasiquote-printing-style)
                                         (string-constant write-printing-style))
                                   (list (string-constant constructor-printing-style)
                                         (string-constant write-printing-style)))
                               output-panel
                               void)]
               [writing-style (make-object radio-box%
					   "write-Ausgabe"
					   (list "explizit"
						 "Datum")
					   output-panel
					   void)]
               [fraction-style
                (make-object radio-box% (string-constant fraction-style)
                  (list (string-constant use-mixed-fractions)
                        (string-constant use-repeating-decimals))
                  output-panel
                  void)]
               [show-sharing #f]
               [insert-newlines (make-object check-box%
					     (string-constant use-pretty-printer-label)
					     output-panel
					     void)]
	       
               [tracing (new check-box%
                             (parent output-panel)
                             (label (string-constant tracing-enable-tracing))
                             (callback void))]
	       
	       [tps '()])
          
          (when allow-sharing-config?
            (set! show-sharing
                  (instantiate check-box% ()
                    (parent output-panel)
                    (label (string-constant sharing-printing-label))
                    (callback void))))
          
          ;; set the characteristics of the GUI
          (send _parent set-alignment 'center 'center)
          (send parent stretchable-height #f)
          (send parent stretchable-width #f)
          (send parent set-alignment 'center 'center)
          
          (case-lambda
            [()
             (make-deinprogramm-lang-settings
              (send case-sensitive get-value)
              (if accept-quasiquote?
                  (case (send output-style get-selection)
                    [(0) 'constructor]
                    [(1) 'quasiquote]
                    [(2) 'write])
                  (case (send output-style get-selection)
                    [(0) 'constructor]
                    [(1) 'write]))
              (case (send fraction-style get-selection)
                [(0) 'mixed-fraction]
                [(1) 'repeating-decimal])
              (and allow-sharing-config? (send show-sharing get-value))
              (send insert-newlines get-value)
              'none
              (case (send writing-style get-selection)
                [(0) 'explicit]
                [(1) 'datum])
              (send tracing get-value)
              tps)]
	    [(settings)
             (send case-sensitive set-value (drscheme:language:simple-settings-case-sensitive settings))
             (send output-style set-selection
		   (if accept-quasiquote?
		       (case (drscheme:language:simple-settings-printing-style settings)
			 [(constructor) 0]
			 [(quasiquote) 1]
			 [(write) 2]
			 [(print) 2])
		       (case (drscheme:language:simple-settings-printing-style settings)
			 [(constructor) 0]
			 [(quasiquote) 0]
			 [(write) 1]
			 [(print) 1])))
             (send writing-style set-selection
                   (case (deinprogramm-lang-settings-writing-style settings)
                     [(explicit) 0]
                     [(datum) 1]))
             (send fraction-style set-selection
                   (case (drscheme:language:simple-settings-fraction-style settings)
                     [(mixed-fraction) 0]
                     [(repeating-decimal) 1]))
             (when allow-sharing-config?
               (send show-sharing set-value (drscheme:language:simple-settings-show-sharing settings)))
             (send insert-newlines set-value 
                   (drscheme:language:simple-settings-insert-newlines settings))
             (set! tps (deinprogramm-lang-settings-teachpacks settings))
             (send tp-panel change-children (lambda (l) '()))
             (if (null? tps)
                 (new message%
                      [parent tp-panel]
                      [label (string-constant teachpacks-none)])
                 (for-each
                  (lambda (tp) (new message% 
				    [parent tp-panel]
				    [label (format "~s" tp)]))
                  tps))
             (send tracing set-value (deinprogramm-lang-settings-tracing? settings))
             (void)])))
      
      (define simple-deinprogramm-language%
        ;; htdp-language<%> interface is here to make
        ;; the "Racket | Disable Tests" menu item
        ;; work for these languages
        (class* drscheme:language:simple-module-based-language% (deinprogramm-language<%> htdp-language<%>)
          (init-field sharing-printing
                      abbreviate-cons-as-list
                      allow-sharing?
                      manual
		      reader-module
                      (use-function-output-syntax? #f)
                      (accept-quasiquote? #t)
                      (read-accept-dot #t) ;; #### should only be this in advanced mode
                      (style-delta #f))
          (define/public (get-sharing-printing) sharing-printing)
          (define/public (get-abbreviate-cons-as-list) abbreviate-cons-as-list)
          (define/public (get-allow-sharing?) allow-sharing?)
          (define/public (get-manual) manual)
          (define/public (get-use-function-output-syntax?) use-function-output-syntax?)
          (define/public (get-accept-quasiquote?) accept-quasiquote?)
          (define/public (get-read-accept-dot) read-accept-dot)
          ;(define/override (get-one-line-summary) one-line-summary)
          (define/public (get-deinprogramm-style-delta) style-delta)
          
          (super-instantiate ()
            (language-url "http://www.deinprogramm.de/sdp/"))))
      
      (define (language-extension %)
        (class %
          (inherit get-manual)

          (define/override (extra-repl-information settings port) 
            (define welcome (drscheme:rep:get-welcome-delta))
            (define (go str sd)
              (let* ([s (make-object string-snip% str)]
                     [sl (editor:get-standard-style-list)]
                     [std (send sl find-named-style "Standard")]
                     [style (send sl find-or-create-style std sd)])
                (send s set-style style)
                (write-special s port)))
            
            (define tps (deinprogramm-lang-settings-teachpacks settings))
            
            (unless (null? tps)
              (go "Teachpack" welcome)
              (cond
                [(= 1 (length tps))
                 (go ": " welcome)
                 (go (tp-require->str (car tps)) (drscheme:rep:get-dark-green-delta))]
                [(= 2 (length tps))
                 (go "s: " welcome)
                 (go (tp-require->str (car tps)) (drscheme:rep:get-dark-green-delta))
                 (go " und " welcome)
                 (go (tp-require->str (cadr tps)) (drscheme:rep:get-dark-green-delta))]
                [else
                 (go "s: " welcome)
                 (go (tp-require->str (car tps)) (drscheme:rep:get-dark-green-delta))
                 (let loop ([these-tps (cdr tps)])
                   (cond
                     [(null? (cdr these-tps))
                      (go " und " welcome)
                      (go (tp-require->str (car these-tps)) (drscheme:rep:get-dark-green-delta))]
                     [else
                      (go ", " welcome)
                      (go (tp-require->str (car these-tps)) (drscheme:rep:get-dark-green-delta))
                      (loop (cdr these-tps))]))])
              (go "." welcome)
              (newline port)))

	  (define/private (tp-require->str tp)
            (match tp
              [`(lib ,x) 
               (define m (regexp-match #rx"teachpack/deinprogramm/sdp/(.*)$" x))
               (if m
                   (list-ref m 1)
                   (format "~s" tp))]
              [_ (format "~s" tp)]))

          (inherit get-module get-transformer-module get-init-code
                   use-namespace-require/copy?)
          (define/override (create-executable setting parent program-filename)
            (let ([dist-filename
		   (drscheme:language:put-executable
		    parent program-filename
		    'distribution 
		    #t
		    (string-constant save-a-mred-distribution))])
              (when dist-filename
                (drscheme:language:create-distribution-for-executable 
                 dist-filename
                 #t
                 (lambda (exe-name)
                   (create-embedding-executable 
                    exe-name
                    #:modules `((#f ,program-filename))
                    #:cmdline `("-l" 
                                "scheme/base"
                                "-e"
                                ,(format "~s" `(#%require ',(filename->require-symbol program-filename))))
                    #:src-filter
                    (lambda (path) (cannot-compile? path))
                    #:get-extra-imports
                    (lambda (path cm)
                      (call-with-input-file path
                        (lambda (port)
                          (cond
                            [(is-wxme-stream? port)
                             (let-values ([(snip-class-names data-class-names)
                                           (extract-used-classes port)])
                               (list*
                                '(lib "wxme/read.ss")
                                '(lib "mred/mred.ss")
                                reader-module
                                (filter
                                 values
                                 (map (lambda (x) (string->lib-path x #t))
                                      (append
                                       snip-class-names
                                       data-class-names)))))]
                            [else
                             '()]))))
                    #:mred? #t))))))

          (define/private (filename->require-symbol fn)
            (let-values ([(base name dir) (split-path fn)])
              (string->symbol
               (path->string
                (path-replace-suffix name #"")))))
          
          (define/private (symbol-append x y)
            (string->symbol
             (string-append
              (symbol->string x)
              (symbol->string y))))
          
          (inherit get-deinprogramm-style-delta)
          (define/override (get-style-delta)
            (get-deinprogramm-style-delta))
          
          (inherit get-reader)
          
          (define/override (front-end/complete-program port settings)
	    (expand-teaching-program port  
                                     (get-reader)
                                     (get-module)
                                     (deinprogramm-lang-settings-teachpacks settings)
				     '#%deinprogramm))

          (define/override (front-end/interaction port settings)
            (let ([reader (get-reader)] ;; DeinProgramm addition:
                  ;; needed for test boxes; see
                  ;; the code in
                  ;; collects/drracket/private/language.rkt
                  [done? #f]
                  [test-object (test-object-copy (current-test-object))])
              (λ ()
                (cond
                  [done? eof]
                  [else
                   (let ([ans (reader (object-name port) port)])
                     (cond
                       [(eof-object? ans)
                        (if (test-object=? test-object (current-test-object))
                            eof
                            (begin
                              ; only retest if something has changed
                              (set! done? #t)
                              #`(test)))]
                       [else ans]))]))))

          (define/augment (capability-value key)
            (case key
              [(drscheme:teachpack-menu-items) deinprogramm-teachpack-callbacks]
              [(drscheme:special:insert-lambda) #f]
              [else (inner (drscheme:language:get-capability-default key) 
                           capability-value
                           key)]))

          (define deinprogramm-teachpack-callbacks
            (drscheme:unit:make-teachpack-callbacks
             (lambda (settings) 
               (map (lambda (x) (tp-require->str x)) (deinprogramm-lang-settings-teachpacks settings)))
             (lambda (settings parent) 
               (define old-tps (deinprogramm-lang-settings-teachpacks settings))
	       (define tp-dirs (list "deinprogramm/sdp"))
	       (define labels (list (string-constant teachpack-pre-installed)))
	       (define tp-syms '(deinprogramm-sdp-teachpacks))
               (define-values (tp-to-remove tp-to-add) (get-teachpack-from-user parent tp-dirs labels tp-syms old-tps))
               (define new-tps (let ([removed (if tp-to-remove
                                                  (remove tp-to-remove old-tps)
                                                  old-tps)])
                                 (if (or (not tp-to-add) (member tp-to-add old-tps))
                                     removed
                                     (append removed (list tp-to-add)))))

	       (preferences:set 'drracket:deinprogramm:sdp:last-set-teachpacks/multi-lib new-tps)
	       (make-deinprogramm-lang-settings
		(drscheme:language:simple-settings-case-sensitive settings)
		(drscheme:language:simple-settings-printing-style settings)
		(drscheme:language:simple-settings-fraction-style settings)
		(drscheme:language:simple-settings-show-sharing settings)
		(drscheme:language:simple-settings-insert-newlines settings)
		(drscheme:language:simple-settings-annotations settings)
		(deinprogramm-lang-settings-writing-style settings)
		(deinprogramm-lang-settings-tracing? settings)
		new-tps))
             (lambda (settings name) 
               (let ([new-tps (filter (lambda (x) (not (equal? (tp-require->str x) name)))
                                      (deinprogramm-lang-settings-teachpacks settings))])
                 (preferences:set 'drracket:deinprogramm:sdp:last-set-teachpacks/multi-lib new-tps)
                 (make-deinprogramm-lang-settings
                  (drscheme:language:simple-settings-case-sensitive settings)
                  (drscheme:language:simple-settings-printing-style settings)
                  (drscheme:language:simple-settings-fraction-style settings)
                  (drscheme:language:simple-settings-show-sharing settings)
                  (drscheme:language:simple-settings-insert-newlines settings)
                  (drscheme:language:simple-settings-annotations settings)
		  (deinprogramm-lang-settings-writing-style settings)
                  (deinprogramm-lang-settings-tracing? settings)
                  new-tps)))
             (lambda (settings) 
               (preferences:set 'drracket:deinprogramm:sdp:last-set-teachpacks/multi-lib '())
               (make-deinprogramm-lang-settings
                (drscheme:language:simple-settings-case-sensitive settings)
                (drscheme:language:simple-settings-printing-style settings)
                (drscheme:language:simple-settings-fraction-style settings)
                (drscheme:language:simple-settings-show-sharing settings)
                (drscheme:language:simple-settings-insert-newlines settings)
                (drscheme:language:simple-settings-annotations settings)
		(deinprogramm-lang-settings-writing-style settings)
                (deinprogramm-lang-settings-tracing? settings)
                '()))))

          (inherit-field reader-module)
          (define/override (get-reader-module) reader-module)
          (define/override (get-metadata modname settings)
            (define parsed-tps 
              (marshall-teachpack-settings
               (deinprogramm-lang-settings-teachpacks settings)))
            (string-append
             ";; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten\n"
             ";; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.\n"
             (format "#reader~s~s\n"
                     reader-module
                     `((modname ,modname)
                       (read-case-sensitive ,(drscheme:language:simple-settings-case-sensitive settings))
                       (teachpacks ,parsed-tps)
                       (deinprogramm-settings 
                        ,(for/vector ([e (in-vector (deinprogramm-lang-settings->vector settings))]
                                      [i (in-naturals)])
                           (cond
                             [(= i deinprogramm-teachpacks-field-index) parsed-tps]
                             [else e])))))))
          
          (inherit default-settings)
          (define/override (metadata->settings metadata)
            (let* ([table (metadata->table metadata)] ;; extract the table
                   [ssv (assoc 'deinprogramm-settings table)])
              (if ssv
                  (let ([settings-list (vector->list (cadr ssv))])
                    (if (equal? (length settings-list)
                                (procedure-arity make-deinprogramm-lang-settings))
                        (apply make-deinprogramm-lang-settings 
                               (for/list ([i (in-naturals)]
                                          [e (in-list settings-list)])
                                 (cond
                                   [(= i deinprogramm-teachpacks-field-index)
                                    (unmarshall-teachpack-settings e)]
                                   [else e])))
                        (default-settings)))
                  (default-settings))))
          
          ;; these are used for the benefit of v5.3.6 and earlier drracket's
          ;; specifically, those language doesn't work right with teachpack
          ;; paths of the form (lib "a/b/c.rkt"), but they do with ones of the
          ;; form (lib "c.rkt" "a" "b"), so we do that conversion here when
          ;; sending out a file that might go into 5.3.6.
          
          (define/private (unmarshall-teachpack-settings obj)
            (cond
              [(list? obj)
               (for/list ([obj (in-list obj)])
                 (match obj
                   [`(lib ,(? string? s1) ,(? string? s2) ...)
                    `(lib ,(apply string-append (add-between (append s2 (list s1)) "/")))]
                   [else obj]))]
              [else obj]))
          
          (define/private (marshall-teachpack-settings obj)
            (define (has-slashes? s) (regexp-match? #rx"/" s))
            (cond
              [(list? obj) 
               (for/list ([obj (in-list obj)])
                 (match obj
                   [`(lib ,(? (and/c string? has-slashes?) s))
                    (define split (regexp-split #rx"/" s))
                    `(lib ,(last split) ,@(reverse (cdr (reverse split))))]
                   [else obj]))]
              [else obj]))
          
          (define/private (metadata->table metadata)
            (let ([p (open-input-string metadata)])
              (regexp-match #rx"\n#reader" p) ;; skip to reader
              (read p) ;; skip module
              (read p)))
          
          (define/override (get-metadata-lines) 3)

          (super-new)))

      ;; cannot-compile? : path -> boolean
      ;; returns #t if the file cannot be compiled, #f otherwise
      (define (cannot-compile? path)
        (call-with-input-file path
          (lambda (port) 
            (let ([ok-to-compile-names 
                   (map (lambda (x) (format "~s" x))
                        '(wxtext
                          (lib "comment-snip.ss" "framework")
                          (lib "xml-snipclass.ss" "xml")
                          (lib "scheme-snipclass.ss" "xml")))])
              (and (is-wxme-stream? port)
                   (let-values ([(snip-class-names data-class-names)
                                 (extract-used-classes port)])
                     (not (and (andmap
                                (lambda (used-name) (member used-name ok-to-compile-names))
                                snip-class-names)
                               (andmap
                                (lambda (used-name) (member used-name ok-to-compile-names))
                                data-class-names)))))))))
      
      (define (stepper-settings-language %)
        (if (implementation? % stepper-language<%>)
            (class* % (stepper-language<%>)
              (init-field stepper:supported)
              (define/override (stepper:supported?) stepper:supported)
              (define/override (stepper:print-boolean-long-form?) #f)
              (super-new))
            (class* % ()
              (init stepper:supported)
              (define/override (stepper:print-boolean-long-form?) #f)
              (super-new))))

      (define (debugger-settings-language %)
        (if (implementation? % debugger-language<%>)
            (class* % (debugger-language<%>)
              (init-field [debugger:supported #f])
              (define/override (debugger:supported?) debugger:supported)
              (super-new))
            (class %
              (init [debugger:supported #f])
              (super-new))))

      ;; filter/hide-ids : syntax[list] -> listof syntax
      (define (filter/hide-ids ids)
        ;; When a `define-values' or `define-syntax' declaration
        ;; is macro-generated, if the defined name also originates
        ;; from a macro, then the name is hidden to anything
        ;; that wasn't generated by the same macro invocation. This
        ;; hiding relies on renaming at the symbol level, and it's
        ;; exposed by the fact that `syntax-e' of the identifier 
        ;; returns a different name than `identifier-binding'.
        (filter
         (lambda (id)
           (let ([ib (identifier-binding id)])
             ;; ib should always be a 4-elem list, but
             ;; check, just in case:
             (or (not (pair? ib)) 
                 (eq? (syntax-e id)
                      (cadr ib)))))
         (syntax->list ids)))
                 
      
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                 ;                             
      ;    ;;;   ; ;  ; ;   ;;;    ; ; ;;;;  ; ;  ;;;     ;;;    ;;;  
      ;   ;   ;  ;;   ;;   ;   ;   ;;   ;    ;;  ;   ;   ;   ;  ;   ; 
      ;  ;    ;  ;    ;   ;     ;  ;    ;    ;       ;  ;      ;    ; 
      ;  ;;;;;;  ;    ;   ;     ;  ;    ;    ;    ;;;;  ;      ;;;;;; 
      ;  ;       ;    ;   ;     ;  ;    ;    ;   ;   ;  ;      ;      
      ;   ;      ;    ;    ;   ;   ;    ;    ;   ;   ;   ;   ;  ;     
      ;    ;;;;  ;    ;     ;;;    ;     ;;  ;    ;;;;;   ;;;    ;;;; 
      ;                                                               
      ;                                                               
      ;                                                               
	      
      (define mf-note
        (let ([bitmap
               (make-object bitmap%
                 (collection-file-path "mf.gif" "icons"))])
          (and (send bitmap ok?)
               (make-object image-snip% bitmap))))

      (define (deinprogramm-path? path)
	(let ((rel (path->collects-relative path)))
	  (and (pair? rel)
	       (eq? 'collects (car rel))
	       (equal? #"deinprogramm" (cadr rel)))))
      
      ;; teaching-languages-error-display-handler : 
      ;;    (string (union TST exn) -> void) -> string exn -> void
      ;; adds in the bug icon, if there are contexts to display
      (define (teaching-languages-error-display-handler msg exn)

          (if (exn? exn)
              (display (get-rewriten-error-message exn) (current-error-port))
              (eprintf "uncaught exception: ~e" exn))
          (eprintf "\n")

          ;; need to flush here so that error annotations inserted in next line
          ;; don't get erased if this output were to happen after the insertion
          (flush-output (current-error-port))
          
          (let ([rep (drscheme:rep:current-rep)])
            (when (and (is-a? rep drscheme:rep:text<%>)
                       (eq? (send rep get-err-port) (current-error-port)))
              (let ([to-highlight 
                     (cond
                       [(exn:srclocs? exn) 
                        ((exn:srclocs-accessor exn) exn)]
                       [(exn? exn) 
                        (let ([cms (continuation-mark-set->list (exn-continuation-marks exn) teaching-languages-continuation-mark-key)])
			  (cond
			   ((not cms) '())
			   ((findf (lambda (mark)
				     (and mark
					  (or (and (path? (car mark))
						   ;; exclude paths that result from macro expansion,
						   ;; specifically define-record
						   ;; see racket/drracket#157
						   (not (deinprogramm-path? (car mark))))
					      (symbol? (car mark)))))
				   cms)
			    => (lambda (mark)
				 (apply (lambda (source line col pos span)
					  (list (make-srcloc source line col pos span)))
					mark)))
			   (else '())))]
		       [else '()])])

                (parameterize ([current-eventspace drs-eventspace])
                  (queue-callback
                   (lambda ()
                     ;; need to make sure that the user's eventspace is still the same
                     ;; and still running here?
                     (send rep highlight-errors to-highlight #f))))))))
      
      ;; with-mark : syntax syntax exact-nonnegative-integer -> syntax
      ;; a member of stacktrace-imports^
      ;; guarantees that the continuation marks associated with
      ;; teaching-languages-continuation-mark-key are members of the debug-source type
      (define (with-mark source-stx expr phase)
        (let ([source (syntax-source source-stx)]
              [line (syntax-line source-stx)]
              [col (syntax-column source-stx)]
              [start-position (syntax-position source-stx)]
              [span (syntax-span source-stx)])
          (if (and (or (symbol? source) (path? source))
                   (number? start-position)
                   (number? span))
              (with-syntax ([expr expr]
                            [mark (list source line col start-position span)]
                            [teaching-languages-continuation-mark-key 
                             teaching-languages-continuation-mark-key]
                            [wcm (syntax-shift-phase-level #'with-continuation-mark 
                                                           (- phase base-phase))]
                            [quot (syntax-shift-phase-level #'quote (- phase base-phase))])
                #`(wcm (quot teaching-languages-continuation-mark-key)
                    (quot mark)
                    expr))
              expr)))

      (define base-phase
        (variable-reference->module-base-phase (#%variable-reference)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  profiling infrastructure. Not used.
      ;;
      
      (define profile-key (gensym))
      (define (profiling-enabled) #f)
      (define (initialize-profile-point . x) (void))
      (define (register-profile-start . x) #f)
      (define (register-profile-done . x) (void))
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  test coverage
      ;;
      
      ;; WARNING: much code copied from "collects/lang/htdp-langs.rkt"
      
      (define test-coverage-enabled (make-parameter #t))
      (define current-test-coverage-info (make-thread-cell #f))
      
      (define (initialize-test-coverage-point expr)
        (unless (thread-cell-ref current-test-coverage-info)
          (let ([ht (make-hasheq)])
            (thread-cell-set! current-test-coverage-info ht)
            (let ([rep (drscheme:rep:current-rep)])
              (when rep
                (parameterize ([current-eventspace drs-eventspace])
                  (queue-callback
                   (lambda ()
                     (let ([on-sd (make-object style-delta%)]
                           [off-sd (make-object style-delta%)])
                       (cond
                         [(preferences:get 'framework:white-on-black?)
                          (send on-sd set-delta-foreground "white")
                          (send off-sd set-delta-background "lightblue")
                          (send off-sd set-delta-foreground "black")]
                         [else
                          (send on-sd set-delta-foreground "black")
                          (send off-sd set-delta-background "lightblue")
                          (send off-sd set-delta-foreground "black")])
                       (send rep set-test-coverage-info ht on-sd off-sd #f)))))))))
        (let ([ht (thread-cell-ref current-test-coverage-info)])
          (when ht
            (hash-set! ht expr #;(box #f) (mcons #f #f)))))
      
      (define (test-covered expr)
        (let* ([ht (or (thread-cell-ref current-test-coverage-info)
                       (error 'sdp-langs
                              "internal-error: no test-coverage table"))]
               [v (hash-ref ht expr
                    (lambda ()
                      (error 'sdp-langs
                             "internal-error: expression not found: ~.s"
                             expr)))])
          #; (lambda () (set-box! v #t))
          (with-syntax ([v v]) #'(#%plain-app set-mcar! v #t))))
      
      (define-values/invoke-unit et:stacktrace@
        (import et:stacktrace-imports^) (export (prefix et: et:stacktrace^)))

      ;; add-annotation : boolean (sexp -> value) -> sexp -> value
      ;; adds debugging and test coverage information to `sexp' and calls `oe'
      (define (add-annotation tracing? oe)
        (let ([teaching-language-eval-handler
               (lambda (exp)
                 (let* ([is-compiled? (compiled-expression? (if (syntax? exp) (syntax-e exp) exp))]
                        [annotated
                         (if is-compiled?
                             exp
                             (let* ([et-annotated (et:annotate-top (expand exp) 
                                                                   (namespace-base-phase))]
                                    [tr-annotated
                                     (if tracing?
                                         (drscheme:tracing:annotate (expand et-annotated))
                                         et-annotated)])
                               tr-annotated))])
                   (oe annotated)))])
          teaching-language-eval-handler))
      

      
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                            ;                   ;   ;                                        ;                  
;                                                ;   ;                                        ;                  
;                   ;            ;               ;   ;       ;                           ;    ;                  
;   ; ;;    ;   ;  ;;;;      ;  ;;;;      ;;;    ;   ;      ;;;;   ;;;     ;; ;    ;;;  ;;;;  ; ;;     ;;;   ; ; 
;   ;;  ;   ;   ;   ;        ;   ;       ;   ;   ;   ;       ;    ;   ;   ;  ;;   ;   ;  ;    ;;  ;   ;   ;  ;;  
;   ;    ;  ;   ;   ;        ;   ;           ;   ;   ;       ;   ;     ; ;    ;  ;    ;  ;    ;   ;  ;    ;  ;   
;   ;    ;  ;   ;   ;        ;   ;        ;;;;   ;   ;       ;   ;     ; ;    ;  ;;;;;;  ;    ;   ;  ;;;;;;  ;   
;   ;    ;  ;   ;   ;        ;   ;       ;   ;   ;   ;       ;   ;     ; ;    ;  ;       ;    ;   ;  ;       ;   
;   ;;  ;   ;  ;;   ;        ;   ;       ;   ;   ;   ;       ;    ;   ;   ;  ;;   ;      ;    ;   ;   ;      ;   
;   ; ;;     ;; ;    ;;      ;    ;;      ;;;;;  ;   ;        ;;   ;;;     ;; ;    ;;;;   ;;  ;   ;    ;;;;  ;   
;   ;                                                                         ;                                  
;   ;                                                                    ;    ;                                  
;   ;                                                                     ;;;;                                   
      
      
      ;; add-deinprogramm-language : (instanceof deinprogramm-language<%>) -> void
      (define (add-deinprogramm-language o)
        (drscheme:language-configuration:add-language
         o
         #:allow-executable-creation? #t))
      
      (define (phase1) (void))

      ;; phase2 : -> void
      (define (phase2)
        (define (make-deinprogramm-language% printing-style writing-style)
	  (debugger-settings-language
	   (stepper-settings-language
	    ((drscheme:language:get-default-mixin)
	     (language-extension
	      (drscheme:language:module-based-language->language-mixin
	       (module-based-language-extension
		printing-style writing-style
		(drscheme:language:simple-module-based-language->module-based-language-mixin
		 simple-deinprogramm-language%))))))))
	  
        (add-deinprogramm-language
         (instantiate (make-deinprogramm-language% 'write 'explicit) ()
	   (module '(lib "deinprogramm/sdp/beginner.rkt"))
           (manual #"sdp-beginner")
	   (language-position (list (string-constant teaching-languages)
				    "DeinProgramm" "Schreibe Dein Programm! - Anfänger"))
	   (language-id "sdp:beginner")
           (language-numbers '(-500 -300 3
				    ))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
	   (reader-module '(lib "beginner-reader.rkt" "deinprogramm" "sdp"))
	   (stepper:supported #t)))
	
	(add-deinprogramm-language
         (instantiate (make-deinprogramm-language% 'write 'explicit) ()
	   (module '(lib "deinprogramm/sdp/vanilla.rkt"))
           (manual #"sdp-vanilla")
	   (language-position (list (string-constant teaching-languages)
				    "DeinProgramm" "Schreibe Dein Programm!"))
	   (language-id "sdp:vanilla")
           (language-numbers '(-500 -300 4))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
	   (reader-module '(lib "vanilla-reader.rkt" "deinprogramm" "sdp"))
	   (stepper:supported #t)))

        (add-deinprogramm-language
         (instantiate (make-deinprogramm-language% 'write 'datum) ()
	   (module '(lib "deinprogramm/sdp/advanced.rkt"))
           (manual #"sdp-advanced")
	   (language-position (list (string-constant teaching-languages)
				    "DeinProgramm" "Schreibe Dein Programm! - fortgeschritten"))
	   (language-id "sdp:advanced")
           (language-numbers '(-500 -300 6))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
	   (reader-module '(lib "advanced-reader.rkt" "deinprogramm" "sdp"))
	   (stepper:supported #f)
	   (debugger:supported #t))))))
