#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     (only-in "type.rhm"
                              configure_typing))
         (prefix-in rhombus: rhombus)
         (for-syntax
          (prefix-in rhombus: rhombus/parse))
         (prefix-in rhombus: rhombus/parse)
         rhombus/private/bounce
         "frame.rhm"
         "declare_configure.rhm")

(provide (rename-out
          [shplait-module-begin #%module-begin]
          [shplait-top-interaction #%top-interaction]))
(bounce "main.rhm")

(module reader syntax/module-reader
  #:language 'shplait/private/core
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info (lambda (a b c)
           ((rhombus:get-info #f #f #f #f #f) a (c a b)))
  #:whole-body-readers? #t
  (require shrubbery/parse
           (prefix-in rhombus: (submod rhombus reader))))

(module configure-runtime racket/base
  (require rhombus/runtime-config))

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))

(define-syntax (shplait-module-begin stx)
  (syntax-parse stx
    #:datum-literals (top)
    [(_ (top form-in ...))
     #:with (lazy? accomodating? untyped? fuel form ...) (parse-options #'(form-in ...))
     #:with (ex ...) (let ([forms (syntax->list #'(form ...))])
                       (if (null? forms)
                           null
                           (list
                            #`(group rhombus:export
                                     (block
                                      (group rhombus:all_defined #:scope_like #,(car (syntax-e (car forms)))))))))
     (configure_language (syntax-e #'lazy?) (syntax-e #'accomodating?))
     (configure_typing (syntax-e #'untyped?) (syntax-e #'fuel))
     (find_type_definitions #'(block form ...))
     (define b
       (local-expand #'(rhombus:#%module-begin
                        (top
                         ex ...
                         form ...))
                     'module-begin
                     null))
     (finish_current_frame)
     (syntax-parse b
       [(#%mb e ...)
        #`(#%mb e ...
                (begin-for-syntax
                  (record-configration! (quote lazy?) (quote accomodating?) (quote untyped?) (quote fuel))
                  (rhombus:rhombus-expression #,(syntax-parse (build_register_defn_types)
                                                  [(parsed kw (re g)) #'g])))
                #,@(map (lambda (sm)
                          #`(rhombus:rhombus-top
                             #,(let ([g (syntax-parse sm
                                          #:datum-literals (multi)
                                          [(multi g) #'g]
                                          [_ sm])])
                                 (syntax-parse g
                                   #:datum-literals (group)
                                   [((~and group-tag group) sm name (block-tag body ...))
                                    #`(group-tag sm name
                                                 (block-tag
                                                  (group declare_configure lazy? accomodating? untyped? fuel)
                                                  body ...))]))))
                        (map syntax-local-introduce (get_submodules))))])]))

(define-for-syntax (parse-options forms-stx)
  (let loop ([forms (syntax->list forms-stx)]
             [eager? #f]
             [lazy? #f]
             [accomodating? #f]
             [untyped? #f]
             [fuel 100])
    (define (done) (list* lazy? accomodating? untyped? fuel forms))
    (define (no-tail tail)
      (unless (null? (syntax-e tail))
        (raise-syntax-error #f "extra terms not allowed after option keyword" (car forms))))
    (cond
      [(null? forms) (done)]
      [else
       (syntax-parse (car forms)
         #:datum-literals (group)
         #:literals (declare_configure)
         [(group declare_configure . args) ; generated for submodules
          (append (syntax->list #'args) (cdr forms))]
         [(group (~and kw #:lazy) . tail)
          (when (or eager? lazy? accomodating?) (raise-syntax-error #f "duplicate eagerness option" #'kw))
          (no-tail #'tail)
          (loop (cdr forms) eager? #t accomodating? untyped? fuel)]
         [(group (~and kw #:accomodating) . tail)
          (when (or eager? lazy? accomodating?) (raise-syntax-error #f "duplicate eagerness option" #'kw))
          (no-tail #'tail)
          (loop (cdr forms) eager? lazy? #t untyped? fuel)]
         #;
         [(group (~and kw #:eager) . tail)
          (when (or lazy? accomodating?) (raise-syntax-error #f "duplicate eagerness option" #'kw))
          (no-tail #'tail)
          (loop (cdr forms) #t lazy? accomodating? untyped? fuel)]
         [(group (~and kw #:untyped) . tail)
          (when untyped? (raise-syntax-error #f "duplicate typedness option" #'kw))
          (no-tail #'tail)
          (loop (cdr forms) eager? lazy? accomodating? #t fuel)]
         [(group (~and kw #:fuel) n . tail)
          (when untyped? (raise-syntax-error #f "duplicate typedness option" #'kw))
          (unless (exact-positive-integer? (syntax-e #'n))
            (raise-syntax-error #f "fuel must be a non-negative integer" #'n))
          (no-tail #'tail)
          (loop (cdr forms) eager? lazy? accomodating? #t (syntax-e #'n))]
         [_ (done)])])))

;; This approach to recording a configuration for interactions is not
;; ideal, because it means the most recently run module wins.
(begin-for-syntax
  (define saved-lazy? #f)
  (define saved-accomodating? #f)
  (define saved-untyped? #f)
  (define saved-fuel 100)
  (define (record-configration! lazy? accomodating? untyped? fuel)
    (set! saved-lazy? lazy?)
    (set! saved-accomodating? accomodating?)
    (set! saved-untyped? untyped?)
    (set! saved-fuel saved-fuel)))

(define-syntax (shplait-top-interaction stx)
  (syntax-parse stx
    #:datum-literals (top)
    [(_ . (top form ...))
     (configure_language saved-lazy? saved-accomodating?)
     (configure_typing saved-untyped? saved-fuel)
     (find_type_definitions #'(block form ...))
     ;; trampoline to deal with `begin` sequences and `local-expand`:
     #'(begin
         (step-top-interaction
          #t
          (rhombus:#%top-interaction
           . (top form ...))))]))

(define-syntax (step-top-interaction stx)
  (syntax-parse stx
    [(_ final? e)
     ;; partial expansion to look for `begin`:
     (define pre-b (local-expand #'e 'top-level (list #'begin)))
     (syntax-parse pre-b
       #:literals (begin)
       [(begin e ... e0)
        ;; trampoline, so earlier definitions ni `begin` are
        ;; available to local-expansion of later forms in `begin`
        #'(begin
            (step-top-interaction #f e)
            ...
            (step-top-interaction final? e0))]
       #; ;; is this needed?
       [(define-values ids rhs)
        ;; don't force expansion, because the rhs might refer to the definition
        (cond
          [(syntax-e #'final?)
           #`(begin #,pre-b  (finish-top-interaction))]
          [else pre-b])]
       [_
        (define b (local-expand pre-b 'top-level null))        
        (cond
          [(syntax-e #'final?)
           (syntax-parse b
             #:literals (#%expression)
             [(#%expression e)
              (finish_current_frame)     
              (displayln (string-append "- " (interaction_type_string #'e)))
              b]
             [else      
              #`(begin
                  #,b
                  (finish-top-interaction))])]
          [else
           b])])]))

(define-syntax (finish-top-interaction stx)
  (finish_current_frame)
  #'(void))
