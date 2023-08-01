#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     rhombus/syntax)
         (prefix-in rhombus: rhombus)
         (for-syntax
          (prefix-in rhombus: rhombus/parse))
         (prefix-in rhombus: rhombus/parse)
         rhombus/private/bounce
         "frame.rhm"
         "configure.rhm"
         "local-lift-meta.rkt")

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
     #:with (config (_ form ...)) (parse_and_apply_configure (syntax->list #'(form-in ...)))
     #`(rhombus:#%module-begin
        (top
         ;; relies on first `begin-for-syntax` happening before any other expansion:
         #,(s-exp->decl-group #`(begin-for-syntax (find_type_definitions #'(block form ...))))
         form ...
         ;; relies on last expression being expanded after everything else is expanded:
         #,(s-exp->expr-group #`(finish-module))))]))

(define-syntax (finish-module stx)
  ;; Perform let-based polymoprhim inference, now that
  ;; all definitions and uses are expanded:
  (finish_current_frame)
  ;; Add an expand-time call to the end
  ;; of the module that registers a mapping of gensyms
  ;; (which are quoted in static info) to rebuilt type objects:
  (syntax-local-lift-meta-expression
   #`(rhombus:rhombus-expression #,(extract-group (build_register_defn_types))))
  #'(void))

(define-syntax (shplait-top-interaction stx)
  (syntax-parse stx
    #:datum-literals (top)
    [(_ . (top form ...))
     (configure_from_saved)
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
        ;; trampoline, so earlier definitions in `begin` are
        ;; available to local-expansion of later forms in `begin`
        #'(begin
            (step-top-interaction #f e)
            ...
            (step-top-interaction final? e0))]
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
