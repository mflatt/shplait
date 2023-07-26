#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     rhombus/private/runtime-config
                     version/utils)
         (prefix-in rhombus: rhombus)
         (for-syntax
          (prefix-in rhombus: rhombus/parse))
         (prefix-in rhombus: rhombus/parse)
         rhombus/private/bounce
         "frame.rhm"
         "configure.rhm")

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
     (find_type_definitions #'(block form ...))
     (when (version<? (version) "8.10.0.3")
       ;; workaround for `module` not setting expand config during `#%module-begin`
       (install-runtime-config!))
     (define b
       (local-expand #'(rhombus:#%module-begin
                        (top
                         form ...))
                     'module-begin
                     null))
     (finish_current_frame)
     (syntax-parse b
       [(#%mb e ...)
        #`(#%mb e ...
                (begin-for-syntax
                  (rhombus:rhombus-expression #,(syntax-parse (build_register_defn_types)
                                                  [(parsed kw (re g)) #'g])))
                #,@(map (lambda (sm)
                          #`(rhombus:rhombus-top #,(syntax-parse sm
                                                     #:datum-literals (multi)
                                                     [(multi g) #'g]
                                                     [_ sm])))
                        (map syntax-local-introduce
                             (get_configured_submodules #'config))))])]))

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
        ;; trampoline, so earlier definitions ni `begin` are
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
