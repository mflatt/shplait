#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     rhombus/expand-config ; for workaround
                     version/utils ; for workaround
                     rhombus/syntax)
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
     (define call-as-expand
       (cond
         [(version<? (version) "8.10.0.3")
          ;; workaround for `module` not setting expand config during `#%module-begin`
          (lambda (thunk)
            (call-with-parameterization
             (enter-parameterization)
             thunk))]
         [else
          (lambda (thunk) (thunk))]))
     (define b
       (call-as-expand
        (lambda ()
          (find_type_definitions #'(block form ...))
          (begin0
            (local-expand #'(rhombus:#%module-begin
                             (top
                              (group rhombus:module #:early configure_runtime #:lang rhombus
                                     (block
                                      (group import shplait/runtime_config)))
                              form ...))
                          'module-begin
                          null)
            (finish_current_frame)))))
     (syntax-parse b
       [(#%mb e ...)
        #`(#%mb e ...
                (begin-for-syntax
                  (rhombus:rhombus-expression #,(extract-group (build_register_defn_types))))
                #,@(map (lambda (sm)
                          #`(rhombus:rhombus-top #,(extract-group sm)))
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
