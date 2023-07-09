#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (prefix-in rhombus: rhombus)
         (for-syntax
          (prefix-in rhombus: rhombus/parse))
         (prefix-in rhombus: rhombus/parse)
         rhombus/private/bounce
         "private/frame.rhm")

(provide (rename-out
          [shplait-module-begin #%module-begin]
          [shplait-top-interaction #%top-interaction]))
(bounce "private/main.rhm")

(module reader syntax/module-reader
  #:language 'shplait
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
    [(_ (top form ...))
     (find_type_definitions #'(block form ...))
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
                        (map syntax-local-introduce (get_submodules))))])]))

(define-syntax (shplait-top-interaction stx)
  (syntax-parse stx
    #:datum-literals (top)
    [(_ . (top form ...))
     (define b
       (local-expand #'(rhombus:#%top-interaction
                        . (top form ...))
                     'top-level
                     null))
     (finish_current_frame)
     (let loop ([b b])
       (syntax-parse b
         #:literals (#%expression begin)
         [(#%expression e) (displayln (string-append "- " (interaction_type_string #'e)))]
         [(begin b ...) (for-each loop (syntax->list #'(b ...)))]
         [_ (void)]))
     b]))
