#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (prefix-in rhombus: rhombus)
         rhombus/private/bounce
         "private/typecheck.rhm")

(provide (rename-out
          [shplait-module-begin #%module-begin]))
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
     (define b
       (local-expand #`(rhombus:#%module-begin
                        (top
                         form ...))
                     'module-begin
                     null))
     (typecheck_do)
     b]))

