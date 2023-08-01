#lang racket/base
(require (for-syntax racket/base))

;; Maybe Racket should provide a function like this,
;; but until it does, use a submodule.

(provide
 (for-syntax syntax-local-lift-meta-expression))

(define-for-syntax (syntax-local-lift-meta-expression e)
  ;; First, create a submodule to hold the expand-time code:
  (syntax-local-lift-module
   #`(module lifted-meta-expression racket/base
       (require (only-in shplait/private/core))
       (begin-for-syntax #,e)))
  ;; Then, lift a require of the submodule (delayed until after the
  ;; submodule has been declared):
  (syntax-local-lift-expression
   #'(let-syntax ([_ (syntax-local-lift-require
                      '(submod "." lifted-meta-expression)
                      (quote-syntax #f))])
       (void))))
