#lang racket/base

;; `rhombus/scribble` needs to provide support for implementing this
;; in Rhombus

(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/property)
         rhombus/parse
         (only-in rhombus/scribble/manual
                  rhombusblock_etc)
         rhombus/scribble/private/typeset-doc)

(provide (for-space rhombus/doc
                    type))

(define-for-syntax (extract-typeset stx space-name subst)
  (syntax-parse stx
    #:datum-literals (group op $ parens)
    [(group _ (quotes (group (~and dollar (op $)) arg (op id) e ...)))
     (doc-typeset-rhombusblock #:at stx
                               #:pattern? #t
                               #`(group dollar arg #,@(subst #'id) e ...))]
    [(group _ (quotes (group (~and pns (parens . _)) (op id) e ...)))
     (doc-typeset-rhombusblock #:at stx
                               #:pattern? #t
                               #`(group pns #,@(subst #'id) e ...))]
    [(group _ (quotes (~and g (group (~and o (op id)) e ...))))
     (doc-typeset-rhombusblock #:at #'g
                               #:pattern? #t
                               #`(group #,@(subst #'id) e ...))]
    [(group _ (quotes (group id e ...)))
     (doc-typeset-rhombusblock #:at stx
                               #:pattern? #t
                               #`(group #,@(subst #'id) e ...))]))

(define-doc-syntax type
  (make-doc-transformer #:extract-desc (lambda (stx) "type")
                        #:extract-space-sym (lambda (stx) 'shplait/type)
                        #:extract-name (lambda (stx space-sym)
                                         (syntax-parse stx
                                           #:datum-literals (group quotes op $ parens)
                                           [(group _ (quotes (group (op $) arg (op id) e ...)))
                                            ((make-interned-syntax-introducer 'shplait/type) #'id 'add)]
                                           [(group _ (quotes (group (parens . _) (op id) e ...)))
                                            ((make-interned-syntax-introducer 'shplait/type) #'id 'add)]
                                           [(group _ (quotes (group (op id) e ...)))
                                            ((make-interned-syntax-introducer 'shplait/type) #'id 'add)]
                                           [(group _ (quotes (group id _ ...)))
                                            ((make-interned-syntax-introducer 'shplait/type) #'id 'add)]))
                        #:extract-metavariables (lambda (stx space-name vars)
                                                  (syntax-parse stx
                                                    #:datum-literals (group op quotes)
                                                    [(group _ (quotes g))
                                                     (extract-pattern-metavariables #'g vars)]
                                                    [_
                                                     vars]))
                        #:extract-typeset extract-typeset))
