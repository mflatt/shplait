#lang racket/base
(require rhombus/private/bounce)

(bounce "private/core.rkt"
        "private/lib.rhm")

(module reader syntax/module-reader
  #:language 'shplait
  #:read (lambda (in) (list (syntax->datum (parse-all in #:variant variant))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src #:variant variant)))
  #:info (lambda (key default make-default)
           (case key
             [(documentation-language-family) "Shplait"]
             [else
              (get-info-proc key default make-default
                             #:variant variant
                             #:semantic-type-guess semantic-type-guess)]))
  #:whole-body-readers? #t
  (require shrubbery/parse
           (only-in (submod rhombus reader) get-info-proc)
           shrubbery/variant
           racket/promise)
  (define variant (make-variant #:allow-operator? (lambda (str)
                                                    (cond
                                                      [(equal? (string-ref str 0) #\|)
                                                       ;; disallowing operators that start with `|`
                                                       ;; prevents confusion ;; when a space is omitted
                                                       ;; after `|`, especially in in `| ~else:`, but
                                                       ;; `||` needs to be allowed
                                                       (equal? str "||")]
                                                      [else #t]))
                                #:indented-operator-continue? (lambda (str)
                                                                ;; disallowing line continuation for `$` or `...`
                                                                ;; prevents confusion in syntax patterns and
                                                                ;; templates, where it looks like `$` or `...`
                                                                ;; is at the start of a new group, but it
                                                                ;; actually continues the previous group
                                                                (not (or (equal? str "$")
                                                                         (equal? str "..."))))))
  (define semantic-type-guess
    (let ([classify (delay (dynamic-require '(lib "shplait/private/syntax_color.rhm") 'classify))])
      (lambda (str default)
        ((force classify) str)))))

(module configure-runtime racket/base
  (require rhombus/runtime-config))

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))
