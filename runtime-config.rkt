#lang racket/base
(require rhombus/runtime-config
         (only-in "private/lazy.rhm"
                  dynamic_force_all))

;; Force lazy structure for printing from an immediate module body or
;; for a REPL. This is really only necessary for lists, since
;; everything else automatically forces for printing (but lists print
;; as pairs with `cons` when the `rest` is not a pre-forced list).
(current-print (let ([orig (current-print)])
                 (lambda (v)
                   (orig (dynamic_force_all v)))))
