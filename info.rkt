#lang setup/infotab

(define collection "shplait")

(define deps '("base"
               ("rhombus-prototype" #:version "0.27")))

(define build-deps '("racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))

(define version "0.7")

(define scribblings '(("scribblings/shplait.scrbl" (multi-page) (language))))
