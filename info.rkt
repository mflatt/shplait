#lang setup/infotab

(define collection "shplait")

(define deps '("base"
               ("rhombus-prototype" #:version "0.17")))

(define build-deps '("racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))

(define version "0.2")

(define scribblings '(("scribblings/shplait.scrbl" (multi-page) (language))))
