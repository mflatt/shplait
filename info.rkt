#lang setup/infotab

(define collection "shplait")

(define deps '("base"
               ("rhombus-prototype" #:version "0.2")))

(define build-deps '("racket-doc"))

(define version "0.1")

(define scribblings '(("scribblings/shplait.scrbl" (multi-page) (language))))
