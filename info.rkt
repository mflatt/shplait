#lang setup/infotab

(define collection "shplait")

(define deps '("base"
               "shrubbery-lib"
               ["rhombus-lib" #:version "0.41"]))

(define build-deps '("racket-doc"
                     "sandbox-lib"
                     "scribble-lib"
                     ["rhombus-scribble-lib" #:version "0.2"]
                     "rhombus"
                     "shrubbery"))

(define version "0.14")

(define scribblings '(("scribblings/shplait.scrbl" (multi-page) (language))))
