#lang info
(define collection 'multi)
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/cfdg.scrbl" ())))
(define pkg-desc "CFDG: Simple implementation of Chris Coyne's original Context Free Data Grammars")
(define version "0.0")
(define pkg-authors '(apg))
