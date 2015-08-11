#lang racket/base

(provide (all-defined-out))

(define (clamp val #:max [max #f] #:min [min #f])
  (if (and max (> val max))
      max
      (if (and min (< val min))
          min
          val)))
