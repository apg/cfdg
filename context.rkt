#lang racket/base

(provide (all-defined-out))

(require racket/math
         "./utils.rkt")

(struct context
  (x y alpha brightness color saturation width height rotation size nesting)
  #:transparent)

(define (context-update old ps)
  (define params (apply hasheq ps))
  (define old-x (context-x old))
  (define old-y (context-y old))
  (define old-alpha (context-alpha old))
  (define old-color (context-color old))
  (define old-brightness (context-brightness old))
  (define old-saturation (context-saturation old))
  (define old-rotation (context-rotation old))
  (define old-size (context-size old))
  (define new-alpha (clamp (let ([na (hash-ref params 'a 0)])
                             (if (> na 0)
                                 (+ old-alpha (* na (- 255 old-alpha)))
                                 (+ old-alpha (* na old-alpha))))
                           #:min 0 #:max 255))
  (define new-brightness (clamp (let ([nb (hash-ref params 'b 0)])
                                  (if (> nb 0)
                                      (+ old-brightness (* nb (- 255 old-brightness)))
                                      (+ old-brightness (* nb old-brightness))))
                                #:min 0 #:max 255))
  (set! new-brightness 255)
  (define new-color (clamp (+ old-color (hash-ref params 'c 0))
                           #:min 0 #:max 255))
  (define tmp-r-radians (degrees->radians (hash-ref params 'r 0)))
  (define new-rotation  (+ old-rotation tmp-r-radians))
  (define new-size (clamp (* old-size (hash-ref params 's 1))
                          #:min 0))
  (define new-saturation (clamp (+ old-saturation (hash-ref params 't 0))
                                #:min 0 #:max 255))
  (define tmp-x (hash-ref params 'x 0))
  (define tmp-y (hash-ref params 'y 0))

  (define new-x (+ old-x
                   (* new-size
                      (- (* tmp-x (cos new-rotation))
                         (* tmp-y (sin new-rotation))))))
  (define new-y (+ old-y
                   (* new-size
                      (+ (* tmp-y (cos new-rotation))
                         (* tmp-x (sin new-rotation))))))
  (struct-copy context old
               [x new-x]
               [y new-y]
               [alpha new-alpha]
               [brightness new-brightness]
               [color new-color]
               [saturation new-saturation]
               [rotation new-rotation]
               [size new-size]))
