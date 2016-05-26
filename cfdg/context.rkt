#lang racket/base

(provide (all-defined-out))

(require racket/math
         "./utils.rkt")

(struct context
  (x y alpha hue saturation luminance width height rotation size nesting)
  #:transparent)

(define *default-background* "white")
(define *default-height* 1000)
(define *default-width* 1000)
(define *default-alpha* 100)
(define *default-hue* 0)
(define *default-saturation* 0)
(define *default-luminance* 0)
(define *default-rotation* 0)
(define *default-size* 10)
(define *default-nesting* 10)

(define (context-name-mapping n)
  (case n
    [(a) 'alpha]
    [(c) 'hue]
    [(t) 'saturation]
    [(b) 'luminance]
    [(l) 'luminance]
    [(r) 'rotation]
    [(s) 'size]
    [(n) 'nesting]
    [(w) 'width]
    [(h) 'height]
    [else n]))

(define (initial-context params)
  (define init-width (hash-ref params 'width *default-width*))
  (define init-height (hash-ref params 'width *default-width*))
  (define init-x (hash-ref params 'x (/ init-width 2)))
  (define init-y (hash-ref params 'x (/ init-height 2)))
  (context init-x
           init-y
           (hash-ref params 'alpha *default-alpha*)
           (hash-ref params 'hue *default-hue*)
           (hash-ref params 'saturation *default-saturation*)
           (hash-ref params 'luminance *default-luminance*)
           init-width
           init-height
           (hash-ref params 'rotation *default-rotation*)
           (hash-ref params 'size *default-size*)
           (hash-ref params 'nesting *default-nesting*)))

(define (context-update old ps)
  (define params (apply hasheq ps))
  (define old-x (context-x old))
  (define old-y (context-y old))
  (define old-alpha (context-alpha old))
  (define old-hue (context-hue old))
  (define old-luminance (context-luminance old))
  (define old-saturation (context-saturation old))
  (define old-rotation (context-rotation old))
  (define old-size (context-size old))
  (define new-alpha (clamp (let ([na (hash-ref params 'a 0)])
                             (if (> na 0)
                                 (+ old-alpha (* na (- 100 old-alpha)))
                                 (+ old-alpha (* na old-alpha))))
                           #:min 0 #:max 100))
  ;; luminance in HSL
  (define new-luminance (clamp (let ([nb (hash-ref params 'b 0)])
                                 (if (> nb 0)
                                     (+ old-luminance (* nb (- 100 old-luminance)))
                                     (+ old-luminance (* nb old-luminance))))
                               #:min 0 #:max 100))
  (define new-hue (clamp (+ old-hue (hash-ref params 'c 0))
                           #:min 0 #:max 360))
  (define tmp-r-radians (degrees->radians (hash-ref params 'r 0)))
  (define new-rotation  (+ old-rotation tmp-r-radians))
  (define new-size (clamp (* old-size (hash-ref params 's 1))
                          #:min 0))
  (define new-saturation (clamp (+ old-saturation (hash-ref params 't 0))
                                #:min 0 #:max 100))
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
               [luminance new-luminance]
               [hue new-hue]
               [saturation new-saturation]
               [rotation new-rotation]
               [size new-size]))
