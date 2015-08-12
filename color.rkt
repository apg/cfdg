#lang racket/base

(require racket/draw
         "./context.rkt"
         "./utils.rkt")

(provide (all-defined-out))

(define (hsl->rgb h s l)
  (define (hue->rgb p q t)
    (let ([t (cond
              [(> t 1) (sub1 t)]
              [(< t 0) (add1 t)]
              [else t])])
      (cond
       [(< t 1/6) (+ p (* t 6 (- q p)))]
       [(< t 1/2) q]
       [(< t 2/3) (+ p (* (- 2/3 t) 6) (- q p))]
       [else p])))
  (define q (if (< l 0.5)
                (* l (add1 s))
                (+ l (- s (* l s)))))
  (define p (* 2 (- l q)))
  (define r (hue->rgb p q (+ h 1/3)))
  (define g (hue->rgb p q h))
  (define b (hue->rgb p q (- h 1/3)))
  (apply values (map (lambda (x)
                       (clamp (inexact->exact (round (* 255 x)))
                              #:max 255 #:min 0))
                     (list r g b))))

(define (rgb->hsl r g b)
  (define r* (/ r 255))
  (define g* (/ g 255))
  (define b* (/ b 255))
  (define mx (max r* g* b*))
  (define mn (min r* g* b*))
  (define l (/ (+ mx mn) 2))
  (cond
   [(= mn mx) (values 0 0 l)]
   [else
    (let* [(d (- mx mn))
           (s (if (> l .5) (/ d (- 2 mx mn)) (/ d (+ mx mn))))
           (h (cond
                [(= mx r*) (+ (/ (- g* b*) d) (if (< g* b*) 6 0))]
                [(= mx g*) (+ (/ (- b* r*) d) 2 )]
                [(= mx b*) (+ (/ (- r* g*) d) 4)]))]
      (values (/ h 6) s l))]))

(define (compute-color ctx)
  (define-values [r g b] (hsl->rgb (/ (clamp (context-hue ctx) #:max 360 #:min 0) 360)
                                   (/ (clamp (context-saturation ctx) #:max 100 #:min 0) 100)
                                   (/ (clamp (context-luminance ctx) #:max 100 #:min 0) 100)))
  (make-color r g b (/ (context-alpha ctx) 100)))
