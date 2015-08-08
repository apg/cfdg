#lang racket/base

(require (for-syntax racket/base))

(require racket/draw
         racket/match
         racket/math
         racket/class)

;;; color related stuff

(define (clamp val #:max [max #f] #:min [min #f])
  (if (and max (> val max))
      max
      (if (and min (< val min))
          min
          val)))

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
  (define-values [r g b] (hsl->rgb (/ (context-color ctx) 255)
                                   (/ (context-saturation ctx) 255)
                                   (/ (context-brightness ctx) 255)))
  (make-color r g b (context-alpha ctx)))

;;; context
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


;;; primitives


(define (circle params)
  (define dc (current-dc))
  (define ctx (current-context))
  (define ctx* (context-update ctx params))
  (define new-size (context-size ctx*))
  (define half-size (/ new-size 2))
  (define top-left-x (- (context-x ctx*) half-size))
  (define top-left-y (- (context-y ctx*) half-size))
  (send dc set-brush (compute-color ctx*) 'solid)
  (send dc set-smoothing 'aligned)
  (send dc set-pen "black" 0 'transparent)
                                        ;  (send dc draw-ellipse top-left-x top-left-y half-size half-size)
  (send dc draw-ellipse top-left-x top-left-y new-size new-size)
  ctx*)

(define (square params)
  (define dc (current-dc))
  (define ctx (current-context))
  (define old-x (context-x ctx))
  (define old-y (context-y ctx))
  (define ctx* (context-update ctx params))
  (define new-rot (context-rotation ctx*))
  (define new-size (context-size ctx*))
  (define (compute-point x y)
    (cons (+ old-x
             (* new-size
                (- (* x (cos new-rot))
                   (* y (sin new-rot)))))
          (+ old-y
             (* new-size
                (+ (* y (cos new-rot))
                   (* x (sin new-rot)))))))
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush (compute-color ctx*) 'solid)
  (send dc set-smoothing 'aligned)
  (send dc draw-polygon (map compute-point '(-1 1 1 -1) '(-1 -1 1 1)))
  ctx*)

;; (struct sketch (start-rule rule-groups) #:mutable)



;; (define-syntax (rule stx)
;;   (syntax-case stx ()
;;     [(rule foo bar) #'(list foo bar)]))

(define current-dc (make-parameter 'empty-dc))
(define current-context (make-parameter 'empty-context))

;;; rule selection.
(define-syntax (rule stx)
  (syntax-case stx ()
    [(_ (name weight) forms ...)
     #'(define (name params)
         (define ctx (current-context))
         (define current-nesting (context-nesting ctx))
         (define ctx* (struct-copy context
                                   (context-update ctx params)
                                   [nesting (sub1 current-nesting)]))
         (cond
          [(and (> current-nesting 0)
                (> (context-size ctx*) 0))
           (parameterize ([current-context ctx*])
             (begin forms ...
                    ctx*))]
          [else ctx*]))]
    [(_ name forms ...) #'(rule (name 1.0) forms ...)]))





;;; Rule.
(rule circles
      (square '())
      (four-circles '()))

(rule four-circles
      (circle '(x 10.5 s 0.7))
      (circle '(x -10.5 s 0.7) )
      (circle '(y 10.5 s 0.7) )
      (circle '(y -10.5 s 0.7))
      (four-circles '(x 10.5 s 0.5))
      (four-circles '(x -10.5 s 0.5))
      (four-circles '(y 10.5 s 0.5))
      (four-circles '(y -10.5 s 0.5)))


(define context-start (context 250 250 1 0 0 0 500 500 0 50 4))
(define target (make-bitmap 500 500))
(define dc (new bitmap-dc% [bitmap target]))
