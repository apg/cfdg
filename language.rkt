#lang racket/base

(require (for-syntax racket/base))

(require racket/draw
         racket/math
         racket/class)

(require "./color.rkt"
         "./context.rkt")

(provide circle
         line
         square
         rect
         rule
         start-rule
         (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [cfdg-module-begin #%module-begin]))

(define (circle . params)
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

(define (line . params)
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
  (define endpt (compute-point (context-x ctx*) (context-y ctx*)))

  (send dc set-brush (compute-color ctx*) 'transparent)
  (send dc set-smoothing 'aligned)
  (send dc set-pen (compute-color ctx*) 1 'solid)
  (send dc draw-line (context-x ctx*) (context-y ctx*) (car endpt) (cdr endpt))

  ctx*)

(define (square . params)
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

(define (rect . params)
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
  (send dc set-pen (compute-color ctx*) 1 'solid)
  (send dc set-brush (compute-color ctx*) 'transparent)
  (send dc set-smoothing 'aligned)
  (send dc draw-polygon (map compute-point '(-1 1 1 -1) '(-1 -1 1 1)))
  ctx*)

(struct rule-set (name total rules) #:mutable)


(define-syntax-rule (start-rule rule params ...)
  (let ([target (make-bitmap 500 500)])
    (parameterize ([current-dc (new bitmap-dc% [bitmap target])]
                   [current-context (context 250 250 1 0 255 255 500 500 0 50 4)])
      (begin
        (rule params ...)
        (send target save-file "out.png" 'png)))))

;;; rule selection.
(define-syntax (rule stx)
  (syntax-case stx ()
    [(_ (name weight) forms ...)
     #'(begin
         (define (name . params)
           (define existing (or (hash-ref (current-rule-set) 'name #f) (error 'rule-not-found)))
           (define max-weight (rule-set-total existing))
           (define val (* max-weight (random)))
           (let loop [(rules (rule-set-rules existing))]
             (cond
              [(null? rules) (error 'empty-rules)]
              [(null? (cdr rules))
               (apply (cdar rules) params)]
              [(> val (caar rules))
               (apply (cdar rules) params)]
              [else (loop (cdr rules))])))
         (let* [(existing (hash-ref (current-rule-set) 'name #f))
                (func (lambda params
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
                         [else ctx*])))]
           (if existing
               (let [(new-weight (+ (rule-set-total existing) weight))
                     (new-rules (cons (cons weight func) (rule-set-rules existing)))]
                 (set-rule-set-total! existing new-weight)
                 (set-rule-set-rules! existing new-rules))
               (begin
                (hash-set! (current-rule-set) 'name (rule-set 'name weight (list (cons weight func))))))))]
    [(_ name forms ...) #'(rule (name 1.0) forms ...)]))


(define current-dc (make-parameter 'empty-dc))
(define current-context (make-parameter 'empty-context))
(define current-rule-set (make-parameter (make-hasheq)))

(define-syntax-rule (cfdg-module-begin body ...)
  (#%plain-module-begin
   (parameterize ([current-rule-set (make-hasheq)])
     (begin body ...)
     )))
