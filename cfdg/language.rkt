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
         set
         rule
         start-rule
         (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [cfdg-module-begin #%module-begin]))

(define (set-pen-brush-properties!
         color
         #:brush [brush 'transparent]
         #:pen [pen 'transparent]
         #:pen-size [pen-size 0])
  (send (current-dc) set-smoothing 'aligned)
  (send (current-dc) set-brush color brush)
  (send (current-dc) set-pen color pen-size pen))

;;; x0, y0 represents the old point.
(define (compute-point x0 y0 x1 y1 size rot)
  (cons (+ x0
           (* size
              (- (* x1 (cos rot))
                 (* y1 (sin rot)))))
        (+ y0
           (* size
              (+ (* y1 (cos rot))
                 (* x1 (sin rot)))))))


;;; standard primitives

(define (circle . params)
  (define ctx* (context-update (current-context) params))
  (define new-size (context-size ctx*))
  (define half-size (/ new-size 2))
  (set-pen-brush-properties! (compute-color ctx*) #:brush 'solid)
  (send (current-dc)
        draw-ellipse
        (- (context-x ctx*) half-size)
        (- (context-y ctx*) half-size)
        new-size
        new-size)
  ctx*)

(define (line . params)
  (define ctx* (context-update (current-context) params))
  (define endpt (compute-point (context-x (current-context))
                               (context-y (current-context))
                               (context-x ctx*)
                               (context-y ctx*)
                               (context-size ctx*)
                               (context-rotation ctx*)))
  (set-pen-brush-properties! (compute-color ctx* #:pen 'solid #:pen-size 1))
  (send (current-dc) draw-line (context-x ctx*) (context-y ctx*) (car endpt) (cdr endpt))
  ctx*)


(define ((make-quad #:brush [brush 'transparent]
                    #:pen [pen 'transparent]
                    #:pen-size [pen-size 0]) . params)
  (define ctx* (context-update (current-context) params))
  (set-pen-brush-properties! (compute-color ctx*) #:brush brush #:pen pen #:pen-size pen-size)
  (send (current-dc)
        draw-polygon
        (let ([x0 (context-x (current-context))]
              [y0 (context-y (current-context))]
              [new-rot (context-rotation ctx*)]
              [new-size (context-size ctx*)])
          (map (lambda (x y)
                 (compute-point x0 y0 x y new-size new-rot))
               '(-1 1 1 -1) '(-1 -1 1 1)))))

(define square (make-quad #:brush 'solid))
(define rect (make-quad #:pen 'solid #:pen-size 1))

;;; rule set
(struct rule-set (name total rules) #:mutable)

(define-syntax-rule (set name value)
  (hash-set! (current-init-params) (context-name-mapping 'name) value))

(define-syntax-rule (start-rule rule params ...)
  (let ([background (hash-ref (current-init-params) 'background *default-background*)]
        [target (make-bitmap
                 (hash-ref (current-init-params) 'width *default-width*)
                 (hash-ref (current-init-params) 'height *default-height*))])
    (parameterize ([current-dc (new bitmap-dc% [bitmap target])]
                   [current-context (initial-context (current-init-params))])
      (begin
        (send (current-dc) set-background background)
        (send (current-dc) clear)
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


;;; parameters for important things.
(define current-dc (make-parameter 'empty-dc))
(define current-context (make-parameter 'empty-context))
(define current-rule-set (make-parameter (make-hasheq)))
(define current-init-params (make-parameter (make-hasheq)))

(define-syntax-rule (cfdg-module-begin body ...)
  (#%plain-module-begin
   (parameterize ([current-rule-set (make-hasheq)]
                  [current-init-params (make-hasheq)])
     (begin body ...))))
