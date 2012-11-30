(use-modules (cairo))

(define *pi* 3.1415926535897931)

(define *image-operations* (make-hash-table))

(define (register-image-operation name f)
  (hash-set! *image-operations* name f))

(register-image-operation 'move
                          (lambda (cr x y)
                            (display "move!\n")
                            (cairo-move-to cr x y)))

(register-image-operation 'rectangle
                          (lambda (cr x y w h)
                            (display "rectangle!\n")
                            (cairo-rectangle cr x y w h)))

(register-image-operation 'arc
                          (lambda (cr x y r a1 a2)
                            (display "arc!\n")
                            (cairo-arc cr x y r a1 a2)))

(register-image-operation 'circle
                          (lambda (cr x y r)
                            (display "arc!\n")
                            (cairo-arc cr x y r 0.0 (* 2 *pi*))))

(register-image-operation 'color
                          (lambda (cr r g b a)
                            (display "color!\n")
                            (cairo-set-source-rgba cr r g b a)))

(register-image-operation 'line
                          (lambda (cr x y)
                            (display "line!\n")
                            (cairo-line-to cr x y)))

(register-image-operation 'fill
                          (lambda (cr)
                            (display "fill!\n")
                            (cairo-fill cr)))

(register-image-operation 'stroke
                          (lambda (cr)
                            (display "stroke!\n")
                            (cairo-stroke cr)))

(register-image-operation 'scale
                          (lambda (cr x y)
                            (display "scale!\n")
                            (cairo-scale cr x y)))

(register-image-operation 'rotate
                          (lambda (cr a)
                            (display "rotate!\n")
                            (cairo-rotate cr a)))

(register-image-operation 'translate
                          (lambda (cr x y)
                            (display "translate!\n")
                            (cairo-translate cr x y)))

(register-image-operation 'reset
                          (lambda (cr)
                            (display "reset!\n")
                            (cairo-identity-matrix cr)))

(register-image-operation 'font-size
                          (lambda (cr s)
                            (display "font-size!\n")
                            (cairo-set-font-size cr s)))

(register-image-operation 'line-cap
                          (lambda (cr s)
                            (display "line-cap!\n")
                            (cairo-set-line-cap cr s)))

(register-image-operation 'line-join
                          (lambda (cr s)
                            (display "line-join!\n")
                            (cairo-set-line-join cr s)))

(register-image-operation 'line-width
                          (lambda (cr s)
                            (display "line-width!\n")
                            (cairo-set-line-width cr s)))

(define (make-matrix-stack)
  (let ((mats '()))
    (lambda (cr msg)
      (cond
       ((eq? msg 'push) 
        (set! mats (cons (cairo-get-matrix cr) mats)))
       ((eq? msg 'pop)
        (if (null? mats)
            (cairo-identity-matrix cr)
            (begin
              (cairo-set-matrix cr (car mats))
              (set! mats (cdr mats)))))
       (else
        (error (format #f "don't understand how to ~a" msg)))))))

(define *matrix-stack* (make-matrix-stack))

(register-image-operation 'pop
                          (lambda (cr)
                            (display "pop!\n")
                            (*matrix-stack* cr 'pop)))

(register-image-operation 'push
                          (lambda (cr)
                            (display "push!\n")
                            (*matrix-stack* cr 'push)))

(define (run-image-steps cr steps)
  (for-each (lambda (step)
              (let ((f (hash-ref *image-operations* (car step))))
                (if f
                    (apply f (cons cr (cdr step)))
                    (error (format #f "invalid operation: ~a" (car step))))))
            steps))

(define-syntax define-image
  (syntax-rules (steps)
    ((_ filename (width height) (steps step ...))
     (let ((cr (create width height)))
       (run-image-steps cr '(step ...))
       (save-png cr filename)))))

(define (create w h)
  (let ((surf (cairo-image-surface-create 'argb32 w h)))
    (cairo-create surf)))

(define (save-png cr filename)
  (with-output-to-file filename
    (lambda ()
      (cairo-surface-write-to-png (cairo-get-target cr)))))

(define (draw-char cr ch x y)
  (cairo-move-to cr x y)
  (cairo-show-text cr (make-string 1 ch)))

(register-image-operation 'char draw-char)

(define (draw-string cr str x y)
  (cairo-move-to cr x y)
  (cairo-show-text cr str))

(register-image-operation 'string draw-string)

(define (draw-poly cr . pts)
  (display pts) (newline)
  (if (> (length pts) 2)
      (begin
        (cairo-move-to cr (caar pts) (cadar pts))
        (for-each (lambda (pt)
                    (cairo-line-to cr (car pt) (cadr pt)))
                  pts))
      (error "polygons have at least 3 points")))

(register-image-operation 'poly draw-poly)



;; (define-image "/tmp/test.png" (500 500)
;;   (steps
;;    (move 0 0)
;;    (rectangle 0 0 500 500)
;;    (color 0.2 0.2 0.2 1.0)
;;    (fill)
;;    (color .9 0.0 0.0 .8)
;;    (poly (10 10) (400 19) (300 400) (12 390))
;;    (fill)))

;; (define-image "/tmp/file.png" (500 500)
;;   (steps
;;    (move 0 0)
;;    (rectangle 0 0 500 500)
;;    (color 0.2 0.2 0.2 1.0)
;;    (fill)
;;    (move 0 0)
;;    (translate 250 250)
;;    (rotate 180)
;;    (color 0.9 0.0 0.0 0.7)
;;    (circle -66 33 100)
;;    (fill)
;;    (color 0.0 0.9 0.0 0.7)
;;    (circle -33 133 100)
;;    (fill)
;;    (color 0.0 0.0 0.9 0.7)
;;    (circle 66 133 100)
;;    (fill)))

;; (define-image "/tmp/push.png" (500 500)
;;   (steps
;;    (move 0 0)
;;    (rectangle 0 0 500 500)
;;    (color 0.2 0.2 0.2 1.0)
;;    (fill)

;;    (push)

;;    (translate 250 250)
;;    (color 0.9 0.0 0.0 0.6)
;;    (rectangle 0 0 100 100)
;;    (fill)

;;    (push)

;;    (rotate 5)
;;    (color 0.9 0.9 0.0 0.6)
;;    (rectangle 0 0 100 100)
;;    (fill)

;;    (push)

;;    (rotate 5)
;;    (color 0.0 0.9 0.9 0.6)
;;    (rectangle 0 0 100 100)
;;    (fill)

;;    (push)

;;    (rotate 5)
;;    (color 0.0 0.7 0.0 0.6)
;;    (rectangle 0 0 100 100)
;;    (fill)

;;    (push)

;;    (rotate 5)
;;    (color 0.7 0.0 0.7 0.6)
;;    (rectangle 0 0 100 100)
;;    (fill)))
