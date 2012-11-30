(use-modules (cairo)
             (srfi srfi-1))

(define (hsl->rgb h s l)
  (define (rgb m . rgb)
    (map (lambda (x)
           (floor (* 255 (+ m x))))
         rgb))
  (let* ((H (round (* (/ h 255) 360)))
         (S (/ s 255))
         (L (/ l 255))
         (C (* S (- 1 (abs (- (* 2 L) 1)))))
         (H' (floor (/ H 60)))
         (X (* C (- 1 (abs (- (remainder H' 2) 1)))))
         (m (- L (* (/ 1 2) C))))
    (cond
     ((and (>= H' 0) (< H' 1)) (rgb m C X 0))
     ((< H' 1) (rgb m C X 0))
     ((< H' 2) (rgb m X C 0))
     ((< H' 3) (rgb m 0 C X))
     ((< H' 4) (rgb m 0 X C))
     ((< H' 5) (rgb m X 0 C))
     ((< H' 6) (rgb m C 0 X))
     (else (list 0 0 0)))))

(define (make-env)
  '())

(define (env-bind env binding val)
  (cons (cons binding val) env))

(define (env-get env binding)
  (let ((found (assq binding env)))
    (if (null? found)
        found
        (cdr found))))

(define (env-initialize env)
  (fold (lambda (x env)
          (env-bind env (car x) (cadr x)))
        env
        '((start-rule '())
          (a 0)
          (b 0)
          (c 0)
          (h 500)
          (n 200)
          (r 0)
          (s 10)
          (t 0)
          (w 500)
          (x 0)
          (y 0))))

(define (rule-env-initialize env)
  (env-set! env 'CIRCLE (cons 'primitive cfdg-circle))
  (env-set! env 'SQUARE (cons 'primitive cfdg-square))
  (env-set! env 'RECT (cons 'primitive cfdg-rect))
  (env-set! env 'LINE (cons 'primitive cfdg-line)))

(define (rule-env-set! name rule . prob)
  (let ((existing (env-get *rule-env* name))
        (prob (if (null? prob) 1 (car prob))))
    (if (null? existing)
        (set! *rule-env* 
              (cons `(user . ((,prob . ,rule))) *rule-env*)
        (set-cdr! existing 
                  (cons `(,prob . ,rule) (cdr existing))))))

(define (rule-env-get name)
  (define (choose-one xs)
    (let* ((flip (random:uniform))
           (choice (let loop ((sp 0)
                              (left xs))
                     (if (null? left)
                         '()
                         (if (>= (+ sp (caar left)) flip)
                             (cdar left)
                             (loop (+ sp (caar left)) (cdr left)))))))
      (if (and (null? choice) (> (length xs) 0))
          (cdar xs)
          choice)))
  (let ((existing (env-get *rule-env* name)))
    (if (null? existing)
        (error (format #f "name-error: ~a" name))
        (if (= (car existing) 'user)
            (choose-one (cdr existing))
            (cdr existing))))

(define *rule-env* (initialize-rule-env (make-env)))

;;; clamps v between n and x (inclusive)
(define (clamp v n x)
  (cond
   ((> v x) x)
   ((< v n) n)
   (else v)))

;;; Wraps calls to operators in push and pop operations
;;; and inserts surface into the first positional argument
;;; of the operations to be called
(define-syntax call-with-current-surface
  (syntax-rules ()
    ((_ surf (op args ...) ...)
     (begin 
       (push surf)
       (apply op (cons surf '(args ...))) ...
       (pop)))))

;;; Ensures colors and rotations are setup correctly
(define-syntax with-current-env
  (syntax-rules ()
    ((_ (surf env) body ...)
     (let ((b (cadr (assoc 'b env)))
           (c (cadr (assoc 'c env)))
           (t (cadr (assoc 't env)))
           (a (cadr (assoc 'a env)))
           (r (cadr (assoc 'r env))))
       ;;; (cairo-rotate surf r) x, y get changed on set of x and y
       (apply cairo-set-source-rgba (list surf ,@(hsl->rgb c t b) a))))))

;;; Draws a filled-circle on surf given current env params
(define (cfdg-circle surf env)
  (with-current-env (surf env)
     (let ((x (cadr (assoc 'x env)))
           (y (cadr (assoc 'y env)))
           (s (cadr (assoc 's env))))
       (cairo-fill surf)
       (cairo-arc surf x y (/ s 2) 0.0 (* 2 *pi*))))

;;; Draws a filled-square on surf given current env params
(define (cfdg-square surf env)
  (with-current-env (surf env)
     (let ((x (cadr (assoc 'x env)))
           (y (cadr (assoc 'y env)))
           (s (cadr (assoc 's env))))
       (cairo-fill surf)
       (cairo-rect surf x y s s))))

;;; Draws an open-square on surf given current env params
(define (cfdg-rect surf env)
  (with-current-env (surf env)
     (let ((x (cadr (assoc 'x env)))
           (y (cadr (assoc 'y env)))
           (s (cadr (assoc 's env))))
       (cairo-stroke surf)
       (cairo-rect surf x y s s))))

;;; Draws a line on surface given current params
(define (cfdg-line surf env)
  (with-current-env env
    (let ((x (cadr (assoc 'x env)))
          (y (cadr (assoc 'y env)))
          (s (cadr (assoc 's env))))
      (cairo-line-to surf (+ x s) (+ y s)))))


; X (x-coord) 	0 to image width 	depends on scaling 	any 	old_x + s*(x*cos(r)-y*sin(r)) 	Horisontal shift.
; Y (y-coord) 	0 to image height 	depends on scaling 	any 	old_y + s*(y*cos(r)+x*sin(r)) 	Vertical shift.
(define *cfdg-parameter-adjustments*
  (list
   ('a (lambda (old-a a)
         (clamp (if (> a 0)
                    (+ old-a (* a (- 255 old-a)))
                    (+ old-a (* a old-a)))) 0 255))
   ('b (lambda (old-b b)
         (clamp (if (> b 0)
                    (+ old-b (* b (- 255 old-b)))
                    (+ old-b (* b old-b)))) 0 255))
   ('c (lambda (old-c c)
         (clamp (+ old-c c) 0 255)))
   ('r (lambda (old-r r)
         (+ old-r r)))
   ('s (lambda (old-s s)
         (let ((ns (* old-s s)))
           (if (<= 0 ns)
               0.0001
               ns))))
   ('t (lambda (old-t t)
         (clamp (+ old-t t) 0 255)))
   ('x (lambda (old-x x s r width)
         (clamp 
          (+ old-x (* s (+ (* x (cos r)) (* y (sin r)))))
          0 width)))
   ('y (lambda (old-y y s r height)
         (clamp
          (+ old-y (* s (+ (* y (cos r)) (* x (sin r)))))
          0 height)))))

       
;;; Extend and update the environment parameters
(define (cfdg-extend-env env . params)
  (let ((cx (assoc 'x env))
        (cy (assoc 'y env))
        (cw (assoc 'width env))
        (ch (assoc 'height env))
        (cr (assoc 'r env))
        (cs (assoc 's env)))
    (fold (lambda (p env)
            (let* ((old (assoc (car p) env))
                   (fun (assoc (car p) *cfdg-parameter-adjustments*)))
              ; TODO replace!
              (cond
               ((eq? 'x (car p))
                (cons (list (car p) (apply (map cadr (list cx p cs cr cw))) '()) env))
               ((eq? 'y (car p))
                (cons (list (car p) (apply (map cadr (list cy p cs cr ch))) '()) env))
               (else
                (cons (list (car p) (apply (map cadr (list old p)))) env))))))))
          
;;; I want this to create a new function `four-circles`
;;; which runs the sketch with CFDG semantics
;; (define-sketch four-circles
;;   (w 1000)
;;   (h 1000)
;;   (rule circles
;;         (four-circles)
;;         (square))
;;   (rule four-circles
;;         (CIRCLE (x 1.5) (s 0.7))
;;         (CIRCLE (x -1.5) (s 0.7))
;;         (CIRCLE (y 1.5) (s 0.7))
;;         (CIRCLE (y -1.5) (s 0.7))
;;         (four-circles (x 1.5 s 0.5))
;;         (four-circles (x -1.5 s 0.5))
;;         (four-circles (y 1.5 s 0.5))
;;         (four-circles (y -1.5 s 0.5)))

;;   (start-rule 'circles))

;;; This is likely output
;; (define (four-circles)
;;   (define (rule-circles surf env n)
;;     (if (not (= 0 n))
;;        (begin
;;           (call-with-current-surface surf (rule-four-circles env (- n 1)) 
;;           (call-with-current-surface surf (square))))
;;   (define (rule-four-circles surf env n)
;;     (cfdg-circle surf (extend-env env '(x 1.5) '(s 0.7)))
;;     (cfdg-circle surf (extend-env env '(x -1.5) '(s 0.7)))
;;     (cfdg-circle surf (extend-env env '(y 1.5) '(s 0.7)))
;;     (cfdg-circle surf (extend-env env '(y -1.5) '(s 0.7)))
;;     (rule-four-circles surf (extend-env env '(x 1.5) '(s 0.5)))
;;     (rule-four-circles surf (extend-env env '(x -1.5) '(s 0.5)))
;;     (rule-four-circles surf (extend-env env '(y 1.5) '(s 0.5)))
;;     (rule-four-circles surf (extend-env env '(y -1.5) '(s 0.5))))

;;   (let ((cr (create 500 500))
;;         (env (make-env)))
;;     (rule-circles cr env)))

  

;; (rule circles
;;   (four-circles)
;;   (square))
;;
;; (rule four-circles
;;   (CIRCLE (x 1.5) (s 0.7))
;;   (CIRCLE (x -1.5) (s 0.7))
;;   (CIRCLE (y 1.5) (s 0.7))
;;   (CIRCLE (y -1.5) (s 0.7))
;;   (four-circles (x 1.5 s 0.5))
;;   (four-circles (x -1.5 s 0.5))
;;   (four-circles (y 1.5 s 0.5))
;;   (four-circles (y -1.5 s 0.5)))

;; (start-rule 'circles)

;; (width 400)
;; (height 400)
;; (s 30)
;; (b 220)
;; (r 0)
;; (c 0)
;; (t 200)
;; (a 0)
;; (n 70)

;; (start-rule 'FIRST)

;; (rule FIRST
;;       (BRICK)
;;       (BRICK (r 72) (c 51))
;;       (BRICK (r 144) (c 102))
;;       (BRICK (r 216) (c 153))
;;       (BRICK (r 288) (c 204))

;;       (CIRCLE (s 45) (t 0) (b 200)))

;; (rule BRICK 
;;       (SQUARE)
;;       (BRICK (r -20) (x 1.3) (s 0.9) (b -0.1)))

;; (rule BRICK .2
;;       (SQUARE)
;;       (BRICK (r -15) (x 1.3) (s 0.9)))
