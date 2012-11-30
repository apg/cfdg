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

(define (cfdg-compile rules env renv)
  ;; need to define this
)

;; (rule circles
;;   (four-circles)
;;   (SQUARE))

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
