#lang racket

;; #1
;; Checking intersection of two segments

(define (make-equation point1 point2)
  ;; points are lists '(x y)
  (let* ((x0 (car point1))   (y0 (cadr point1))
         (x1 (car point2))   (y1 (cadr point2))
         (a (- y1 y0))
         (b (- x0 x1))
         (c (+ (* a x0) (* b y0))))
    (list a b c)))

(define (linesX line1 line2)
  ; whether lines are parallel or intersected
  (let* ((a0 (first  line1)) (a1 (first  line2))
         (b0 (second line1)) (b1 (second line2)     
         (det (- (* a0 b1) (* a1 b0))))
      (cond [(zero? det) #f] 
            [else  (cons (/ (- (* b1 c0) (* b0 c1)) det)
                     (/ (- (* a0 c1) (* a1 c0)) det))])))

(define (make-rectangle point1 point2)
  (let* ((x0 (car point1)) (y0 (cadr point1))
         (x1 (car point2)) (y1 (cadr point2)))   
 (list (min x0 x1) (max x0 x1) (min y0 y1) (max y0 y1))))


(define (point-in-rectangle? point rect)
    (let* ((x (car point))
           (y (cdr point))
           (xmin (first rect))
           (xmax (second rect))
           (ymin (third rect))
           (ymax (fourth rect)))
  (and (<= xmin x xmax)
       (<= ymin y ymax))))

;main
(define (intersects? p0 p1 q0 q1)
  (define p (linesX (make-equation p0 p1) 
                    (make-equation q0 q1)))
  (if (false? p) #f
    (and (point-in-rectangle? p (make-rectangle p0 p1))
         (point-in-rectangle? p (make-rectangle q0 q1)))))


;; #2
;; Checking a self-intersection of a polyline

(define (self-intersects? polyline)
  
  (define (list-of-segments lst)
  (define (iter lst res)
    (if (null? (rest lst)) (reverse res)
        (iter (rest lst) 
              (cons (list (first lst) (second lst)) res))))
  (iter lst null))

  
  (define (iter lst)
    (if (null? (drop lst 2)) #f
        (let ((current (first lst)))
          (if (false? (ormap (λ (x) (intersects? current x)) (drop lst 2)))
              (iter (rest lst))
              #t))))
  
  (iter (list-of-segments polyline)))


;; #3
;; Coordinates of a regular polygon with {n} vertices and {radius}

(define (regular-polygon n radius)
  (for/list ((i (in-range 0 n)))
    (define angle (* 2 pi (/ i n)))
    (list (* radius (cos angle))
          (* radius (sin angle)))))

(define (regular-polygon-approx n radius)
  ; for small n
  (for/list ((i (in-range 0 n)))
    (define angle (* 2 pi (/ i n)))
    (list (inexact->exact (round (* radius (cos angle))))
          (inexact->exact (round (* radius (sin angle)))))))
